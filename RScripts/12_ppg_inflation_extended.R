# ==============================================================
# 12_ppg_inflation_extended.R
# Four extensions on top of the master dataset produced by
# 04_merge_standings_and_market_values.R:
#   1. Points_Per_Game outcome + Model H refit
#   2. Season-level transfer-market inflation adjustment
#   3. Linear-vs-quadratic (Model A vs Model H) side-by-side justification
#   4. Five-way extended model comparison table
#
# Outputs (all saved to outputs/):
#   41_points_per_game_scatter.png
#   41b_ppg_model_coefficients.csv               (bonus — Task 1 coefficients)
#   42_transfer_market_inflation_index.png
#   42b_financial_rank_adjustment_check.csv       (bonus — Task 2 rank check)
#   43_inflation_adjusted_scatter.png
#   44_linear_vs_quadratic_comparison.png
#   45_extended_model_comparison.csv
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr", "ggplot2", "broom", "scales", "purrr", "stringr"))

ensure_installed <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing,
                     repos = "https://packagemanager.posit.co/cran/latest",
                     quiet = TRUE)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}
ensure_installed(c("ggrepel", "patchwork"))

make_dir("outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 10.5, colour = "grey45", margin = margin(b = 12)),
    plot.caption      = element_text(size = 8.5, colour = "grey55", hjust = 0, margin = margin(t = 10)),
    axis.title        = element_text(size = 10.5, colour = "grey30"),
    axis.text         = element_text(size = 9.5,  colour = "grey40"),
    panel.grid.major  = element_line(colour = "grey92", linewidth = 0.5),
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    legend.title      = element_text(size = 9.5, face = "bold"),
    legend.text       = element_text(size = 9),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = margin(16, 16, 12, 16)
  )

league_colours <- c(
  "Bundesliga"     = "#E41A1C",
  "La Liga"        = "#FF7F00",
  "Ligue 1"        = "#4DAF4A",
  "Premier League" = "#984EA3",
  "Serie A"        = "#377EB8"
)

season_order <- paste0(2015:2024, "-", 2016:2025)
BIG4_LEAGUES <- c("Premier League", "La Liga", "Serie A", "Ligue 1")


# ══════════════════════════════════════════════════════════════
# PHASE 0: Load master dataset
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 0: Load Master Dataset ===\n")

master_path <- "data/merged/master_dataset_corrected.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset_corrected.csv — run extensions/scripts/15_spal_correction.R first.")

master <- read_csv(master_path, show_col_types = FALSE) |>
  mutate(League = as.character(League))

cat(sprintf("  Rows: %d  |  Leagues: %s\n", nrow(master),
            paste(sort(unique(master$League)), collapse = ", ")))
cat("PHASE 0 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 1 — Points Per Game
#   →  41_points_per_game_scatter.png
#   →  41b_ppg_model_coefficients.csv
# ══════════════════════════════════════════════════════════════
cat("=== TASK 1: Points Per Game ===\n")

master <- master |>
  mutate(
    Matches_Played = case_when(
      League %in% BIG4_LEAGUES ~ 38,
      League == "Bundesliga"   ~ 34,
      TRUE                     ~ NA_real_
    ),
    Points_Per_Game = Points / Matches_Played
  )

# Diagnostic: flag league-seasons where actual W+D+L differs from the
# assumed schedule length (e.g. Ligue 1's COVID-shortened 2019-20 season
# and its move to an 18-team, 34-match format from 2023-24 onward).
# Points_Per_Game is still computed with the assumed 38/34 divisor per the
# task specification — this is flagged for transparency, not corrected.
schedule_check <- master |>
  mutate(Actual_Matches = W + D + L) |>
  filter(Actual_Matches != Matches_Played) |>
  distinct(League, Season, Actual_Matches, Matches_Played)

if (nrow(schedule_check) > 0) {
  cat("  NOTE: league-seasons where actual matches != assumed schedule length:\n")
  print(as.data.frame(schedule_check))
  cat("  Points_Per_Game below still uses the assumed 38/34 divisor (per spec).\n")
} else {
  cat("  No league-seasons deviate from the assumed 38/34 match schedule.\n")
}

model_df_ppg <- master |>
  filter(!is.na(Financial_Rank), !is.na(Points_Per_Game))
cat(sprintf("  Complete-case rows: %d\n", nrow(model_df_ppg)))

modH_ppg <- lm(Points_Per_Game ~ Financial_Rank + I(Financial_Rank^2), data = model_df_ppg)
sumH_ppg <- summary(modH_ppg)

cat(sprintf("  Points_Per_Game ~ FR + FR^2:  R2 = %.4f  |  Adj-R2 = %.4f  |  AIC = %.2f\n",
            sumH_ppg$r.squared, sumH_ppg$adj.r.squared, AIC(modH_ppg)))
cat(sprintf("    Slope (FR)    = %.5f\n", coef(modH_ppg)[["Financial_Rank"]]))
cat(sprintf("    Slope (FR^2)  = %.5f\n", coef(modH_ppg)[["I(Financial_Rank^2)"]]))

coefs_ppg_tbl <- tidy(modH_ppg, conf.int = TRUE) |>
  mutate(response = "Points_Per_Game", .before = 1)

path41b <- "outputs/41b_ppg_model_coefficients.csv"
write_csv(coefs_ppg_tbl, path41b)
track(path41b)
cat(sprintf("  Saved: %s\n", path41b))

# ── Plot 41: FR vs Points_Per_Game scatter with quadratic fit ──
cat("  Generating plot 41: Points Per Game scatter\n")

aug_ppg <- augment(modH_ppg, data = model_df_ppg)

fr_seq_ppg <- seq(1, max(model_df_ppg$Financial_Rank, na.rm = TRUE), length.out = 300)
quad_pred_ppg <- predict(modH_ppg, newdata = data.frame(Financial_Rank = fr_seq_ppg),
                         interval = "confidence", level = 0.95) |>
  as.data.frame() |>
  mutate(Financial_Rank = fr_seq_ppg)

label_df_ppg <- aug_ppg |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024") |
    (grepl("Lille",      Team) & Season == "2020-2021")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

p41 <- ggplot() +
  geom_ribbon(data = quad_pred_ppg, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill = "#2C3E73", alpha = 0.12) +
  geom_jitter(data = aug_ppg, aes(x = Financial_Rank, y = Points_Per_Game, colour = League),
              alpha = 0.35, size = 1.3, width = 0.18, height = 0.03) +
  geom_line(data = quad_pred_ppg, aes(x = Financial_Rank, y = fit),
            colour = "#2C3E73", linewidth = 1.4) +
  geom_point(data = label_df_ppg, aes(x = Financial_Rank, y = Points_Per_Game),
             colour = "#F39C12", fill = "#F39C12", shape = 23, size = 5, stroke = 0.8) +
  ggrepel::geom_label_repel(
    data = label_df_ppg,
    aes(x = Financial_Rank, y = Points_Per_Game, label = Label),
    fill = "white", colour = "#2C3E50", size = 3.1, fontface = "bold",
    label.size = 0.3, label.r = unit(0.2, "lines"),
    box.padding = 0.6, point.padding = 0.4,
    segment.colour = "#F39C12", segment.size = 0.55,
    min.segment.length = 0.2, max.overlaps = 20
  ) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), labels = function(x) paste0("FR ", x)) +
  labs(
    title    = "Richer Squads Earn More Points Per Game (Model H, PPG outcome)",
    subtitle = sprintf(
      "N = %d team-seasons  •  R2 = %.3f  •  Points_Per_Game ~ FR + FR^2  •  Gold = key story clubs",
      nrow(aug_ppg), sumH_ppg$r.squared
    ),
    x       = "Financial Rank within league-season (FR 1 = richest squad)",
    y       = "Points per game that season",
    colour  = "League",
    caption = "Points_Per_Game = Points / Matches_Played (38 for PL/La Liga/Serie A/Ligue 1, 34 for Bundesliga)."
  ) +
  theme_clean

path41 <- "outputs/41_points_per_game_scatter.png"
ggsave(path41, p41, width = 12, height = 7.5, dpi = 150)
track(path41)
cat(sprintf("  Saved: %s\n", path41))
cat("TASK 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 2 — Transfer-Market Inflation Adjustment
#   →  42_transfer_market_inflation_index.png
#   →  42b_financial_rank_adjustment_check.csv
#   →  43_inflation_adjusted_scatter.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 2: Transfer-Market Inflation Adjustment ===\n")

season_avg <- master |>
  filter(!is.na(Market_Value_M_EUR)) |>
  group_by(Season) |>
  summarise(Avg_Market_Value_M_EUR = mean(Market_Value_M_EUR, na.rm = TRUE), .groups = "drop") |>
  mutate(Season = factor(Season, levels = season_order)) |>
  arrange(Season)

base_avg <- season_avg$Avg_Market_Value_M_EUR[season_avg$Season == "2019-2020"]
cat(sprintf("  Base year 2019-20 average squad value: €%.1fM (index = 1.000)\n", base_avg))

season_avg <- season_avg |>
  mutate(Inflation_Index = round(Avg_Market_Value_M_EUR / base_avg, 4))

cat("\n  Inflation index by season (2019-20 = 1.0):\n")
print(as.data.frame(season_avg))

master <- master |>
  left_join(season_avg |> mutate(Season = as.character(Season)) |> select(Season, Inflation_Index),
            by = "Season") |>
  mutate(Adjusted_Market_Value_M_EUR = Market_Value_M_EUR / Inflation_Index)

master <- master |>
  group_by(League, Season) |>
  mutate(Financial_Rank_Adjusted = ifelse(
    is.na(Adjusted_Market_Value_M_EUR), NA_real_,
    rank(-Adjusted_Market_Value_M_EUR, ties.method = "min")
  )) |>
  ungroup()

# League-seasons with an unmatched team (NA Market_Value_M_EUR from the
# standings/market-value merge, e.g. SPAL 2017-18 Serie A) have one fewer
# real observation than Financial_Rank was originally computed over. Any
# rank shift arising purely from that pre-existing gap is a merge artifact,
# not an effect of inflation adjustment — flag it so the two are not conflated.
incomplete_groups <- master |>
  filter(is.na(Market_Value_M_EUR)) |>
  distinct(League, Season) |>
  mutate(Confounded_By_Missing_Data = TRUE)

rank_check <- master |>
  filter(!is.na(Financial_Rank), !is.na(Financial_Rank_Adjusted)) |>
  mutate(Rank_Change = Financial_Rank_Adjusted - Financial_Rank) |>
  left_join(incomplete_groups, by = c("League", "Season")) |>
  mutate(Confounded_By_Missing_Data = coalesce(Confounded_By_Missing_Data, FALSE)) |>
  select(League, Season, Team, Market_Value_M_EUR, Financial_Rank,
         Adjusted_Market_Value_M_EUR, Financial_Rank_Adjusted, Rank_Change,
         Confounded_By_Missing_Data) |>
  arrange(desc(abs(Rank_Change)))

n_changed            <- sum(rank_check$Rank_Change != 0)
n_changed_confounded <- sum(rank_check$Rank_Change != 0 & rank_check$Confounded_By_Missing_Data)
n_changed_genuine    <- n_changed - n_changed_confounded

cat(sprintf("\n  Teams whose Financial_Rank changed after inflation adjustment: %d / %d\n",
            n_changed, nrow(rank_check)))
cat("  Explanation: the inflation index is computed once per SEASON across all five\n")
cat("  leagues combined, so within any single League+Season group every team's market\n")
cat("  value is divided by the SAME constant. Dividing a group of numbers by a common\n")
cat("  positive constant cannot change their relative order, so Financial_Rank is\n")
cat("  mathematically guaranteed to be invariant to this adjustment — it only matters\n")
cat("  for comparing raw market value levels across seasons, not for within-season rank.\n")
if (n_changed > 0) {
  cat(sprintf("  Of the %d apparent changes, %d occur in a league-season with an unmatched\n",
              n_changed, n_changed_confounded))
  cat("  team (missing Market_Value_M_EUR from the merge, e.g. SPAL 2017-18 Serie A) —\n")
  cat("  re-ranking only the remaining teams compacts the numbering by one slot below the\n")
  cat("  gap. This is a pre-existing data-completeness artifact, not an inflation effect.\n")
  cat(sprintf("  Genuine inflation-driven rank changes: %d\n", n_changed_genuine))
}

path42b <- "outputs/42b_financial_rank_adjustment_check.csv"
write_csv(rank_check, path42b)
track(path42b)
cat(sprintf("  Saved: %s\n", path42b))

# ── Plot 42: Inflation index line chart ────────────────────────
cat("\n  Generating plot 42: Inflation index line chart\n")

p42 <- ggplot(season_avg, aes(x = Season, y = Inflation_Index, group = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.6) +
  geom_line(colour = "#2C3E73", linewidth = 1.3) +
  geom_point(colour = "#2C3E73", size = 2.8) +
  geom_text(aes(label = sprintf("%.2f", Inflation_Index)),
            vjust = -1.1, size = 3.2, colour = "#2C3E73") +
  annotate("text", x = "2019-2020", y = 1.0, label = "Base year (2019-20 = 1.00)",
           vjust = 1.8, hjust = 0.35, size = 3.2, colour = "grey40", fontface = "italic") +
  scale_y_continuous(labels = number_format(accuracy = 0.01), expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title    = "Transfer Market Inflation Index, 2015-16 to 2024-25",
    subtitle = "Average squad market value across all 5 leagues each season, indexed to 2019-20 = 1.00",
    x        = "Season",
    y        = "Inflation Index (2019-20 = 1.00)"
  ) +
  theme_clean +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

path42 <- "outputs/42_transfer_market_inflation_index.png"
ggsave(path42, p42, width = 10, height = 6.5, dpi = 150)
track(path42)
cat(sprintf("  Saved: %s\n", path42))

# ── Plot 43: Inflation-adjusted market value vs Points_Per_Game ─
cat("  Generating plot 43: Inflation-adjusted scatter\n")

model_df_infl_scatter <- master |>
  filter(!is.na(Adjusted_Market_Value_M_EUR), !is.na(Points_Per_Game))

label_df_infl <- model_df_infl_scatter |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024") |
    (grepl("Lille",      Team) & Season == "2020-2021")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

p43 <- ggplot(model_df_infl_scatter, aes(x = Adjusted_Market_Value_M_EUR, y = Points_Per_Game)) +
  geom_point(aes(colour = League), alpha = 0.4, size = 1.8) +
  geom_smooth(method = "lm", colour = "grey20", linewidth = 1, se = TRUE) +
  geom_point(data = label_df_infl, aes(x = Adjusted_Market_Value_M_EUR, y = Points_Per_Game),
             colour = "#F39C12", fill = "#F39C12", shape = 23, size = 5, stroke = 0.8) +
  ggrepel::geom_label_repel(
    data = label_df_infl,
    aes(x = Adjusted_Market_Value_M_EUR, y = Points_Per_Game, label = Label),
    fill = "white", colour = "#2C3E50", size = 3.1, fontface = "bold",
    label.size = 0.3, label.r = unit(0.2, "lines"),
    box.padding = 0.6, point.padding = 0.4,
    segment.colour = "#F39C12", segment.size = 0.55,
    min.segment.length = 0.2, max.overlaps = 20
  ) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(labels = comma_format()) +
  labs(
    title    = "Inflation-Adjusted Squad Value vs Points Per Game",
    subtitle = sprintf("N = %d team-seasons  •  Market value expressed in 2019-20-equivalent euros",
                       nrow(model_df_infl_scatter)),
    x        = "Inflation-Adjusted Market Value (€M, 2019-20 equivalent)",
    y        = "Points per game that season",
    colour   = "League"
  ) +
  theme_clean

path43 <- "outputs/43_inflation_adjusted_scatter.png"
ggsave(path43, p43, width = 12, height = 7.5, dpi = 150)
track(path43)
cat(sprintf("  Saved: %s\n", path43))
cat("TASK 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 3 — FR^2 Justification: Model A vs Model H
#   →  44_linear_vs_quadratic_comparison.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 3: Linear vs Quadratic Comparison ===\n")

model_df_pos <- master |>
  filter(!is.na(Financial_Rank), !is.na(League_Position))
cat(sprintf("  Complete-case rows: %d\n", nrow(model_df_pos)))

modA <- lm(League_Position ~ Financial_Rank, data = model_df_pos)
modH <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df_pos)
sumA <- summary(modA)
sumH <- summary(modH)

cat(sprintf("  Model A (linear):    R2 = %.4f  |  AIC = %.2f\n", sumA$r.squared, AIC(modA)))
cat(sprintf("  Model H (quadratic): R2 = %.4f  |  AIC = %.2f\n", sumH$r.squared, AIC(modH)))

fr_range  <- range(model_df_pos$Financial_Rank, na.rm = TRUE)
pos_range <- range(model_df_pos$League_Position, na.rm = TRUE)
fr_seq    <- seq(fr_range[1], fr_range[2], length.out = 300)

# Small buffer beyond the data range so jittered points at the boundary
# (Financial_Rank/League_Position = 1 or 20) aren't clipped by the shared
# axis limits below — both panels get the same buffered range.
fr_lim  <- fr_range  + c(-0.6, 0.6)
pos_lim <- pos_range + c(-0.6, 0.6)

pred_A <- predict(modA, newdata = data.frame(Financial_Rank = fr_seq),
                  interval = "confidence", level = 0.95) |>
  as.data.frame() |> mutate(Financial_Rank = fr_seq)
pred_H <- predict(modH, newdata = data.frame(Financial_Rank = fr_seq),
                  interval = "confidence", level = 0.95) |>
  as.data.frame() |> mutate(Financial_Rank = fr_seq)

label_y <- pos_range[1] + 1

p_left <- ggplot() +
  geom_jitter(data = model_df_pos, aes(x = Financial_Rank, y = League_Position),
              colour = "grey45", alpha = 0.25, size = 1.1, width = 0.25, height = 0.25) +
  geom_ribbon(data = pred_A, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill = "#2C3E73", alpha = 0.18) +
  geom_line(data = pred_A, aes(x = Financial_Rank, y = fit),
            colour = "#2C3E73", linewidth = 1.3) +
  annotate("label", x = fr_range[1] + 0.62 * diff(fr_range), y = label_y,
           label = sprintf("R² = %.3f", sumA$r.squared),
           colour = "#2C3E73", fill = "white", size = 4, fontface = "bold", hjust = 0) +
  scale_x_continuous(limits = fr_lim, breaks = c(1, 5, 10, 15, 20)) +
  scale_y_reverse(limits = rev(pos_lim), breaks = c(1, 5, 10, 15, 20)) +
  labs(
    title    = "Model A — Straight Line",
    subtitle = "League_Position ~ Financial_Rank",
    x        = "Financial Rank (1 = richest)",
    y        = "Final league position (1 = champion)"
  ) +
  theme_clean

p_right <- ggplot() +
  geom_jitter(data = model_df_pos, aes(x = Financial_Rank, y = League_Position),
              colour = "grey45", alpha = 0.25, size = 1.1, width = 0.25, height = 0.25) +
  geom_ribbon(data = pred_H, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill = "#2C3E73", alpha = 0.18) +
  geom_line(data = pred_H, aes(x = Financial_Rank, y = fit),
            colour = "#2C3E73", linewidth = 1.3) +
  annotate("label", x = fr_range[1] + 0.62 * diff(fr_range), y = label_y,
           label = sprintf("R² = %.3f", sumH$r.squared),
           colour = "#2C3E73", fill = "white", size = 4, fontface = "bold", hjust = 0) +
  scale_x_continuous(limits = fr_lim, breaks = c(1, 5, 10, 15, 20)) +
  scale_y_reverse(limits = rev(pos_lim), breaks = c(1, 5, 10, 15, 20)) +
  labs(
    title    = "Model H — Quadratic",
    subtitle = "League_Position ~ Financial_Rank + Financial_Rank²",
    x        = "Financial Rank (1 = richest)",
    y        = "Final league position (1 = champion)"
  ) +
  theme_clean

p_combined <- p_left + p_right +
  patchwork::plot_annotation(
    title    = "Why the Curve Fits Better Than the Straight Line",
    subtitle = sprintf("Same %d team-seasons, same axes, same fit method — only the functional form differs",
                       nrow(model_df_pos)),
    caption  = "Ribbons = 95% confidence band. Identical data, colour scheme, and axis scales in both panels.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(size = 11, colour = "grey40", hjust = 0, margin = margin(b = 8)),
      plot.caption  = element_text(size = 8.5, colour = "grey55", hjust = 0, margin = margin(t = 8))
    )
  )

path44 <- "outputs/44_linear_vs_quadratic_comparison.png"
ggsave(path44, p_combined, width = 13, height = 6.5, dpi = 150)
track(path44)
cat(sprintf("  Saved: %s\n", path44))
cat("TASK 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 4 — Extended Model Comparison Table
#   →  45_extended_model_comparison.csv
# ══════════════════════════════════════════════════════════════
cat("=== TASK 4: Extended Model Comparison Table ===\n")

model_df_points <- master |>
  filter(!is.na(Financial_Rank), !is.na(Points))
modH_points <- lm(Points ~ Financial_Rank + I(Financial_Rank^2), data = model_df_points)
sumH_points <- summary(modH_points)

model_df_infl <- master |>
  filter(!is.na(Financial_Rank_Adjusted), !is.na(League_Position))
modH_infl <- lm(League_Position ~ Financial_Rank_Adjusted + I(Financial_Rank_Adjusted^2),
                data = model_df_infl)
sumH_infl <- summary(modH_infl)

model_row <- function(model_name, model, s, outcome, formula_str, n, fr_term, fr2_term, notes) {
  tibble(
    Model          = model_name,
    Outcome        = outcome,
    Formula        = formula_str,
    N              = n,
    R_squared      = round(s$r.squared, 4),
    Adj_R_squared  = round(s$adj.r.squared, 4),
    AIC            = round(AIC(model), 2),
    FR_linear_coef = round(unname(coef(model)[fr_term]), 5),
    FR_squared_coef = if (is.na(fr2_term)) NA_real_ else round(unname(coef(model)[fr2_term]), 5),
    Notes          = notes
  )
}

extended_comparison <- bind_rows(
  model_row("A: Linear (position)", modA, sumA, "League_Position",
            "Pos ~ FR", nrow(model_df_pos), "Financial_Rank", NA_character_,
            "Baseline straight-line fit"),
  model_row("H: Quadratic (position)", modH, sumH, "League_Position",
            "Pos ~ FR + FR^2", nrow(model_df_pos), "Financial_Rank", "I(Financial_Rank^2)",
            "Preferred model — Task 3 comparison"),
  model_row("Points model (quadratic)", modH_points, sumH_points, "Points",
            "Points ~ FR + FR^2", nrow(model_df_points), "Financial_Rank", "I(Financial_Rank^2)",
            "Higher Points = better; slope flips sign vs position models"),
  model_row("PPG model (quadratic)", modH_ppg, sumH_ppg, "Points_Per_Game",
            "PPG ~ FR + FR^2", nrow(model_df_ppg), "Financial_Rank", "I(Financial_Rank^2)",
            "Task 1 model"),
  model_row("Inflation-adjusted FR model (quadratic)", modH_infl, sumH_infl, "League_Position",
            "Pos ~ FR_adj + FR_adj^2", nrow(model_df_infl),
            "Financial_Rank_Adjusted", "I(Financial_Rank_Adjusted^2)",
            sprintf(paste0(
              "0/%d genuine inflation-driven rank changes (mathematically invariant — same ",
              "season-level divisor within each league-season); %d apparent shifts are a ",
              "pre-existing missing-data artifact (Serie A 2017-18) unrelated to inflation -> ",
              "stats equal Model H by construction"),
              nrow(rank_check), n_changed_confounded))
)

path45 <- "outputs/45_extended_model_comparison.csv"
write_csv(extended_comparison, path45)
track(path45)
cat(sprintf("  Saved: %s\n", path45))
cat("\n=== Extended Model Comparison (Task 4) ===\n")
print(as.data.frame(extended_comparison))
cat("TASK 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Output Inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s):\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n12_ppg_inflation_extended.R complete.\n")
