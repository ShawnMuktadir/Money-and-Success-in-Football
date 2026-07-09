# ==============================================================
# 13_model_evolution_comparison.R
# Traces the full model evolution from the simplest linear baseline
# (Model A) through to the professor-suggested Points-Per-Game
# quadratic specification (Model H / PPG), and validates that the
# transfer-market inflation adjustment leaves Financial_Rank intact.
#
# Uses data/merged/master_dataset_corrected.csv (SPAL 2017-18 Serie A
# market value fix from extensions/scripts/15_spal_correction.R) —
# run script 15 first if that file doesn't exist yet.
#
#   Task 1 — Three-way (four-row) model evolution comparison table
#   Task 2 — Four-panel visual comparison (A -> H position -> H points -> H PPG)
#   Task 3 — Inflation validation chart (index line + rank scatter)
#   Task 4 — Coefficient evolution dot-and-whisker plot
#   Task 5 — Clean console summary + data-quality warnings
#
# Outputs (all saved to extensions/outputs/ — existing outputs/ and
# RScripts/ folders are untouched):
#   E1_model_evolution_table.csv
#   E2_model_evolution_4panel.png
#   E3_inflation_validation.png
#   E4_coefficient_evolution.png
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

make_dir("extensions/outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13, margin = margin(b = 3)),
    plot.subtitle     = element_text(size = 9.5, colour = "grey45", margin = margin(b = 8)),
    plot.caption      = element_text(size = 8.5, colour = "grey55", hjust = 0, margin = margin(t = 8)),
    axis.title        = element_text(size = 10, colour = "grey30"),
    axis.text         = element_text(size = 9, colour = "grey40"),
    panel.grid.major  = element_line(colour = "grey92", linewidth = 0.5),
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    legend.title      = element_text(size = 9.5, face = "bold"),
    legend.text       = element_text(size = 9),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = margin(12, 14, 10, 14)
  )

league_colours <- c(
  "Bundesliga"     = "#E41A1C",
  "La Liga"        = "#FF7F00",
  "Ligue 1"        = "#4DAF4A",
  "Premier League" = "#984EA3",
  "Serie A"        = "#377EB8"
)

season_order  <- paste0(2015:2024, "-", 2016:2025)
BIG4_LEAGUES  <- c("Premier League", "La Liga", "Serie A", "Ligue 1")


# ══════════════════════════════════════════════════════════════
# PHASE 0: Load master dataset & derive Points_Per_Game
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 0: Load Master Dataset ===\n")

master_path <- "data/merged/master_dataset_corrected.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset_corrected.csv — run extensions/scripts/15_spal_correction.R first.")

master <- read_csv(master_path, show_col_types = FALSE) |>
  mutate(League = as.character(League))

master <- master |>
  mutate(
    Matches_Played  = case_when(
      League %in% BIG4_LEAGUES ~ 38,
      League == "Bundesliga"   ~ 34,
      TRUE                     ~ NA_real_
    ),
    Points_Per_Game = Points / Matches_Played
  )

# Flag league-seasons where actual W+D+L differs from the assumed
# schedule length (COVID-shortened Ligue 1 2019-20 is the headline
# case) — Points_Per_Game still uses the fixed 38/34 divisor per spec,
# this is surfaced later in the Task 5 warnings section.
schedule_check <- master |>
  mutate(Actual_Matches = W + D + L) |>
  filter(Actual_Matches != Matches_Played) |>
  distinct(League, Season, Actual_Matches, Matches_Played)

cat(sprintf("  Rows: %d  |  Leagues: %s\n", nrow(master),
            paste(sort(unique(master$League)), collapse = ", ")))
cat("PHASE 0 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 1 — Three/four-way model evolution comparison table
#   →  E1_model_evolution_table.csv
# ══════════════════════════════════════════════════════════════
cat("=== TASK 1: Model Evolution Table ===\n")

model_df_pos <- master |>
  filter(!is.na(Financial_Rank), !is.na(League_Position))
model_df_points <- master |>
  filter(!is.na(Financial_Rank), !is.na(Points))
model_df_ppg <- master |>
  filter(!is.na(Financial_Rank), !is.na(Points_Per_Game))

modA        <- lm(League_Position ~ Financial_Rank, data = model_df_pos)
modH_pos    <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df_pos)
modH_points <- lm(Points ~ Financial_Rank + I(Financial_Rank^2), data = model_df_points)
modH_ppg    <- lm(Points_Per_Game ~ Financial_Rank + I(Financial_Rank^2), data = model_df_ppg)

evolution_row <- function(model_name, outcome, model, n) {
  s      <- summary(model)
  co     <- coef(s)
  fr_p   <- if ("Financial_Rank" %in% rownames(co)) co["Financial_Rank", "Estimate"] else NA_real_
  fr_pv  <- if ("Financial_Rank" %in% rownames(co)) co["Financial_Rank", "Pr(>|t|)"] else NA_real_
  fr2_p  <- if ("I(Financial_Rank^2)" %in% rownames(co)) co["I(Financial_Rank^2)", "Estimate"] else NA_real_
  fr2_pv <- if ("I(Financial_Rank^2)" %in% rownames(co)) co["I(Financial_Rank^2)", "Pr(>|t|)"] else NA_real_

  tibble(
    Model              = model_name,
    Outcome            = outcome,
    N                  = n,
    R_squared          = round(s$r.squared, 4),
    Adj_R_squared      = round(s$adj.r.squared, 4),
    AIC                = round(AIC(model), 2),
    FR_coef            = round(unname(fr_p), 5),
    FR_squared_coef    = if (is.na(fr2_p)) NA_real_ else round(unname(fr2_p), 5),
    FR_pvalue          = signif(unname(fr_pv), 4),
    FR_squared_pvalue  = if (is.na(fr2_pv)) NA_real_ else signif(unname(fr2_pv), 4)
  )
}

evolution_table <- bind_rows(
  evolution_row("A: Linear baseline",  "League_Position",  modA,        nrow(model_df_pos)),
  evolution_row("H: Quadratic (position)", "League_Position", modH_pos, nrow(model_df_pos)),
  evolution_row("H: Quadratic (points)",   "Points",           modH_points, nrow(model_df_points)),
  evolution_row("H: Quadratic (PPG)",      "Points_Per_Game",  modH_ppg,    nrow(model_df_ppg))
)

path_e1 <- "extensions/outputs/E1_model_evolution_table.csv"
write_csv(evolution_table, path_e1)
track(path_e1)
cat(sprintf("  Saved: %s\n", path_e1))
print(as.data.frame(evolution_table))
cat("TASK 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 2 — Four-panel visual comparison
#   →  E2_model_evolution_4panel.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 2: Four-Panel Visual Comparison ===\n")

make_evolution_panel <- function(data, yvar, model, title, ylab,
                                  reverse_y = FALSE, r2_corner = "top") {
  yvar_sym <- sym(yvar)
  fr_range <- range(data$Financial_Rank, na.rm = TRUE)
  fr_seq   <- seq(fr_range[1], fr_range[2], length.out = 300)

  pred <- predict(model, newdata = data.frame(Financial_Rank = fr_seq),
                   interval = "confidence", level = 0.95) |>
    as.data.frame() |>
    mutate(Financial_Rank = fr_seq)

  r2       <- summary(model)$r.squared
  y_range  <- range(data[[yvar]], na.rm = TRUE)
  label_y  <- if (r2_corner == "top") y_range[2] - 0.06 * diff(y_range)
              else                    y_range[1] + 0.06 * diff(y_range)

  p <- ggplot() +
    geom_jitter(data = data, aes(x = Financial_Rank, y = !!yvar_sym, colour = League),
                alpha = 0.3, size = 1.1, width = 0.18, height = 0.15) +
    geom_ribbon(data = pred, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
                fill = "#2C3E73", alpha = 0.15) +
    geom_line(data = pred, aes(x = Financial_Rank, y = fit),
              colour = "#2C3E73", linewidth = 1.2) +
    annotate("label", x = fr_range[1] + 0.05 * diff(fr_range), y = label_y,
             label = sprintf("R² = %.3f", r2),
             colour = "#2C3E73", fill = "white", size = 3.6, fontface = "bold", hjust = 0) +
    scale_colour_manual(values = league_colours) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    labs(title = title, x = "Financial Rank (FR 1 = richest squad)", y = ylab, colour = "League") +
    theme_clean

  if (reverse_y) p <- p + scale_y_reverse()
  p
}

panel1 <- make_evolution_panel(model_df_pos, "League_Position", modA,
                                "Model A: Linear Baseline",
                                "Final league position (1 = champion)",
                                reverse_y = TRUE, r2_corner = "bottom")

panel2 <- make_evolution_panel(model_df_pos, "League_Position", modH_pos,
                                "Model H: Quadratic (Position)",
                                "Final league position (1 = champion)",
                                reverse_y = TRUE, r2_corner = "bottom")

panel3 <- make_evolution_panel(model_df_points, "Points", modH_points,
                                "Model H: Total Points",
                                "Points won that season",
                                reverse_y = FALSE, r2_corner = "bottom")

panel4 <- make_evolution_panel(model_df_ppg, "Points_Per_Game", modH_ppg,
                                "Model H: Points Per Game",
                                "Points per game that season",
                                reverse_y = FALSE, r2_corner = "bottom")

p_4panel <- (panel1 | panel2) / (panel3 | panel4) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "Model Evolution: From Linear Baseline to Points Per Game",
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, hjust = 0.5, margin = margin(b = 6))
    )
  ) &
  theme(legend.position = "bottom")

path_e2 <- "extensions/outputs/E2_model_evolution_4panel.png"
ggsave(path_e2, p_4panel, width = 12, height = 10, dpi = 150)
track(path_e2)
cat(sprintf("  Saved: %s\n", path_e2))
cat("TASK 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 3 — Inflation validation chart
#   →  E3_inflation_validation.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 3: Inflation Validation Chart ===\n")

season_avg <- master |>
  filter(!is.na(Market_Value_M_EUR)) |>
  group_by(Season) |>
  summarise(Avg_Market_Value_M_EUR = mean(Market_Value_M_EUR, na.rm = TRUE), .groups = "drop") |>
  mutate(Season = factor(Season, levels = season_order)) |>
  arrange(Season)

base_avg <- season_avg$Avg_Market_Value_M_EUR[season_avg$Season == "2019-2020"]
season_avg <- season_avg |>
  mutate(Inflation_Index = round(Avg_Market_Value_M_EUR / base_avg, 4))

cat(sprintf("  Base year 2019-20 average squad value: €%.1fM (index = 1.000)\n", base_avg))

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

rank_compare <- master |>
  filter(!is.na(Financial_Rank), !is.na(Financial_Rank_Adjusted))

n_rank_changes <- sum(rank_compare$Financial_Rank != rank_compare$Financial_Rank_Adjusted)

cat(sprintf("  Team-seasons compared (complete cases): %d\n", nrow(rank_compare)))
cat(sprintf("  Genuine rank changes after inflation adjustment: %d\n", n_rank_changes))

# ── Left panel: inflation index line chart ─────────────────────
p_left <- ggplot(season_avg, aes(x = Season, y = Inflation_Index, group = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.6) +
  geom_line(colour = "#2C3E73", linewidth = 1.3) +
  geom_point(colour = "#2C3E73", size = 2.6) +
  geom_text(aes(label = sprintf("%.2f", Inflation_Index)),
            vjust = -1.1, size = 3, colour = "#2C3E73") +
  annotate("text", x = "2019-2020", y = 1.0, label = "Base year (2019-20 = 1.00)",
           vjust = 1.8, hjust = 0.35, size = 3, colour = "grey40", fontface = "italic") +
  scale_y_continuous(labels = number_format(accuracy = 0.01),
                      expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Transfer Market Inflation Index, 2015-16 to 2024-25",
       x = "Season", y = "Inflation Index (2019-20 = 1.00)") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# ── Right panel: FR vs FR_Adjusted diagonal check ───────────────
fr_lim <- range(c(rank_compare$Financial_Rank, rank_compare$Financial_Rank_Adjusted), na.rm = TRUE)

p_right <- ggplot(rank_compare, aes(x = Financial_Rank, y = Financial_Rank_Adjusted)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linewidth = 0.8, linetype = "dashed") +
  geom_jitter(aes(colour = League), alpha = 0.35, size = 1.4, width = 0.12, height = 0.12) +
  annotate("label", x = fr_lim[1] + 0.05 * diff(fr_lim), y = fr_lim[2] - 0.04 * diff(fr_lim),
           label = sprintf("%d genuine rank changes\nafter inflation adjustment\n(N = %d team-seasons)",
                           n_rank_changes, nrow(rank_compare)),
           colour = "grey20", fill = "white", size = 3.3, fontface = "bold", hjust = 0, vjust = 1) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20)) +
  coord_equal() +
  labs(title = "Original vs Inflation-Adjusted Financial Rank",
       x = "Original Financial Rank", y = "Inflation-Adjusted Financial Rank", colour = "League") +
  theme_clean

p_inflation <- (p_left | p_right) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "Inflation Check: Transfer Market Values 2015-2025",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 6)))
  ) &
  theme(legend.position = "bottom")

path_e3 <- "extensions/outputs/E3_inflation_validation.png"
ggsave(path_e3, p_inflation, width = 12, height = 6, dpi = 150)
track(path_e3)
cat(sprintf("  Saved: %s\n", path_e3))
cat("TASK 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 4 — Coefficient evolution plot
#   →  E4_coefficient_evolution.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 4: Coefficient Evolution Plot ===\n")

coef_row <- function(model_name, model) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) |>
    filter(term == "Financial_Rank") |>
    transmute(Model = model_name, Estimate = estimate, Conf_Low = conf.low, Conf_High = conf.high)
}

coef_evolution <- bind_rows(
  coef_row("A: Linear baseline\n(Position)",       modA),
  coef_row("H: Quadratic\n(Position)",              modH_pos),
  coef_row("H: Quadratic\n(Points)",                modH_points),
  coef_row("H: Quadratic\n(PPG)",                   modH_ppg)
) |>
  mutate(Model = factor(Model, levels = rev(Model)))

p_e4 <- ggplot(coef_evolution, aes(x = Estimate, y = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.6) +
  geom_errorbar(aes(xmin = Conf_Low, xmax = Conf_High), orientation = "y", width = 0.15,
                colour = "#2C3E73", linewidth = 1) +
  geom_point(size = 3.4, colour = "#2C3E73") +
  geom_text(aes(label = sprintf("%.3f", Estimate)), vjust = -1.1, size = 3.4, colour = "#2C3E73") +
  labs(
    title    = "Financial Rank Slope Coefficient Across Model Specifications",
    subtitle = "Point = coefficient estimate  •  Bars = 95% confidence interval",
    x        = "Financial_Rank coefficient (linear term)",
    y        = NULL
  ) +
  theme_clean

path_e4 <- "extensions/outputs/E4_coefficient_evolution.png"
ggsave(path_e4, p_e4, width = 10, height = 5, dpi = 150)
track(path_e4)
cat(sprintf("  Saved: %s\n", path_e4))
cat("TASK 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 5 — Clean summary for the professor
# ══════════════════════════════════════════════════════════════
cat("=== TASK 5: Summary for the Professor ===\n\n")

cat("+----------------------------------------------------------------------------------+\n")
cat("|                     MODEL EVOLUTION SUMMARY (FR + FR^2)                          |\n")
cat("+----------------------------------------------------------------------------------+\n")

summary_specs <- list(
  list(label = "A: Linear baseline",       outcome = "League_Position", model = modA,
       note  = "Establishes that FR predicts position at all — a straight line only."),
  list(label = "H: Quadratic (Position)",  outcome = "League_Position", model = modH_pos,
       note  = "Curved fit captures the sharp drop-off for the very richest clubs."),
  list(label = "H: Quadratic (Points)",    outcome = "Points",          model = modH_points,
       note  = "Switches to points won — finer-grained than integer league position."),
  list(label = "H: Quadratic (PPG)",       outcome = "Points_Per_Game", model = modH_ppg,
       note  = "Puts every league on a common per-match scale (professor's suggestion).")
)

for (spec in summary_specs) {
  s   <- summary(spec$model)
  co  <- coef(s)
  fr  <- co["Financial_Rank", "Estimate"]
  cat(sprintf("\n  Model: %-24s  Outcome: %s\n", spec$label, spec$outcome))
  cat(sprintf("    R² = %-8.4f  AIC = %-10.2f  FR slope = %.5f\n", s$r.squared, AIC(spec$model), fr))
  cat(sprintf("    -> %s\n", spec$note))
}

cat("\n+----------------------------------------------------------------------------------+\n")
cat("|                                  WARNINGS                                        |\n")
cat("+----------------------------------------------------------------------------------+\n")

cat("\n  [1] COVID-shortened Ligue 1 2019-20 season:\n")
if (nrow(schedule_check) > 0) {
  ligue1_covid <- schedule_check |> filter(League == "Ligue 1", Season == "2019-2020")
  if (nrow(ligue1_covid) > 0) {
    cat(sprintf("      Ligue 1 2019-20 was curtailed at %d actual matches (W+D+L), but\n",
                ligue1_covid$Actual_Matches[1]))
    cat(sprintf("      Points_Per_Game still divides by the assumed %d-match schedule.\n",
                ligue1_covid$Matches_Played[1]))
    cat("      -> PPG values for Ligue 1 2019-20 teams are biased DOWNWARD relative to\n")
    cat("         seasons that were played to completion. Interpret that season's PPG\n")
    cat("         figures with caution; do not treat them as directly comparable.\n")
  } else {
    cat("      No Ligue 1 2019-20 schedule deviation detected in this run of the data.\n")
  }
  other_deviations <- schedule_check |> filter(!(League == "Ligue 1" & Season == "2019-2020"))
  if (nrow(other_deviations) > 0) {
    cat("      Other league-seasons with a non-standard match count:\n")
    print(as.data.frame(other_deviations))
  }
} else {
  cat("      No schedule-length deviations detected in this run of the data.\n")
}

cat("\n  [2] SPAL missing-value artifact (Serie A):\n")
missing_mv <- master |> filter(is.na(Market_Value_M_EUR)) |> distinct(League, Season, Team)
if (nrow(missing_mv) > 0) {
  cat("      The following team-season(s) have no matched Market_Value_M_EUR\n")
  cat("      (unmatched name during the standings/market-value merge):\n")
  print(as.data.frame(missing_mv))
  cat("      -> This team is excluded from all FR-based models above (complete-case\n")
  cat("         analysis) and from the inflation rank-check scatter in Task 3. Any\n")
  cat("         apparent Financial_Rank shift in that league-season is a pre-existing\n")
  cat("         data-completeness gap, not an effect of inflation adjustment.\n")
} else {
  cat("      RESOLVED: no missing Market_Value_M_EUR rows in this run — SPAL's true\n")
  cat("      2017-18 squad value (€63.55M, Financial_Rank 16) was inserted by\n")
  cat("      extensions/scripts/15_spal_correction.R. The 4 apparent rank shifts\n")
  cat("      previously seen in the inflation check are gone as a result (see Task 3).\n")
}

cat("\nTASK 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Output Inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s) in extensions/outputs/:\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n13_model_evolution_comparison.R complete.\n")
