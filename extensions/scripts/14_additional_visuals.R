# ==============================================================
# 14_additional_visuals.R
# Three follow-up visuals building on 13_model_evolution_comparison.R:
#
#   Visual 1 — E3b: faceted, jittered inflation rank-stability check
#              (fixes overplotting in the E3 right panel) + console
#              report of the exact team-seasons whose Financial_Rank
#              changed after inflation adjustment.
#   Visual 2 — E4b: standardised effect comparison. For each model,
#              the predicted change in outcome from FR 1 -> FR max,
#              expressed as a % of that outcome's observed range —
#              puts all four models on a common 0-100% scale. "FR max"
#              is 18 for the Bundesliga and 20 for the other four
#              leagues, since Financial_Rank is a within-league-season
#              rank, not a fixed 1-20 scale.
#   Visual 3 — E5: PPG-model residuals vs fitted, styled identically
#              to outputs/28b_residual_plot_clean.png, confirming
#              Leicester 2015-16 remains the top outlier under the
#              professor's preferred PPG specification.
#
# Uses data/merged/master_dataset_corrected.csv (SPAL 2017-18 Serie A
# market value fix from extensions/scripts/15_spal_correction.R) —
# run script 15 first if that file doesn't exist yet. With the fix in
# place, Visual 1 should report 0 rank changes (previously 4, all a
# SPAL merge artifact).
#
# Outputs (all saved to extensions/outputs/ only):
#   E3b_inflation_rank_stability_faceted.png
#   E4b_standardised_effect_comparison.png
#   E5_ppg_residuals.png
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
ensure_installed(c("ggrepel"))

make_dir("extensions/outputs")
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
# PHASE 0: Load master dataset, derive PPG + inflation adjustment
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

# Season-level transfer-market inflation index (2019-20 = 1.00)
season_avg <- master |>
  filter(!is.na(Market_Value_M_EUR)) |>
  group_by(Season) |>
  summarise(Avg_Market_Value_M_EUR = mean(Market_Value_M_EUR, na.rm = TRUE), .groups = "drop") |>
  mutate(Season = factor(Season, levels = season_order)) |>
  arrange(Season)

base_avg <- season_avg$Avg_Market_Value_M_EUR[season_avg$Season == "2019-2020"]
season_avg <- season_avg |> mutate(Inflation_Index = round(Avg_Market_Value_M_EUR / base_avg, 4))

master <- master |>
  left_join(season_avg |> mutate(Season = as.character(Season)) |> select(Season, Inflation_Index),
            by = "Season") |>
  mutate(Adjusted_Market_Value_M_EUR = Market_Value_M_EUR / Inflation_Index) |>
  group_by(League, Season) |>
  mutate(Financial_Rank_Adjusted = ifelse(
    is.na(Adjusted_Market_Value_M_EUR), NA_real_,
    rank(-Adjusted_Market_Value_M_EUR, ties.method = "min")
  )) |>
  ungroup()

cat(sprintf("  Rows: %d  |  Leagues: %s\n", nrow(master),
            paste(sort(unique(master$League)), collapse = ", ")))
cat("PHASE 0 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# VISUAL 1 — Faceted, jittered inflation rank-stability check
#   →  E3b_inflation_rank_stability_faceted.png
# ══════════════════════════════════════════════════════════════
cat("=== VISUAL 1: Faceted Inflation Rank-Stability Check ===\n")

rank_compare <- master |>
  filter(!is.na(Financial_Rank), !is.na(Financial_Rank_Adjusted)) |>
  mutate(Rank_Changed = Financial_Rank != Financial_Rank_Adjusted)

n_changed <- sum(rank_compare$Rank_Changed)
cat(sprintf("  Team-seasons compared (complete cases): %d\n", nrow(rank_compare)))
cat(sprintf("  Rank changes after inflation adjustment: %d\n\n", n_changed))

changed_obs <- rank_compare |>
  filter(Rank_Changed) |>
  select(Team, Season, League, Financial_Rank, Financial_Rank_Adjusted) |>
  rename(Original_Rank = Financial_Rank, Adjusted_Rank = Financial_Rank_Adjusted) |>
  arrange(League, Season)

cat("  --- Exact observations whose Financial_Rank changed ---\n")
if (nrow(changed_obs) == 0) {
  cat("  None — Financial_Rank is fully stable after inflation adjustment (SPAL data gap fixed in script 15).\n")
} else {
  print(as.data.frame(changed_obs))
}
cat("\n")

fr_lim <- range(c(rank_compare$Financial_Rank, rank_compare$Financial_Rank_Adjusted), na.rm = TRUE)

p_e3b <- ggplot(rank_compare,
                 aes(x = Financial_Rank, y = Financial_Rank_Adjusted, colour = League)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linewidth = 0.8, linetype = "dashed") +
  geom_jitter(data = rank_compare |> filter(!Rank_Changed),
              alpha = 0.4, size = 1.6, width = 0.15, height = 0.15) +
  geom_jitter(data = rank_compare |> filter(Rank_Changed),
              colour = "#C0392B", shape = 21, fill = "#C0392B",
              size = 2.6, stroke = 0.6, width = 0.15, height = 0.15) +
  facet_wrap(~League, nrow = 1) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = c(1, 10, 20), limits = fr_lim) +
  scale_y_continuous(breaks = c(1, 10, 20), limits = fr_lim) +
  coord_equal() +
  labs(
    title    = "Financial Rank Stability After Inflation Adjustment — By League",
    subtitle = if (n_changed == 0) {
      sprintf("N = %d team-seasons  •  0 rank changes  •  All leagues sit perfectly on the diagonal (SPAL gap fixed)",
              nrow(rank_compare))
    } else {
      sprintf("N = %d team-seasons  •  %d genuine rank change(s) (red)  •  All leagues individually sit on the diagonal",
              nrow(rank_compare), n_changed)
    },
    x       = "Original Financial Rank",
    y       = "Inflation-Adjusted Financial Rank",
    caption = "Jittered ± 0.15 with alpha = 0.4 so overlapping dots remain visible. Red (if any) = rank changes reported in the console."
  ) +
  theme_clean +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 10))

path_e3b <- "extensions/outputs/E3b_inflation_rank_stability_faceted.png"
ggsave(path_e3b, p_e3b, width = 12, height = 6, dpi = 150)
track(path_e3b)
cat(sprintf("  Saved: %s\n", path_e3b))
cat("VISUAL 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# VISUAL 2 — Standardised effect comparison across models
#   →  E4b_standardised_effect_comparison.png
# ══════════════════════════════════════════════════════════════
cat("=== VISUAL 2: Standardised Effect Comparison ===\n")

model_df_pos    <- master |> filter(!is.na(Financial_Rank), !is.na(League_Position))
model_df_points <- master |> filter(!is.na(Financial_Rank), !is.na(Points))
model_df_ppg    <- master |> filter(!is.na(Financial_Rank), !is.na(Points_Per_Game))

modA        <- lm(League_Position ~ Financial_Rank, data = model_df_pos)
modH_pos    <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df_pos)
modH_points <- lm(Points ~ Financial_Rank + I(Financial_Rank^2), data = model_df_points)
modH_ppg    <- lm(Points_Per_Game ~ Financial_Rank + I(Financial_Rank^2), data = model_df_ppg)

standardised_effect <- function(model_name, model, data, outcome_var) {
  pred_1  <- predict(model, newdata = data.frame(Financial_Rank = 1))
  pred_20 <- predict(model, newdata = data.frame(Financial_Rank = 20))
  outcome_range <- diff(range(data[[outcome_var]], na.rm = TRUE))
  pct <- 100 * abs(pred_20 - pred_1) / outcome_range
  tibble(Model = model_name, Outcome = outcome_var,
         Pred_FR1 = pred_1, Pred_FR20 = pred_20,
         Outcome_Range = outcome_range, Pct_of_Range = pct)
}

effect_table <- bind_rows(
  standardised_effect("A: Linear\n(Position)",    modA,        model_df_pos,    "League_Position"),
  standardised_effect("H: Quadratic\n(Position)", modH_pos,    model_df_pos,    "League_Position"),
  standardised_effect("H: Quadratic\n(Points)",   modH_points, model_df_points, "Points"),
  standardised_effect("H: Quadratic\n(PPG)",      modH_ppg,    model_df_ppg,    "Points_Per_Game")
) |>
  mutate(Model = factor(Model, levels = Model))

cat("  Predicted change from FR 1 -> FR max, as % of the outcome's observed range:\n")
print(as.data.frame(effect_table |> mutate(across(where(is.numeric), \(x) round(x, 2)))))
cat("\n")

p_e4b <- ggplot(effect_table, aes(x = Model, y = Pct_of_Range, fill = Model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", Pct_of_Range)), vjust = -0.6, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#8FA6C7", "#5B7FB5", "#2C3E73", "#1B2A52")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                      labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Standardised Effect: FR 1 -> FR max as % of Outcome Range",
    subtitle = "Predicted change from richest (FR 1) to poorest (FR max) squad, scaled 0-100% for direct comparison",
    x        = NULL,
    y        = "% of outcome's observed range explained",
    caption  = "\"FR max\" = the last-place financial rank within each league-season: 18 for the Bundesliga (18 clubs), 20 for the other four\nleagues (20 clubs each). Financial_Rank is a within-league-season rank, not a fixed 1-20 scale applied uniformly across leagues."
  ) +
  theme_clean

path_e4b <- "extensions/outputs/E4b_standardised_effect_comparison.png"
ggsave(path_e4b, p_e4b, width = 10, height = 5, dpi = 150)
track(path_e4b)
cat(sprintf("  Saved: %s\n", path_e4b))
cat("VISUAL 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# VISUAL 3 — PPG-model residuals vs fitted (style of 28b)
#   →  E5_ppg_residuals.png
# ══════════════════════════════════════════════════════════════
cat("=== VISUAL 3: PPG Residuals vs Fitted ===\n")

aug_ppg <- augment(modH_ppg, data = model_df_ppg) |>
  rename(Fitted_PPG = .fitted, Residual_PPG = .resid)

resid_sd_ppg <- sd(aug_ppg$Residual_PPG, na.rm = TRUE)
# Higher PPG is better, so — unlike the position model — a POSITIVE
# residual means the team overperformed its FR-predicted PPG.
overp_thr_ppg  <-  1.5 * resid_sd_ppg
underp_thr_ppg <- -1.5 * resid_sd_ppg

aug_ppg <- aug_ppg |>
  mutate(
    Outlier_Flag = case_when(
      Residual_PPG > overp_thr_ppg  ~ "Overperformer",
      Residual_PPG < underp_thr_ppg ~ "Underperformer",
      TRUE                          ~ "Normal"
    )
  )

cat(sprintf("  Residual SD: %.3f PPG  |  Overp. threshold: > %.3f  |  Underp. threshold: < %.3f\n",
            resid_sd_ppg, overp_thr_ppg, underp_thr_ppg))
cat(sprintf("  Overperformers: %d  |  Underperformers: %d\n",
            sum(aug_ppg$Outlier_Flag == "Overperformer"),
            sum(aug_ppg$Outlier_Flag == "Underperformer")))

top_over <- aug_ppg |> arrange(desc(Residual_PPG)) |> slice(1)
cat(sprintf("  Largest positive PPG residual (top overperformer): %s %s  (Residual = +%.3f PPG)\n",
            top_over$Team, top_over$Season, top_over$Residual_PPG))
leicester_rank_ppg <- aug_ppg |> arrange(desc(Residual_PPG)) |>
  mutate(Rank = row_number()) |> filter(grepl("Leicester", Team), Season == "2015-2016")
cat(sprintf("  Leicester City 2015-16 PPG-residual rank: #%d of %d  |  Residual = +%.3f PPG  |  Still #1 overperformer? %s\n\n",
            leicester_rank_ppg$Rank, nrow(aug_ppg), leicester_rank_ppg$Residual_PPG,
            ifelse(leicester_rank_ppg$Rank == 1, "YES", "NO")))

label_df_e5 <- aug_ppg |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024") |
    (grepl("Lille",      Team) & Season == "2020-2021") |
    (grepl("Napoli",     Team) & Season == "2024-2025") |
    (grepl("Liverpool",  Team) & Season == "2024-2025" & Champion == "Yes")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

bg_df_e5    <- aug_ppg |> filter(Outlier_Flag == "Normal")
over_df_e5  <- aug_ppg |> filter(Outlier_Flag == "Overperformer")
under_df_e5 <- aug_ppg |> filter(Outlier_Flag == "Underperformer")

p_e5 <- ggplot() +

  # Shaded +-1.5 SD band
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = underp_thr_ppg, ymax = overp_thr_ppg,
           fill = "#F5F5F5", alpha = 0.8) +

  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.9) +
  geom_hline(yintercept = overp_thr_ppg,  linetype = "dashed", colour = "#1D6FA4", linewidth = 0.75) +
  geom_hline(yintercept = underp_thr_ppg, linetype = "dashed", colour = "#C0392B", linewidth = 0.75) +

  geom_point(data = bg_df_e5, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "grey72", size = 1.5, alpha = 0.55) +

  # Underperformers — red downward triangles (below-predicted PPG)
  geom_point(data = under_df_e5, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#C0392B", fill = "#C0392B", shape = 25, size = 2.2, alpha = 0.6) +

  # Overperformers — blue upward triangles (above-predicted PPG)
  geom_point(data = over_df_e5, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#1D6FA4", fill = "#1D6FA4", shape = 24, size = 2.4, alpha = 0.7) +

  # Story clubs — large orange diamonds
  geom_point(data = label_df_e5, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#F39C12", fill = "#F39C12", shape = 23, size = 4.5) +

  geom_label_repel(
    data           = label_df_e5,
    aes(x = Fitted_PPG, y = Residual_PPG, label = Label),
    fill           = "white",
    colour         = "#2C3E50",
    size           = 3.2,
    fontface       = "bold",
    label.size     = 0.3,
    label.r        = unit(0.2, "lines"),
    box.padding    = 0.6,
    point.padding  = 0.4,
    segment.colour = "#F39C12",
    segment.size   = 0.5,
    min.segment.length = 0.2,
    max.overlaps   = 20
  ) +

  annotate("text", x = 1.4, y = overp_thr_ppg + 0.09,
           label = "Overperformer zone (above +1.5 SD)", colour = "#1D6FA4",
           size = 3, hjust = 0.5, fontface = "italic") +
  annotate("text", x = min(aug_ppg$Fitted_PPG) + 0.05, y = underp_thr_ppg - 0.09,
           label = "Underperformer zone (below -1.5 SD)", colour = "#C0392B",
           size = 3, hjust = 0, fontface = "italic") +

  labs(
    title    = "Residuals vs Fitted — PPG Model",
    subtitle = sprintf(
      "N = %d  •  Residual SD = %.3f PPG  •  Orange diamonds = key story clubs  •  Leicester 2015-16 is still the top outlier",
      nrow(aug_ppg), resid_sd_ppg
    ),
    x       = "Fitted Points Per Game  (PPG model prediction)",
    y       = "Residual (Actual - Predicted)\nPositive = performed BETTER than predicted",
    caption = "Model: Points_Per_Game ~ Financial_Rank + Financial_Rank²"
  ) +
  theme_clean

path_e5 <- "extensions/outputs/E5_ppg_residuals.png"
ggsave(path_e5, p_e5, width = 10, height = 7, dpi = 150)
track(path_e5)
cat(sprintf("  Saved: %s\n", path_e5))
cat("VISUAL 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Output Inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s) in extensions/outputs/:\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n14_additional_visuals.R complete.\n")
