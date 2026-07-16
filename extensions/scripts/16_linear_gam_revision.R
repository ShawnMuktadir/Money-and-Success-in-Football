# ==============================================================
# 16_linear_gam_revision.R
# Responds to Jacob's updated feedback: league position is not a
# suitable outcome (not continuous), and polynomial terms should
# not be used. This drops the quadratic Model H entirely and tests
# two alternatives, both using Points_Per_Game as the outcome and
# Financial_Rank as the only predictor:
#
#   Model 1 — Linear:  Points_Per_Game ~ Financial_Rank
#   Model 2 — GAM:     Points_Per_Game ~ s(Financial_Rank)
#
# Uses data/merged/master_dataset_corrected.csv (SPAL 2017-18 Serie A
# market value fix from extensions/scripts/15_spal_correction.R) —
# run script 15 first if that file doesn't exist yet.
#
#   Table 1 — This script's two new models vs the quadratic Model H
#             (PPG) reference, read from E1_model_evolution_table.csv
#   Table 2 — Combined master comparison across the whole project:
#             all four E1 rows (read-only) + the two new models here,
#             flagged against Jacob's feedback criteria
#   Plot 1  — Residuals vs fitted for the linear PPG model (E5 style)
#   Plot 2  — GAM fitted smooth vs Financial_Rank with CI band
#   Plot 3  — Residuals vs fitted for the GAM PPG model (E5 style),
#             with a check for new standout outliers vs earlier work
#
# This script only READS extensions/outputs/E1_model_evolution_table.csv
# — it never writes to it. Nothing in extensions/outputs/ (E1-E6) or
# anywhere else in the project is modified.
#
# Outputs (all new, all in extensions/outputs/gam_revision/):
#   gam_revision_table.csv
#   gam_revision_table.html          (formatted version, recommended row bolded)
#   combined_model_comparison.csv
#   combined_model_comparison.html   (formatted version, recommended row bolded)
#   linear_ppg_residuals.png
#   gam_smooth_confidence.png
#   gam_ppg_residuals.png
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
ensure_installed(c("ggrepel", "mgcv", "kableExtra"))

OUTPUT_DIR <- "extensions/outputs/gam_revision"
make_dir(OUTPUT_DIR)
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

BIG4_LEAGUES <- c("Premier League", "La Liga", "Serie A", "Ligue 1")


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

model_df_ppg <- master |>
  filter(!is.na(Financial_Rank), !is.na(Points_Per_Game))

cat(sprintf("  Rows: %d  |  Complete cases for FR + PPG: %d\n", nrow(master), nrow(model_df_ppg)))
cat("PHASE 0 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 1: Fit the two new models
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Fit Linear and GAM Models (PPG ~ Financial_Rank) ===\n")

lin_ppg <- lm(Points_Per_Game ~ Financial_Rank, data = model_df_ppg)
gam_ppg <- mgcv::gam(Points_Per_Game ~ s(Financial_Rank), data = model_df_ppg, method = "REML")

lin_summary <- summary(lin_ppg)
gam_summary <- summary(gam_ppg)

gam_edf     <- unname(gam_summary$s.table[1, "edf"])
gam_devexpl <- gam_summary$dev.expl

cat(sprintf("  Linear PPG model:  R² = %.4f  |  AIC = %.2f\n", lin_summary$r.squared, AIC(lin_ppg)))
cat(sprintf("  GAM PPG model:     Deviance explained = %.4f  |  edf = %.3f  |  AIC = %.2f\n",
            gam_devexpl, gam_edf, AIC(gam_ppg)))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 1 — Table 1: this script's two new models vs quadratic
#          Model H (PPG) reference, read from E1 (not refit)
#   →  gam_revision_table.csv
# ══════════════════════════════════════════════════════════════
cat("=== TASK 1: gam_revision_table (this script's models) ===\n")

e1_path <- "extensions/outputs/E1_model_evolution_table.csv"
if (!file.exists(e1_path)) stop("Cannot find E1_model_evolution_table.csv — run extensions/scripts/13_model_evolution_comparison.R first.")
e1_table <- read_csv(e1_path, show_col_types = FALSE)

modH_ppg_ref <- e1_table |> filter(Model == "H: Quadratic (PPG)")
if (nrow(modH_ppg_ref) != 1) stop("Expected exactly one 'H: Quadratic (PPG)' row in E1_model_evolution_table.csv.")

gam_revision_table <- tibble(
  Model                 = c("H: Quadratic (PPG) [reference]", "Linear (PPG)", "GAM (PPG)"),
  Outcome               = "Points_Per_Game",
  N                     = c(modH_ppg_ref$N, nrow(model_df_ppg), nrow(model_df_ppg)),
  R2_or_Dev_Explained  = c(modH_ppg_ref$R_squared,
                            round(lin_summary$r.squared, 4),
                            round(gam_devexpl, 4)),
  AIC                   = c(modH_ppg_ref$AIC, round(AIC(lin_ppg), 2), round(AIC(gam_ppg), 2)),
  EDF                   = c(NA_real_, NA_real_, round(gam_edf, 3))
)

path_gam_table <- file.path(OUTPUT_DIR, "gam_revision_table.csv")
write_csv(gam_revision_table, path_gam_table)
track(path_gam_table)
cat(sprintf("  Saved: %s\n", path_gam_table))
print(as.data.frame(gam_revision_table))
cat("TASK 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 2 — Table 2: combined master comparison across the
#          whole project (E1 rows read-only + this script's rows)
#   →  combined_model_comparison.csv
# ══════════════════════════════════════════════════════════════
cat("=== TASK 2: combined_model_comparison (whole-project master table) ===\n")

e1_rows <- e1_table |>
  transmute(
    Model,
    Outcome,
    N,
    R2_or_Dev_Explained = R_squared,
    AIC,
    EDF = NA_real_,
    Has_Polynomial_Term = grepl("Quadratic", Model)
  )

new_rows <- tibble(
  Model                = c("Linear (PPG)", "GAM (PPG)"),
  Outcome              = "Points_Per_Game",
  N                    = c(nrow(model_df_ppg), nrow(model_df_ppg)),
  R2_or_Dev_Explained = c(round(lin_summary$r.squared, 4), round(gam_devexpl, 4)),
  AIC                  = c(round(AIC(lin_ppg), 2), round(AIC(gam_ppg), 2)),
  EDF                  = c(NA_real_, round(gam_edf, 3)),
  Has_Polynomial_Term  = c(FALSE, FALSE)
)

combined_table <- bind_rows(e1_rows, new_rows) |>
  mutate(
    meets_feedback_criteria = (Outcome == "Points_Per_Game") & !Has_Polynomial_Term
  ) |>
  select(-Has_Polynomial_Term)

best_fit <- combined_table |>
  filter(meets_feedback_criteria) |>
  slice_max(order_by = R2_or_Dev_Explained, n = 1, with_ties = FALSE)

combined_table <- combined_table |>
  mutate(recommended = meets_feedback_criteria & (Model == best_fit$Model))

path_combined <- file.path(OUTPUT_DIR, "combined_model_comparison.csv")
write_csv(combined_table, path_combined)
track(path_combined)
cat(sprintf("  Saved: %s\n", path_combined))
print(as.data.frame(combined_table))

cat(sprintf(
  "\n  SUMMARY: Recommended model = \"%s\" (Points_Per_Game outcome, no polynomial term, %s = %.4f — the best fit among models meeting Jacob's feedback criteria).\n",
  best_fit$Model,
  ifelse(best_fit$Model == "GAM (PPG)", "deviance explained", "R-squared"),
  best_fit$R2_or_Dev_Explained
))
cat("TASK 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 3 — Formatted (HTML) versions with the recommended row bolded
# ══════════════════════════════════════════════════════════════
cat("=== TASK 3: Formatted HTML Tables ===\n")

bold_gam_row <- which(gam_revision_table$Model == "GAM (PPG)")
gam_html <- kableExtra::kbl(gam_revision_table, digits = 4,
                             caption = "Table 1: Linear & GAM PPG models vs quadratic Model H (reference)") |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) |>
  kableExtra::row_spec(bold_gam_row, bold = TRUE, background = "#EAF2FB")
path_gam_html <- file.path(OUTPUT_DIR, "gam_revision_table.html")
kableExtra::save_kable(gam_html, path_gam_html)
track(path_gam_html)
cat(sprintf("  Saved: %s\n", path_gam_html))

bold_combined_row <- which(combined_table$recommended)
combined_html <- kableExtra::kbl(combined_table, digits = 4,
                                  caption = "Table 2: Combined model comparison across the whole project") |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) |>
  kableExtra::row_spec(bold_combined_row, bold = TRUE, background = "#EAF2FB")
path_combined_html <- file.path(OUTPUT_DIR, "combined_model_comparison.html")
kableExtra::save_kable(combined_html, path_combined_html)
track(path_combined_html)
cat(sprintf("  Saved: %s\n", path_combined_html))
cat("TASK 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 4 — Residuals vs fitted, linear PPG model (E5 style)
#   →  linear_ppg_residuals.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 4: Linear PPG Residuals vs Fitted ===\n")

aug_lin <- augment(lin_ppg, data = model_df_ppg) |>
  rename(Fitted_PPG = .fitted, Residual_PPG = .resid)

resid_sd_lin  <- sd(aug_lin$Residual_PPG, na.rm = TRUE)
overp_thr_lin  <-  1.5 * resid_sd_lin
underp_thr_lin <- -1.5 * resid_sd_lin

aug_lin <- aug_lin |>
  mutate(
    Outlier_Flag = case_when(
      Residual_PPG > overp_thr_lin  ~ "Overperformer",
      Residual_PPG < underp_thr_lin ~ "Underperformer",
      TRUE                          ~ "Normal"
    )
  )

cat(sprintf("  Residual SD: %.3f PPG  |  Overp. threshold: > %.3f  |  Underp. threshold: < %.3f\n",
            resid_sd_lin, overp_thr_lin, underp_thr_lin))

label_df_lin <- aug_lin |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

leicester_rank_lin <- aug_lin |> arrange(desc(Residual_PPG)) |>
  mutate(Rank = row_number()) |> filter(grepl("Leicester", Team), Season == "2015-2016")
if (nrow(leicester_rank_lin) == 1) {
  cat(sprintf("  Leicester City 2015-16 residual rank under the LINEAR PPG model: #%d of %d  |  Residual = +%.3f PPG\n",
              leicester_rank_lin$Rank, nrow(aug_lin), leicester_rank_lin$Residual_PPG))
}
leverkusen_rank_lin <- aug_lin |> arrange(desc(Residual_PPG)) |>
  mutate(Rank = row_number()) |> filter(grepl("Leverkusen", Team), Season == "2023-2024")
if (nrow(leverkusen_rank_lin) == 1) {
  cat(sprintf("  Bayer Leverkusen 2023-24 residual rank under the LINEAR PPG model: #%d of %d  |  Residual = +%.3f PPG (overperformer)\n\n",
              leverkusen_rank_lin$Rank, nrow(aug_lin), leverkusen_rank_lin$Residual_PPG))
} else {
  cat("\n")
}

bg_df_lin    <- aug_lin |> filter(Outlier_Flag == "Normal")
over_df_lin  <- aug_lin |> filter(Outlier_Flag == "Overperformer")
under_df_lin <- aug_lin |> filter(Outlier_Flag == "Underperformer")

p_lin_resid <- ggplot() +

  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = underp_thr_lin, ymax = overp_thr_lin,
           fill = "#F5F5F5", alpha = 0.8) +

  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.9) +
  geom_hline(yintercept = overp_thr_lin,  linetype = "dashed", colour = "#1D6FA4", linewidth = 0.75) +
  geom_hline(yintercept = underp_thr_lin, linetype = "dashed", colour = "#C0392B", linewidth = 0.75) +

  geom_point(data = bg_df_lin, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "grey72", size = 1.5, alpha = 0.55) +

  geom_point(data = under_df_lin, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#C0392B", fill = "#C0392B", shape = 25, size = 2.2, alpha = 0.6) +

  geom_point(data = over_df_lin, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#1D6FA4", fill = "#1D6FA4", shape = 24, size = 2.4, alpha = 0.7) +

  geom_point(data = label_df_lin, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#F39C12", fill = "#F39C12", shape = 23, size = 4.5) +

  geom_label_repel(
    data           = label_df_lin,
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

  annotate("text", x = max(aug_lin$Fitted_PPG) - 0.15, y = overp_thr_lin + 0.09,
           label = "Overperformer zone (above +1.5 SD)", colour = "#1D6FA4",
           size = 3, hjust = 1, fontface = "italic") +
  annotate("text", x = min(aug_lin$Fitted_PPG) + 0.05, y = underp_thr_lin - 0.09,
           label = "Underperformer zone (below -1.5 SD)", colour = "#C0392B",
           size = 3, hjust = 0, fontface = "italic") +

  labs(
    title    = "Residuals vs Fitted — Linear PPG Model",
    subtitle = sprintf(
      "N = %d  •  Residual SD = %.3f PPG  •  Orange diamonds = key story clubs (labelled where still notable outliers)",
      nrow(aug_lin), resid_sd_lin
    ),
    x       = "Fitted Points Per Game  (linear model prediction)",
    y       = "Residual (Actual - Predicted)\nPositive = performed BETTER than predicted",
    caption = "Model: Points_Per_Game ~ Financial_Rank  (no polynomial term, per Jacob's feedback)"
  ) +
  theme_clean

path_lin_resid <- file.path(OUTPUT_DIR, "linear_ppg_residuals.png")
ggsave(path_lin_resid, p_lin_resid, width = 10, height = 7, dpi = 150)
track(path_lin_resid)
cat(sprintf("  Saved: %s\n", path_lin_resid))
cat("TASK 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 5 — GAM fitted smooth vs Financial_Rank with CI band
#   →  gam_smooth_confidence.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 5: GAM Fitted Smooth with Confidence Band ===\n")

fr_range <- range(model_df_ppg$Financial_Rank, na.rm = TRUE)
fr_seq   <- seq(fr_range[1], fr_range[2], length.out = 300)

gam_pred <- predict(gam_ppg, newdata = data.frame(Financial_Rank = fr_seq),
                     se.fit = TRUE)
gam_smooth_df <- tibble(
  Financial_Rank = fr_seq,
  fit            = gam_pred$fit,
  se             = gam_pred$se.fit
) |>
  mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se)

# Where the CI band is widest (least data-supported) vs narrowest
widest_pt    <- gam_smooth_df |> slice_max(order_by = upr - lwr, n = 1)
narrowest_pt <- gam_smooth_df |> slice_min(order_by = upr - lwr, n = 1)
cat(sprintf("  CI band narrowest (best-supported) near FR = %.1f (width = %.3f PPG)\n",
            narrowest_pt$Financial_Rank, narrowest_pt$upr - narrowest_pt$lwr))
cat(sprintf("  CI band widest (least-supported) near FR = %.1f (width = %.3f PPG)\n\n",
            widest_pt$Financial_Rank, widest_pt$upr - widest_pt$lwr))

p_gam_smooth <- ggplot() +
  geom_jitter(data = model_df_ppg, aes(x = Financial_Rank, y = Points_Per_Game, colour = League),
              alpha = 0.3, size = 1.1, width = 0.18, height = 0.02) +
  geom_ribbon(data = gam_smooth_df, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill = "#2C3E73", alpha = 0.18) +
  geom_line(data = gam_smooth_df, aes(x = Financial_Rank, y = fit),
            colour = "#2C3E73", linewidth = 1.2) +
  annotate("label", x = fr_range[1] + 0.05 * diff(fr_range),
           y = max(gam_smooth_df$upr) - 0.04 * diff(range(model_df_ppg$Points_Per_Game, na.rm = TRUE)),
           label = sprintf("Deviance explained = %.3f\nedf = %.2f", gam_devexpl, gam_edf),
           colour = "#2C3E73", fill = "white", size = 3.6, fontface = "bold", hjust = 0) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
  labs(
    title    = "GAM Fitted Smooth — Points Per Game vs Financial Rank",
    subtitle = "Shaded band = 95% confidence interval; widens where financial-rank values are sparser",
    x        = "Financial Rank (FR 1 = richest squad)",
    y        = "Points Per Game",
    colour   = "League",
    caption  = "Model: Points_Per_Game ~ s(Financial_Rank), fit with mgcv::gam(method = \"REML\")"
  ) +
  theme_clean

path_gam_smooth <- file.path(OUTPUT_DIR, "gam_smooth_confidence.png")
ggsave(path_gam_smooth, p_gam_smooth, width = 10, height = 7, dpi = 150)
track(path_gam_smooth)
cat(sprintf("  Saved: %s\n", path_gam_smooth))
cat("TASK 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# TASK 6 — GAM residuals vs fitted (same visual style as E5 /
#          linear_ppg_residuals.png), plus a check for any new
#          standout outliers that emerge specifically under the GAM
#   →  gam_ppg_residuals.png
# ══════════════════════════════════════════════════════════════
cat("=== TASK 6: GAM Residuals vs Fitted ===\n")

aug_gam <- augment(gam_ppg, data = model_df_ppg) |>
  rename(Fitted_PPG = .fitted, Residual_PPG = .resid)

resid_sd_gam   <- sd(aug_gam$Residual_PPG, na.rm = TRUE)
overp_thr_gam  <-  1.5 * resid_sd_gam
underp_thr_gam <- -1.5 * resid_sd_gam

aug_gam <- aug_gam |>
  mutate(
    Outlier_Flag = case_when(
      Residual_PPG > overp_thr_gam  ~ "Overperformer",
      Residual_PPG < underp_thr_gam ~ "Underperformer",
      TRUE                          ~ "Normal"
    )
  )

cat(sprintf("  Residual SD: %.3f PPG  |  Overp. threshold: > %.3f  |  Underp. threshold: < %.3f\n",
            resid_sd_gam, overp_thr_gam, underp_thr_gam))
cat(sprintf("  For comparison — residual SD: linear PPG = %.3f PPG  |  quadratic Model H (PPG) = 0.254 PPG\n",
            resid_sd_lin))

# Story clubs already flagged in earlier work (E5's PPG-model plot and
# this script's linear-model plot) — used below to detect whether the
# GAM surfaces any *new* standout club-seasons, not just the familiar ones.
previously_flagged <- tibble::tribble(
  ~Team_Pattern, ~Season,
  "Leicester",   "2015-2016",
  "Leverkusen",  "2023-2024",
  "Lille",       "2020-2021",
  "Napoli",      "2024-2025",
  "Liverpool",   "2024-2025"
)
is_previously_flagged <- function(team, season) {
  any(mapply(function(p, s) grepl(p, team) & season == s,
             previously_flagged$Team_Pattern, previously_flagged$Season))
}
aug_gam <- aug_gam |>
  rowwise() |>
  mutate(Previously_Flagged = is_previously_flagged(Team, Season)) |>
  ungroup()

leicester_gam <- aug_gam |> filter(grepl("Leicester", Team), Season == "2015-2016")
leverkusen_gam <- aug_gam |> filter(grepl("Leverkusen", Team), Season == "2023-2024")
if (nrow(leicester_gam) == 1) {
  cat(sprintf("  Leicester City 2015-16 under the GAM: Residual = %+.3f PPG  (%s)\n",
              leicester_gam$Residual_PPG, leicester_gam$Outlier_Flag))
}
if (nrow(leverkusen_gam) == 1) {
  cat(sprintf("  Bayer Leverkusen 2023-24 under the GAM: Residual = %+.3f PPG  (%s)\n",
              leverkusen_gam$Residual_PPG, leverkusen_gam$Outlier_Flag))
}

new_over <- aug_gam |>
  filter(Outlier_Flag == "Overperformer", !Previously_Flagged) |>
  arrange(desc(Residual_PPG)) |> slice(1)
new_under <- aug_gam |>
  filter(Outlier_Flag == "Underperformer", !Previously_Flagged) |>
  arrange(Residual_PPG) |> slice(1)

if (nrow(new_over) == 1) {
  cat(sprintf("  New standout overperformer under the GAM (not previously flagged): %s %s  (Residual = %+.3f PPG)\n",
              new_over$Team, new_over$Season, new_over$Residual_PPG))
}
if (nrow(new_under) == 1) {
  cat(sprintf("  New standout underperformer under the GAM (not previously flagged): %s %s  (Residual = %+.3f PPG)\n",
              new_under$Team, new_under$Season, new_under$Residual_PPG))
}
cat("\n")

label_df_gam <- bind_rows(
  leicester_gam  |> filter(Outlier_Flag != "Normal"),
  leverkusen_gam |> filter(Outlier_Flag != "Normal"),
  new_over,
  new_under
) |>
  distinct(Team, Season, .keep_all = TRUE) |>
  mutate(Label = paste0(Team, "\n", Season))

bg_df_gam    <- aug_gam |> filter(Outlier_Flag == "Normal")
over_df_gam  <- aug_gam |> filter(Outlier_Flag == "Overperformer")
under_df_gam <- aug_gam |> filter(Outlier_Flag == "Underperformer")

p_gam_resid <- ggplot() +

  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = underp_thr_gam, ymax = overp_thr_gam,
           fill = "#F5F5F5", alpha = 0.8) +

  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.9) +
  geom_hline(yintercept = overp_thr_gam,  linetype = "dashed", colour = "#1D6FA4", linewidth = 0.75) +
  geom_hline(yintercept = underp_thr_gam, linetype = "dashed", colour = "#C0392B", linewidth = 0.75) +

  geom_point(data = bg_df_gam, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "grey72", size = 1.5, alpha = 0.55) +

  geom_point(data = under_df_gam, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#C0392B", fill = "#C0392B", shape = 25, size = 2.2, alpha = 0.6) +

  geom_point(data = over_df_gam, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#1D6FA4", fill = "#1D6FA4", shape = 24, size = 2.4, alpha = 0.7) +

  geom_point(data = label_df_gam, aes(x = Fitted_PPG, y = Residual_PPG),
             colour = "#F39C12", fill = "#F39C12", shape = 23, size = 4.5) +

  geom_label_repel(
    data           = label_df_gam,
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

  annotate("text", x = max(aug_gam$Fitted_PPG) - 0.15, y = overp_thr_gam + 0.09,
           label = "Overperformer zone (above +1.5 SD)", colour = "#1D6FA4",
           size = 3, hjust = 1, fontface = "italic") +
  annotate("text", x = min(aug_gam$Fitted_PPG) + 0.05, y = underp_thr_gam - 0.09,
           label = "Underperformer zone (below -1.5 SD)", colour = "#C0392B",
           size = 3, hjust = 0, fontface = "italic") +

  labs(
    title    = "Residuals vs Fitted — GAM (PPG) Model",
    subtitle = sprintf(
      "N = %d  •  Residual SD = %.3f PPG (linear = 0.271, quadratic = 0.254)  •  Orange diamonds = story clubs",
      nrow(aug_gam), resid_sd_gam
    ),
    x       = "Fitted Points Per Game  (GAM prediction)",
    y       = "Residual (Actual - Predicted)\nPositive = performed BETTER than predicted",
    caption = "Model: Points_Per_Game ~ s(Financial_Rank), fit with mgcv::gam(method = \"REML\")"
  ) +
  theme_clean

path_gam_resid <- file.path(OUTPUT_DIR, "gam_ppg_residuals.png")
ggsave(path_gam_resid, p_gam_resid, width = 10, height = 7, dpi = 150)
track(path_gam_resid)
cat(sprintf("  Saved: %s\n", path_gam_resid))
cat("TASK 6 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Output Inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s) in %s/:\n", length(OUTPUT_FILES), OUTPUT_DIR))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-65s  %6.1f KB\n", f, size_kb))
}
cat("\n16_linear_gam_revision.R complete.\n")
