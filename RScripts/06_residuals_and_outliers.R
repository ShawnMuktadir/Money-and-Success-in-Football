# ==============================================================
# 06_residuals_and_outliers.R
# Residual analysis and outlier visualisations for the seminar
# presentation "Money and Success in Football".
#
# Model H  (from 05b_extended_regression.R, PHASE 6):
#   League_Position ~ Financial_Rank + I(Financial_Rank^2)
#
# Outputs (all saved to outputs/):
#   28_residual_plot_modelH.png
#   29_cooks_distance_modelH.png
#   30_champions_surprise_dotplot.png
#   31_all_champions_with_residuals.csv
# ==============================================================

# ── 0. Auto-install and load packages ─────────────────────────
source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")

load_packages(c("dplyr", "readr", "ggplot2", "broom"))

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

make_dir("outputs")

# ── Theme & colours ───────────────────────────────────────────
theme_seminar <- theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    strip.text       = element_text(face = "bold", size = 11),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

outlier_colours <- c(
  "Overperformer"  = "#2166AC",
  "Underperformer" = "#D6604D",
  "Normal"         = "grey60"
)

league_colours <- c(
  "Bundesliga"     = "#E41A1C",
  "La Liga"        = "#FF7F00",
  "Ligue 1"        = "#4DAF4A",
  "Premier League" = "#984EA3",
  "Serie A"        = "#377EB8"
)


# ══════════════════════════════════════════════════════════════
# PHASE 1 — Re-fit Model H, compute residuals, flag outliers
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Re-fit Model H & Compute Residuals ===\n")

master_path <- "data/merged/master_dataset.csv"
if (!file.exists(master_path)) {
  stop("Cannot find data/merged/master_dataset.csv. Run 04_merge_standings_and_market_values.R first.")
}

master <- read_csv(master_path, show_col_types = FALSE) |>
  mutate(
    Champion_binary = as.integer(Champion == "Yes"),
    League          = as.character(League)
  )

required_cols <- c("League", "Season", "Season_Index", "League_Position",
                   "Team", "Points", "Financial_Rank", "Market_Value_M_EUR",
                   "Normalized_Value", "Log_Normalized_Value", "Champion")
missing_cols <- setdiff(required_cols, names(master))
if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))

# Complete-case subset used for fitting
model_df <- master |>
  filter(!is.na(Financial_Rank), !is.na(League_Position))

cat(sprintf("  Complete-case rows for Model H: %d\n", nrow(model_df)))

# Fit Model H — quadratic on Financial_Rank
modH <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df)
sumH <- summary(modH)
cat(sprintf("  Model H  R² = %.4f  |  Adj-R² = %.4f  |  AIC = %.2f\n",
            sumH$r.squared, sumH$adj.r.squared, AIC(modH)))
cat(sprintf("  Coefficients: (Intercept)=%.4f, FR=%.4f, FR²=%.4f\n",
            coef(modH)[1], coef(modH)[2], coef(modH)[3]))

# Augment complete-case rows with fitted values and residuals
aug_df <- augment(modH, data = model_df) |>
  rename(Fitted_H   = .fitted,
         Residual_H = .resid,
         CooksD     = .cooksd,
         Hat        = .hat)

# Compute residual SD and outlier thresholds
resid_sd <- sd(aug_df$Residual_H, na.rm = TRUE)
overp_thr  <- -1.5 * resid_sd
underp_thr <-  1.5 * resid_sd

aug_df <- aug_df |>
  mutate(
    Outlier_Flag = case_when(
      Residual_H < overp_thr  ~ "Overperformer",
      Residual_H > underp_thr ~ "Underperformer",
      TRUE                     ~ "Normal"
    )
  )

cat(sprintf("  Residual SD      : %.4f positions\n", resid_sd))
cat(sprintf("  Overperformer threshold  : < %.4f  (< -1.5 SD)\n", overp_thr))
cat(sprintf("  Underperformer threshold : > %.4f  (> +1.5 SD)\n", underp_thr))
cat(sprintf("  Overperformers   : %d teams\n",  sum(aug_df$Outlier_Flag == "Overperformer"))  )
cat(sprintf("  Underperformers  : %d teams\n",  sum(aug_df$Outlier_Flag == "Underperformer")) )
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2 — Plot 28: Residual plot (Residuals vs Fitted)
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Residual Plot (28_residual_plot_modelH.png) ===\n")

# Label the extreme outliers for readability
label_df <- aug_df |>
  filter(Outlier_Flag != "Normal") |>
  mutate(Label = paste0(Team, "\n", Season))

p28 <- ggplot(aug_df, aes(x = Fitted_H, y = Residual_H)) +
  # SD reference bands
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = overp_thr,  ymax = underp_thr,
           fill = "grey92", alpha = 0.6) +
  geom_hline(yintercept = 0, colour = "black",   linewidth = 0.9) +
  geom_hline(yintercept = overp_thr,  linetype = "dashed", colour = "#2166AC", linewidth = 0.7) +
  geom_hline(yintercept = underp_thr, linetype = "dashed", colour = "#D6604D", linewidth = 0.7) +
  # All points
  geom_point(aes(colour = Outlier_Flag, shape = Outlier_Flag),
             alpha = 0.55, size = 2.2) +
  # Champion points on top
  geom_point(data = filter(aug_df, Champion == "Yes"),
             aes(colour = Outlier_Flag),
             shape = 18, size = 3.5) +
  # Labels for extreme outliers
  geom_text_repel(
    data    = label_df,
    aes(label = Label, colour = Outlier_Flag),
    size    = 2.8,
    segment.size = 0.35,
    segment.colour = "grey50",
    box.padding  = 0.4,
    point.padding = 0.3,
    max.overlaps  = 20,
    show.legend  = FALSE
  ) +
  scale_colour_manual(values = outlier_colours) +
  scale_shape_manual(values = c("Overperformer" = 17, "Underperformer" = 25, "Normal" = 16)) +
  annotate("text", x = max(aug_df$Fitted_H, na.rm = TRUE) * 0.6,
           y = overp_thr  - 0.4, label = "-1.5 SD (overperformer)", colour = "#2166AC", size = 3.2) +
  annotate("text", x = max(aug_df$Fitted_H, na.rm = TRUE) * 0.6,
           y = underp_thr + 0.4, label = "+1.5 SD (underperformer)", colour = "#D6604D", size = 3.2) +
  labs(
    title    = "Residuals vs Fitted — Model H (Quadratic FR)",
    subtitle = sprintf(
      "N = %d  •  Residual SD = %.2f positions  •  Diamonds = champions  •  Dashed = ±1.5 SD",
      nrow(aug_df), resid_sd
    ),
    x      = "Fitted League Position  (Model H)",
    y      = "Residual  (Actual − Predicted)\nNegative = finished BETTER than predicted",
    colour = "Outlier category",
    shape  = "Outlier category"
  ) +
  theme_seminar

path28 <- "outputs/28_residual_plot_modelH.png"
ggsave(path28, p28, width = 11, height = 7, dpi = 180)
cat(sprintf("  Saved: %s\n", path28))
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3 — Plot 29: Cook's Distance
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Cook's Distance Plot (29_cooks_distance_modelH.png) ===\n")

n_obs      <- nrow(aug_df)
cooks_thr  <- 4 / n_obs          # conventional threshold

aug_df <- aug_df |>
  mutate(Obs_Index = row_number(),
         High_Influence = CooksD > cooks_thr)

high_inf_df <- aug_df |>
  filter(High_Influence) |>
  mutate(Label = paste0(Team, "\n", Season))

p29 <- ggplot(aug_df, aes(x = Obs_Index, y = CooksD)) +
  geom_hline(yintercept = cooks_thr, linetype = "dashed", colour = "firebrick",
             linewidth = 0.8) +
  geom_segment(aes(xend = Obs_Index, yend = 0, colour = High_Influence),
               linewidth = 0.5, alpha = 0.6) +
  geom_point(aes(colour = High_Influence), size = 1.8, alpha = 0.75) +
  geom_text_repel(
    data    = high_inf_df,
    aes(label = Label),
    size    = 2.6,
    colour  = "firebrick",
    segment.size   = 0.3,
    segment.colour = "grey50",
    box.padding    = 0.4,
    point.padding  = 0.3,
    max.overlaps   = 25,
    show.legend    = FALSE
  ) +
  annotate("text",
           x     = n_obs * 0.85,
           y     = cooks_thr + max(aug_df$CooksD, na.rm = TRUE) * 0.03,
           label = sprintf("Threshold = 4/n = %.4f", cooks_thr),
           colour = "firebrick", size = 3.2, hjust = 1) +
  scale_colour_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey55"),
                      labels = c("TRUE" = "High influence (> 4/n)", "FALSE" = "Normal")) +
  labs(
    title    = "Cook's Distance — Model H (Quadratic FR)",
    subtitle = sprintf("N = %d  •  High-influence observations (Cook's D > 4/n = %.4f) labelled",
                       n_obs, cooks_thr),
    x      = "Observation index",
    y      = "Cook's Distance",
    colour = NULL
  ) +
  theme_seminar

path29 <- "outputs/29_cooks_distance_modelH.png"
ggsave(path29, p29, width = 12, height = 6, dpi = 180)
cat(sprintf("  Saved: %s\n", path29))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4 — Plot 30: Champions Surprise Dotplot
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Champions Surprise Dotplot (30_champions_surprise_dotplot.png) ===\n")

champ_df <- aug_df |>
  filter(Champion == "Yes") |>
  arrange(Residual_H) |>
  mutate(
    Label      = paste0(Team, "  (", Season, ")"),
    Is_Outlier = Outlier_Flag == "Overperformer"
  )

# A horizontal lollipop: y = champion label ordered by residual, x = Residual_H
# Reverse order so most negative (biggest overperformer) is at top
champ_df <- champ_df |>
  mutate(Label_Ordered = factor(Label, levels = rev(Label)))

p30 <- ggplot(champ_df, aes(x = Residual_H, y = Label_Ordered)) +
  geom_vline(xintercept = 0,       colour = "black",   linewidth = 0.9) +
  geom_vline(xintercept = overp_thr, linetype = "dashed", colour = "#2166AC", linewidth = 0.7) +
  geom_segment(aes(x = 0, xend = Residual_H, yend = Label_Ordered, colour = League),
               linewidth = 0.8, alpha = 0.7) +
  geom_point(aes(colour = League, shape = Is_Outlier), size = 3.2) +
  geom_text_repel(
    aes(label = sprintf("FR %d", Financial_Rank), colour = League),
    size          = 2.8,
    nudge_y       = 0.35,
    segment.size  = 0.25,
    segment.colour = "grey60",
    max.overlaps  = 30,
    show.legend   = FALSE
  ) +
  scale_colour_manual(values = league_colours) +
  scale_shape_manual(values  = c("TRUE" = 18, "FALSE" = 16),
                     labels  = c("TRUE" = "Overperformer (< -1.5 SD)", "FALSE" = "Within ±1.5 SD")) +
  annotate("text",
           x = overp_thr - 0.1, y = 1,
           label = "-1.5 SD", colour = "#2166AC",
           hjust = 1, size = 3, fontface = "italic") +
  labs(
    title    = "Champions' Residuals vs Model H Prediction",
    subtitle = "Negative residual = won despite finishing BETTER than their financial rank predicted\nAnnotations show Financial Rank (FR) of each champion",
    x        = "Residual  (Actual Position − Predicted Position)\n← Overperformed (finished higher)    Underperformed (finished lower) →",
    y        = NULL,
    colour   = "League",
    shape    = "Outlier status"
  ) +
  theme_seminar +
  theme(axis.text.y = element_text(size = 8))

path30 <- "outputs/30_champions_surprise_dotplot.png"
ggsave(path30, p30, width = 13, height = max(8, nrow(champ_df) * 0.42), dpi = 180)
cat(sprintf("  Saved: %s  (%d champions plotted)\n", path30, nrow(champ_df)))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5 — CSV 31: All champions with residuals
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: All Champions with Residuals CSV (31_all_champions_with_residuals.csv) ===\n")

champ_csv <- aug_df |>
  filter(Champion == "Yes") |>
  select(League, Season, Team, Points,
         Financial_Rank, Market_Value_M_EUR,
         Normalized_Value, Fitted_H, Residual_H,
         Outlier_Flag, Champion) |>
  arrange(Residual_H)   # ascending: biggest overperformers (most negative) first

path31 <- "outputs/31_all_champions_with_residuals.csv"
write_csv(champ_csv, path31)
cat(sprintf("  Saved: %s  (%d rows)\n", path31, nrow(champ_csv)))

cat("\n=== Top 10 Biggest Overperformers Among Champions ===\n")
print(as.data.frame(head(champ_csv, 10)), digits = 4)
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6 — Final Summary to Console
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Final Summary ===\n\n")

n_total       <- nrow(aug_df)
n_overp       <- sum(aug_df$Outlier_Flag == "Overperformer")
n_underp      <- sum(aug_df$Outlier_Flag == "Underperformer")

# Key outlier champions specified in the brief
key_teams <- list(
  list(team = "Leicester City",   season = "2015-2016"),
  list(team = "Bayer Leverkusen", season = "2023-2024"),
  list(team = "Lille",            season = "2020-2021"),
  list(team = "Napoli",           season = "2024-2025"),
  list(team = "Liverpool",        season = "2024-2025")
)

key_rows <- lapply(key_teams, function(kt) {
  row <- aug_df |>
    filter(grepl(kt$team, Team, ignore.case = TRUE), Season == kt$season)
  if (nrow(row) == 0) return(NULL)
  row[1, ]
}) |>
  bind_rows()

# PASS/FAIL checks: these teams should be overperformers (Residual_H < 0)
leicester_pass <- tryCatch({
  r <- aug_df |> filter(grepl("Leicester", Team, ignore.case = TRUE), Season == "2015-2016")
  if (nrow(r) == 0) FALSE else r$Residual_H[1] < 0
}, error = function(e) FALSE)

leverkusen_pass <- tryCatch({
  r <- aug_df |> filter(grepl("Leverkusen", Team, ignore.case = TRUE), Season == "2023-2024")
  if (nrow(r) == 0) FALSE else r$Residual_H[1] < 0
}, error = function(e) FALSE)

# ── Print summary ─────────────────────────────────────────────
cat("══════════════════════════════════════════════════\n")
cat("RESIDUAL ANALYSIS SUMMARY — Model H\n")
cat("  League_Position ~ Financial_Rank + I(Financial_Rank^2)\n")
cat("══════════════════════════════════════════════════\n")
cat(sprintf("  Total observations       : %d\n", n_total))
cat(sprintf("  Residual SD              : %.2f positions\n", resid_sd))
cat(sprintf("  Overperformers (< -1.5SD): %d teams\n",  n_overp))
cat(sprintf("  Underperformers (> +1.5SD): %d teams\n", n_underp))
cat("\n")

if (nrow(key_rows) > 0) {
  cat("  KEY OUTLIER CHAMPIONS (finished better than predicted):\n")
  cat("  ┌──────────────────────┬──────────┬─────┬───────────┬───────────┐\n")
  cat("  │ Team                 │ Season   │ FR  │ Predicted │ Residual  │\n")
  cat("  ├──────────────────────┼──────────┼─────┼───────────┼───────────┤\n")
  for (i in seq_len(nrow(key_rows))) {
    r <- key_rows[i, ]
    cat(sprintf("  │ %-20s │ %-8s │ %3d │ %9.1f │ %9.1f │\n",
                r$Team, r$Season, r$Financial_Rank, r$Fitted_H, r$Residual_H))
  }
  cat("  └──────────────────────┴──────────┴─────┴───────────┴───────────┘\n")
}

cat("\n  PASS/FAIL checks:\n")
cat(sprintf("  Leicester City 2015-16 in overperformers (Residual < 0): %s\n",
            ifelse(leicester_pass,  "PASS", "FAIL")))
cat(sprintf("  Bayer Leverkusen 2023-24 in overperformers (Residual < 0): %s\n",
            ifelse(leverkusen_pass, "PASS", "FAIL")))

cat("\n  Output files saved:\n")
cat(sprintf("    %s\n", path28))
cat(sprintf("    %s\n", path29))
cat(sprintf("    %s\n", path30))
cat(sprintf("    %s\n", path31))
cat("══════════════════════════════════════════════════\n")
# ══════════════════════════════════════════════════════════════
# PHASE 7 — Plot 35: Predicted vs Actual Dumbbell Chart
# Shows Model H predictions against actual finishes for five
# manually chosen notable overperformers plus 2-3 well-predicted
# teams selected automatically as a contrast.
# Output: outputs/35_predicted_vs_actual_dumbbell.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 7: Predicted vs Actual Dumbbell Chart (35_predicted_vs_actual_dumbbell.png) ===\n")

# ── 7a. Focal team-seasons (manually specified) ───────────────
focal_specs <- tibble(
  Team   = c("Leicester City", "Napoli",    "Liverpool",  "Lille",      "Bayer Leverkusen"),
  Season = c("2015-2016",      "2024-2025", "2024-2025",  "2020-2021",  "2023-2024")
)

focal_df <- focal_specs |>
  left_join(
    aug_df |> select(Team, Season, League, Financial_Rank,
                     League_Position, Fitted_H, Residual_H),
    by = c("Team", "Season")
  ) |>
  filter(!is.na(League_Position))

cat(sprintf("  Focal teams matched in dataset: %d of %d\n",
            nrow(focal_df), nrow(focal_specs)))

# ── 7b. Well-predicted contrast: |residual| < 1.0, FR >= 2 ───
contrast_df <- aug_df |>
  anti_join(focal_specs, by = c("Team", "Season")) |>
  filter(abs(Residual_H) < 1.0, Financial_Rank >= 2) |>
  arrange(abs(Residual_H)) |>
  group_by(League) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(abs(Residual_H)) |>
  slice_head(n = 3) |>
  select(Team, Season, League, Financial_Rank,
         League_Position, Fitted_H, Residual_H)

cat(sprintf("  Contrast teams selected: %d\n", nrow(contrast_df)))
if (nrow(contrast_df) > 0)
  print(as.data.frame(
    contrast_df[, c("Team", "Season", "League", "Financial_Rank",
                    "League_Position", "Fitted_H", "Residual_H")]
  ), digits = 3)

# ── 7c. Combine, classify, build x labels ────────────────────
n_focal    <- nrow(focal_df)
n_contrast <- nrow(contrast_df)

dumb_df <- bind_rows(
  focal_df    |> mutate(Group = "Featured overperformers"),
  contrast_df |> mutate(Group = "Well-predicted contrast")
) |>
  mutate(
    Direction = case_when(
      abs(Residual_H) < 1.0 ~ "Well predicted",
      Residual_H < 0        ~ "Outperformed",
      TRUE                   ~ "Underperformed"
    ),
    x_label = paste0(Team, "\n", Season, "  [FR ", Financial_Rank, "]"),
    # Y-nudge for focal labels: push text away from arrowhead
    pnudge = if_else(Fitted_H >= League_Position,  0.55, -0.55),
    anudge = if_else(League_Position >  Fitted_H,  0.55, -0.55)
  )

# Factor order: focal most -> least overperformance, then contrast
focal_order <- focal_df |>
  arrange(Residual_H) |>
  mutate(x_label = paste0(Team, "\n", Season, "  [FR ", Financial_Rank, "]")) |>
  pull(x_label)

contrast_order <- contrast_df |>
  arrange(abs(Residual_H)) |>
  mutate(x_label = paste0(Team, "\n", Season, "  [FR ", Financial_Rank, "]")) |>
  pull(x_label)

dumb_df <- dumb_df |>
  mutate(x_label = factor(x_label, levels = c(focal_order, contrast_order)))

# Split data for different rendering strategies
focal_plot    <- filter(dumb_df, Direction != "Well predicted")
contrast_plot <- filter(dumb_df, Direction == "Well predicted")

# ── 7d. Colours ───────────────────────────────────────────────
dir_colours <- c(
  "Outperformed"   = "#2166AC",
  "Underperformed" = "#D6604D",
  "Well predicted" = "grey45"
)
dir_labels <- c(
  "Outperformed"   = "Outperformed: actual finish better than Model H prediction",
  "Underperformed" = "Underperformed: actual finish worse than Model H prediction",
  "Well predicted" = "Well predicted: predicted = actual (residual < 0.01)"
)

sep_x <- if (n_contrast > 0L) n_focal + 0.5 else NULL

# ── 7e. Build the plot ────────────────────────────────────────
p35 <- ggplot(dumb_df, aes(x = x_label)) +

  # Dashed vertical separator between the two groups
  {if (!is.null(sep_x))
    geom_vline(xintercept = sep_x, linetype = "dashed",
               colour = "grey72", linewidth = 0.55)
  else NULL} +

  # FOCAL TEAMS: arrow from predicted to actual ─────────────────
  geom_segment(
    data = focal_plot,
    aes(y = Fitted_H, yend = League_Position,
        xend = x_label, colour = Direction),
    linewidth   = 1.15,
    arrow = arrow(length = unit(0.23, "cm"), type = "closed", ends = "last"),
    show.legend = FALSE
  ) +
  # Open diamond = predicted position
  geom_point(
    data  = focal_plot,
    aes(y = Fitted_H, colour = Direction),
    shape = 23, fill = "white", size = 4.2, stroke = 1.3
  ) +
  # Filled circle = actual position
  geom_point(
    data  = focal_plot,
    aes(y = League_Position, colour = Direction),
    shape = 16, size = 4.7
  ) +
  # Predicted value (italic label, nudged clear of arrowhead)
  geom_text(
    data = focal_plot,
    aes(y     = Fitted_H + pnudge,
        label = sprintf("pred. %.1f", Fitted_H),
        colour = Direction),
    size = 2.55, fontface = "italic", hjust = 0.5, show.legend = FALSE
  ) +
  # Actual value (bold label, nudged clear of circle)
  geom_text(
    data = focal_plot,
    aes(y     = League_Position + anudge,
        label = paste0("actual ", as.integer(round(League_Position))),
        colour = Direction),
    size = 2.55, fontface = "bold", hjust = 0.5, show.legend = FALSE
  ) +

  # CONTRAST TEAMS: bullseye = large open diamond enclosing small circle ──
  # Large open diamond (outer ring)
  geom_point(
    data = contrast_plot,
    aes(y = League_Position),
    shape = 23, fill = "white", colour = "grey45",
    size = 7.0, stroke = 1.4
  ) +
  # Small filled circle (inner dot)
  geom_point(
    data = contrast_plot,
    aes(y = League_Position),
    shape = 16, colour = "grey45", size = 3.2
  ) +
  # Single combined label above: "pred. 13 ~ actual 13"
  geom_text(
    data = contrast_plot,
    aes(y     = League_Position - 1.1,
        label = sprintf("pred. %.0f ~ actual %.0f", Fitted_H, League_Position)),
    colour = "grey35", size = 2.55, fontface = "italic",
    hjust = 0.5, show.legend = FALSE
  ) +

  # Group header annotations near the top of the chart
  annotate("text",
           x = (n_focal + 1L) / 2, y = -1.3,
           label = "Notable overperformers",
           colour = "#2166AC", size = 3.15, fontface = "bold.italic",
           hjust = 0.5) +
  {if (n_contrast > 0L)
    annotate("text",
             x      = n_focal + (n_contrast + 1L) / 2, y = -1.3,
             label  = "Well-predicted (model correct)",
             colour = "grey38", size = 3.15, fontface = "bold.italic",
             hjust  = 0.5)
  else NULL} +

  # Y-axis trimmed to position 16: removes empty space below contrast teams
  scale_y_reverse(
    limits       = c(-2.5, 16),
    breaks       = c(1, 3, 5, 8, 11, 14),
    minor_breaks = NULL,
    name         = "League Position  (1 = Champion)"
  ) +
  scale_x_discrete(expand = expansion(add = c(0.55, 0.55))) +
  scale_colour_manual(values = dir_colours, labels = dir_labels, name = NULL) +

  labs(
    title    = "Predicted vs Actual League Position: Selected Team-Seasons (Model H)",
    subtitle = paste0(
      "Diamond = Model H predicted position (FR + FR^2)",
      "  ·  Circle = actual final league position",
      "  ·  Bullseye = predicted matches actual"
    ),
    x        = NULL,
    caption  = paste0(
      "Upward arrows (toward position 1): team finished HIGHER than financial rank predicted",
      " - beat model expectations.\n",
      "Bullseye symbol: predicted and actual coincide",
      " - model was essentially correct for this team.\n",
      "FR = Financial Rank (1 = richest squad in league that season)",
      "  |  Model H: R^2 = 0.671, N = 975 team-seasons"
    )
  ) +

  theme_seminar +
  theme(
    axis.text.x        = element_text(size = 8.2, lineheight = 1.25),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey87", linewidth = 0.45),
    legend.position    = "top",
    legend.text        = element_text(size = 8.8),
    legend.key.width   = unit(1.6, "cm"),
    plot.caption       = element_text(size = 7.8, colour = "grey35",
                                      lineheight = 1.3, hjust = 0),
    plot.subtitle      = element_text(size = 9.5, colour = "grey30")
  )

path35 <- "outputs/35_predicted_vs_actual_dumbbell.png"
ggsave(path35, p35, width = 14, height = 7.5, dpi = 180)
cat(sprintf("  Saved: %s\n", path35))
cat("PHASE 7 complete.\n\n")

cat("\n06_residuals_and_outliers.R complete.\n")