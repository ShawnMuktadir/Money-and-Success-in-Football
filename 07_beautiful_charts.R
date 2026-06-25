# ==============================================================
# 07_beautiful_charts.R
# Clean, presentation-ready versions of plots 28-30 PLUS three
# new presentation charts (32, 33, 34) used in the final slides.
# Outputs (all saved to outputs/):
#   28b_residual_plot_clean.png
#   29b_cooks_distance_clean.png
#   30b_champions_dotplot_clean.png
#   32_league_r2_bars.png
#   33_scatter_story.png
#   34_champions_fr_dist.png
# ==============================================================

# ── 0. Utils & packages ────────────────────────────────────────
source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")

load_packages(c("dplyr", "readr", "ggplot2", "broom", "tidyr", "scales"))

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
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }


# ── Shared presentation theme ──────────────────────────────────
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 14,
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 10.5, colour = "grey45",
                                     margin = margin(b = 12)),
    plot.caption      = element_text(size = 8.5, colour = "grey55",
                                     hjust = 0, margin = margin(t = 10)),
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

# Project-standard league palette (consistent with 05–06 scripts)
league_colours <- c(
  "Bundesliga"     = "#E41A1C",
  "La Liga"        = "#FF7F00",
  "Ligue 1"        = "#4DAF4A",
  "Premier League" = "#984EA3",
  "Serie A"        = "#377EB8"
)


# ══════════════════════════════════════════════════════════════
# PHASE 1 — Load data, re-fit Model H, compute residuals
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load Data & Re-fit Model H ===\n")

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

model_df <- master |>
  filter(!is.na(Financial_Rank), !is.na(League_Position))

cat(sprintf("  Complete-case rows for Model H: %d\n", nrow(model_df)))

modH <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2),
           data = model_df)

aug_df <- augment(modH, data = model_df) |>
  rename(Fitted_H   = .fitted,
         Residual_H = .resid,
         CooksD     = .cooksd)

resid_sd   <- sd(aug_df$Residual_H, na.rm = TRUE)
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

cat(sprintf("  Residual SD: %.2f positions  |  Overp. threshold: < %.2f  |  Underp. threshold: > %.2f\n",
            resid_sd, overp_thr, underp_thr))
cat(sprintf("  Overperformers: %d  |  Underperformers: %d\n",
            sum(aug_df$Outlier_Flag == "Overperformer"),
            sum(aug_df$Outlier_Flag == "Underperformer")))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2 — Plot 28b: Clean Residuals vs Fitted
#           Only label the 5 key story clubs
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Clean Residual Plot (28b_residual_plot_clean.png) ===\n")

label_df28 <- aug_df |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024") |
    (grepl("Lille",      Team) & Season == "2020-2021") |
    (grepl("Napoli",     Team) & Season == "2024-2025") |
    (grepl("Liverpool",  Team) & Season == "2024-2025" & Champion == "Yes")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

bg_df    <- aug_df |> filter(Outlier_Flag == "Normal")
over_df  <- aug_df |> filter(Outlier_Flag == "Overperformer")
under_df <- aug_df |> filter(Outlier_Flag == "Underperformer")

p28 <- ggplot() +

  # Shaded ±1.5 SD band
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = overp_thr, ymax = underp_thr,
           fill = "#F5F5F5", alpha = 0.8) +

  # Reference lines
  geom_hline(yintercept = 0,
             colour = "grey20", linewidth = 0.9) +
  geom_hline(yintercept = overp_thr,
             linetype = "dashed", colour = "#1D6FA4",
             linewidth = 0.75) +
  geom_hline(yintercept = underp_thr,
             linetype = "dashed", colour = "#C0392B",
             linewidth = 0.75) +

  # Normal points — small, grey
  geom_point(data = bg_df,
             aes(x = Fitted_H, y = Residual_H),
             colour = "grey72", size = 1.5, alpha = 0.55) +

  # Underperformers — red downward triangles
  geom_point(data = under_df,
             aes(x = Fitted_H, y = Residual_H),
             colour = "#C0392B", fill = "#C0392B",
             shape = 25, size = 2.2, alpha = 0.6) +

  # Overperformers — blue upward triangles (better than predicted)
  geom_point(data = over_df,
             aes(x = Fitted_H, y = Residual_H),
             colour = "#1D6FA4", fill = "#1D6FA4",
             shape = 24, size = 2.4, alpha = 0.7) +

  # Story clubs — large orange diamonds
  geom_point(data = label_df28,
             aes(x = Fitted_H, y = Residual_H),
             colour = "#F39C12", fill = "#F39C12",
             shape = 23, size = 4.5) +

  # Labels — 5 key clubs only
  geom_label_repel(
    data           = label_df28,
    aes(x = Fitted_H, y = Residual_H, label = Label),
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

  # Band annotations
  annotate("text",
           x = 1.5, y = overp_thr - 0.5,
           label = "← Overperformer zone (below −1.5 SD)",
           colour = "#1D6FA4", size = 3, hjust = 0, fontface = "italic") +
  annotate("text",
           x = 1.5, y = underp_thr + 0.5,
           label = "Underperformer zone (above +1.5 SD) →",
           colour = "#C0392B", size = 3, hjust = 0, fontface = "italic") +

  scale_x_continuous(breaks = seq(2, 18, by = 2)) +
  scale_y_continuous(breaks = seq(-12, 14, by = 2)) +

  labs(
    title    = "Residuals vs Fitted — Model H",
    subtitle = sprintf(
      "N = %d  •  Residual SD = %.2f positions  •  Orange diamonds = key story clubs",
      nrow(aug_df), resid_sd
    ),
    x       = "Fitted League Position  (Model H prediction)",
    y       = "Residual (Actual − Predicted)\nNegative = finished BETTER than predicted",
    caption = "Model H: League_Position ~ Financial_Rank + Financial_Rank²"
  ) +
  theme_clean

path28b <- "outputs/28b_residual_plot_clean.png"
ggsave(path28b, p28, width = 11, height = 7, dpi = 200)
track(path28b)
cat(sprintf("  Saved: %s\n", path28b))
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3 — Plot 29b: Clean Cook's Distance
#           Label only top influential observations
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Clean Cook's Distance Plot (29b_cooks_distance_clean.png) ===\n")

n_obs     <- nrow(aug_df)
cooks_thr <- 4 / n_obs

aug_df <- aug_df |>
  mutate(
    Obs_Index      = row_number(),
    High_Influence = CooksD > cooks_thr,
    Top_Influence  = CooksD > quantile(CooksD, 0.993, na.rm = TRUE)
  )

top_label_df <- aug_df |>
  filter(Top_Influence) |>
  mutate(Label = paste0(Team, "\n", Season))

story_cook <- aug_df |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

p29 <- ggplot(aug_df, aes(x = Obs_Index, y = CooksD)) +

  # Threshold line
  geom_hline(yintercept = cooks_thr,
             linetype = "dashed", colour = "#C0392B",
             linewidth = 0.8) +

  # All spikes — normal grey, high-influence red
  geom_segment(aes(xend = Obs_Index, yend = 0,
                   colour = High_Influence),
               linewidth = 0.4, alpha = 0.5) +
  geom_point(aes(colour = High_Influence,
                 size   = High_Influence),
             alpha = 0.8) +

  # Story clubs — orange diamond on top
  geom_point(data = story_cook,
             aes(x = Obs_Index, y = CooksD),
             colour = "#F39C12", fill = "#F39C12",
             shape = 23, size = 4) +

  # Labels — top influential observations
  geom_label_repel(
    data           = top_label_df,
    aes(x = Obs_Index, y = CooksD, label = Label),
    fill           = "white",
    colour         = "#2C3E50",
    size           = 2.8,
    fontface       = "bold",
    label.size     = 0.25,
    label.r        = unit(0.2, "lines"),
    box.padding    = 0.5,
    point.padding  = 0.3,
    segment.colour = "grey50",
    segment.size   = 0.4,
    max.overlaps   = 20
  ) +

  # Story club labels
  geom_label_repel(
    data           = story_cook,
    aes(x = Obs_Index, y = CooksD, label = Label),
    fill           = "#FEF9E7",
    colour         = "#F39C12",
    size           = 3,
    fontface       = "bold",
    label.size     = 0.3,
    box.padding    = 0.6,
    segment.colour = "#F39C12",
    segment.size   = 0.5,
    max.overlaps   = 20
  ) +

  annotate("text",
           x     = n_obs * 0.72,
           y     = cooks_thr * 1.15,
           label = sprintf("Influence threshold = 4/n = %.4f", cooks_thr),
           colour = "#C0392B", size = 3.2, hjust = 0, fontface = "italic") +

  scale_colour_manual(
    values = c("TRUE"  = "#C0392B", "FALSE" = "grey65"),
    labels = c("TRUE"  = paste0("High influence (Cook's D > ", round(cooks_thr, 4), ")"),
               "FALSE" = "Normal")
  ) +
  scale_size_manual(values = c("TRUE" = 2.2, "FALSE" = 1.2), guide = "none") +

  labs(
    title    = "Cook's Distance — Model H",
    subtitle = sprintf(
      "N = %d  •  Red = high-influence observations (above threshold)  •  Orange = key story clubs",
      n_obs
    ),
    x       = "Observation index",
    y       = "Cook's Distance",
    colour  = NULL,
    caption = "Threshold = 4/n. High-influence points may pull the regression line disproportionately."
  ) +
  theme_clean

path29b <- "outputs/29b_cooks_distance_clean.png"
ggsave(path29b, p29, width = 13, height = 6, dpi = 200)
track(path29b)
cat(sprintf("  Saved: %s\n", path29b))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4 — Plot 30b: Clean Champions Dotplot
#           Horizontal lollipop, clear story
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Clean Champions Dotplot (30b_champions_dotplot_clean.png) ===\n")

champ_df <- aug_df |>
  filter(Champion == "Yes") |>
  arrange(Residual_H) |>
  mutate(
    Label         = paste0(Team, "  (", Season, ")"),
    Label_Ordered = factor(Label, levels = rev(Label)),
    Is_Surprise   = Financial_Rank > 3,
    FR_Label      = paste0("FR ", Financial_Rank)
  )

p30 <- ggplot(champ_df, aes(x = Residual_H, y = Label_Ordered)) +

  # Zero line
  geom_vline(xintercept = 0,
             colour = "grey30", linewidth = 0.9) +

  # Overperformer threshold line
  geom_vline(xintercept = overp_thr,
             linetype = "dashed", colour = "#1D6FA4",
             linewidth = 0.7) +

  # Lollipop stems — thicker for surprise winners
  geom_segment(
    aes(x = 0, xend = Residual_H,
        yend = Label_Ordered,
        colour    = League,
        linewidth = Is_Surprise),
    alpha = 0.7
  ) +

  # Points — surprise winners are larger diamonds
  geom_point(
    aes(colour = League,
        shape  = Is_Surprise,
        size   = Is_Surprise)
  ) +

  # FR annotations for surprise winners (positioned left of point)
  geom_text(
    data     = filter(champ_df, Is_Surprise),
    aes(label = FR_Label, colour = League),
    hjust    = 1.35,
    size     = 3.0,
    fontface = "bold"
  ) +

  # Threshold annotation
  annotate("text",
           x = overp_thr - 0.2, y = 2.5,
           label = "Statistical\noverperformer\nthreshold\n(−1.5 SD)",
           colour = "#1D6FA4", size = 2.8, hjust = 1, fontface = "italic") +

  # Surprise zone shading
  annotate("rect",
           xmin = min(champ_df$Residual_H) - 0.5,
           xmax = overp_thr,
           ymin = 0.5,
           ymax = length(levels(champ_df$Label_Ordered)) + 0.5,
           fill = "#EAF4FB", alpha = 0.4) +

  scale_colour_manual(values = league_colours) +
  scale_shape_manual(
    values = c("TRUE" = 18, "FALSE" = 16),
    labels = c("TRUE"  = "Financial Rank > 3 (surprise winner)",
               "FALSE" = "Financial Rank ≤ 3 (expected contender)")
  ) +
  scale_size_manual(
    values = c("TRUE" = 5.5, "FALSE" = 2.8),
    guide  = "none"
  ) +
  scale_linewidth_manual(
    values = c("TRUE" = 1.4, "FALSE" = 0.7),
    guide  = "none"
  ) +

  labs(
    title    = "Champions' Residuals vs Model H Prediction",
    subtitle = paste0(
      "How far did each champion finish above their predicted position?",
      "  •  Annotations show Financial Rank (FR) of surprise winners"
    ),
    x = paste0(
      "Residual  (Actual Position − Predicted Position)\n",
      "← Finished HIGHER than predicted          ",
      "Finished LOWER than predicted →"
    ),
    y       = NULL,
    colour  = "League",
    shape   = "Champion type",
    caption = sprintf(
      "Model H: League_Position ~ Financial_Rank + Financial_Rank²  •  Blue shading = statistical overperformer zone (residual < −%.2f positions = −1.5 SD)",
      abs(overp_thr)
    )
  ) +

  theme_clean +
  theme(
    axis.text.y     = element_text(size = 8.5, colour = "grey30"),
    legend.key.size = unit(1.1, "lines")
  )

h <- max(10, nrow(champ_df) * 0.38)
path30b <- "outputs/30b_champions_dotplot_clean.png"
ggsave(path30b, p30, width = 13, height = h, dpi = 200)
track(path30b)
cat(sprintf("  Saved: %s  (%d champions plotted)\n", path30b, nrow(champ_df)))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5 — Plot 32: League-Level R² Comparison (bar chart)
# Clear, journalist-readable "How much does money explain per
# league?" — replaces raw correlation tables from 05 scripts.
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: League R² Bars (32_league_r2_bars.png) ===\n")

league_r2_raw <- read_csv("outputs/10_league_regression_breakdown.csv",
                           show_col_types = FALSE)

league_r2 <- league_r2_raw |>
  select(League, R2_FR, R2_LogMV) |>
  pivot_longer(cols      = c(R2_FR, R2_LogMV),
               names_to  = "Predictor",
               values_to = "R2") |>
  mutate(
    Predictor = recode(Predictor,
                       "R2_FR"    = "Financial Rank",
                       "R2_LogMV" = "Log(Market Value)"),
    League = factor(League,
                    levels = c("Serie A", "Premier League",
                               "Ligue 1", "La Liga", "Bundesliga"))
  )

p32 <- ggplot(league_r2, aes(x = R2, y = League, fill = Predictor)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.60, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.0f%%", R2 * 100)),
            position    = position_dodge(width = 0.7),
            hjust       = -0.15,
            size        = 3.5,
            fontface    = "bold",
            colour      = "grey25") +
  geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.5) +
  scale_x_continuous(
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.16))
  ) +
  scale_fill_manual(values = c("Financial Rank"    = "#2C3E73",
                                "Log(Market Value)" = "#27AE60")) +
  labs(
    title    = "Money Explains 61–79% of Final League Position",
    subtitle = expression(paste(
      "R"^2, " per league  •  Both predictors significant (p < 0.001) in all leagues"
    )),
    x       = expression(paste(R^2, "  (share of variation in final standing explained)")),
    y       = NULL,
    fill    = "Predictor",
    caption = paste0(
      "OLS: League_Position ∼ predictor, estimated separately per league.  ",
      "Serie A shows the strongest relationship (R² = 0.79)."
    )
  ) +
  theme_clean +
  theme(panel.grid.major.y = element_blank())

path32 <- "outputs/32_league_r2_bars.png"
ggsave(path32, p32, width = 10, height = 5.5, dpi = 200)
track(path32)
cat(sprintf("  Saved: %s\n", path32))
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6 — Plot 33: Annotated Main Scatter (story clubs)
# The core result visualised at journalist level: the scatter
# with the 5 key clubs labelled as gold diamonds.
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Annotated Scatter (33_scatter_story.png) ===\n")

# Quadratic fit ribbon from Model H
fr_seq   <- seq(1, max(aug_df$Financial_Rank, na.rm = TRUE), length.out = 300)
quad_pred <- predict(modH,
                     newdata  = data.frame(Financial_Rank = fr_seq),
                     interval = "confidence",
                     level    = 0.95) |>
  as.data.frame() |>
  mutate(Financial_Rank = fr_seq)

# Position axis labels that speak to fans, not statisticians
pos_labels <- function(x) {
  dplyr::case_when(
    x == 1  ~ "1st — Champion",
    x == 5  ~ "5th",
    x == 10 ~ "10th",
    x == 15 ~ "15th",
    x == 20 ~ "20th",
    TRUE    ~ as.character(x)
  )
}

p33 <- ggplot() +
  # 95% CI ribbon
  geom_ribbon(data  = quad_pred,
              aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill  = "#2C3E73", alpha = 0.12) +
  # Background scatter — all teams, very light
  geom_jitter(data   = aug_df,
              aes(x  = Financial_Rank, y = League_Position,
                  colour = League),
              alpha  = 0.28, size = 1.2,
              width  = 0.18, height = 0.18) +
  # Quadratic fit line
  geom_line(data  = quad_pred,
            aes(x = Financial_Rank, y = fit),
            colour    = "#2C3E73", linewidth = 1.4) +
  # Story clubs — gold diamonds on top
  geom_point(data   = label_df28,
             aes(x  = Fitted_H + Residual_H,        # = actual Financial_Rank
                 y  = League_Position),
             colour = "#F39C12", fill = "#F39C12",
             shape  = 23, size = 5, stroke = 0.8) +
  geom_label_repel(
    data           = label_df28,
    aes(x          = Fitted_H + Residual_H,
        y          = League_Position,
        label      = Label),
    fill           = "white",
    colour         = "#2C3E50",
    size           = 3.1,
    fontface       = "bold",
    label.size     = 0.3,
    label.r        = unit(0.2, "lines"),
    box.padding    = 0.55,
    point.padding  = 0.4,
    segment.colour = "#F39C12",
    segment.size   = 0.55,
    min.segment.length = 0.2,
    max.overlaps   = 20
  ) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20),
                     labels = function(x) paste0("FR ", x)) +
  scale_y_continuous(breaks = c(1, 5, 10, 15, 20),
                     labels = pos_labels,
                     trans  = "reverse") +
  labs(
    title    = "Richer Squads Finish Higher — With Notable Exceptions",
    subtitle = sprintf(
      "N = %d team-seasons  •  Pearson r = 0.81, p < 0.001  •  Model H quadratic fit  •  Gold = key story clubs",
      nrow(aug_df)
    ),
    x       = "Financial Rank within league-season  (FR 1 = richest squad)",
    y       = "Final league position",
    colour  = "League",
    caption = "Financial Rank normalised within each league-season. Axis reversed: top = better."
  ) +
  theme_clean

path33 <- "outputs/33_scatter_story.png"
ggsave(path33, p33, width = 12, height = 7.5, dpi = 200)
track(path33)
cat(sprintf("  Saved: %s\n", path33))
cat("PHASE 6 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 7 — Plot 34: Financial Rank distribution of all champions
# A single bar chart that instantly shows how dominant FR-1/2
# teams are — and how rare the exceptions are.
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 7: Champion FR Distribution (34_champions_fr_dist.png) ===\n")

champ_csv_raw <- read_csv("outputs/31_all_champions_with_residuals.csv",
                           show_col_types = FALSE)

champ_fr <- champ_csv_raw |>
  mutate(
    FR_Group = case_when(
      Financial_Rank == 1          ~ "FR 1\n(richest)",
      Financial_Rank == 2          ~ "FR 2",
      Financial_Rank == 3          ~ "FR 3",
      Financial_Rank %in% c(4, 5)  ~ "FR 4–5",
      TRUE                          ~ "FR 6+"
    ),
    FR_Group = factor(FR_Group,
                      levels = c("FR 1\n(richest)", "FR 2", "FR 3",
                                 "FR 4–5", "FR 6+")),
    Surprise = Financial_Rank > 3
  ) |>
  count(FR_Group, Surprise) |>
  mutate(Pct = round(n / sum(n) * 100, 0))

# Colours: richer = deeper blue; surprises = orange/red
fr_cols <- c(
  "FR 1\n(richest)" = "#1A5276",
  "FR 2"            = "#2E86C1",
  "FR 3"            = "#7FB3D3",
  "FR 4–5"     = "#F39C12",
  "FR 6+"           = "#C0392B"
)

p34 <- ggplot(champ_fr, aes(x = FR_Group, y = n, fill = FR_Group)) +
  geom_col(width = 0.62, alpha = 0.93) +
  geom_text(aes(label = sprintf("%d titles\n(%d%%)", n, Pct)),
            vjust    = -0.25,
            size     = 3.6,
            fontface = "bold",
            colour   = "grey22") +
  # "Normal zone" shading
  annotate("rect",
           xmin = 0.5, xmax = 3.5,
           ymin = 0,   ymax = max(champ_fr$n) * 1.25,
           fill = "#EBF5FB", alpha = 0.35) +
  annotate("text",
           x = 2, y = max(champ_fr$n) * 1.22,
           label    = "Expected zone (FR ≤ 3)",
           colour   = "#2C3E73", size = 3.2, fontface = "italic") +
  annotate("text",
           x = 4.5, y = max(champ_fr$n) * 1.22,
           label    = "Surprise zone",
           colour   = "#C0392B", size = 3.2, fontface = "italic") +
  scale_fill_manual(values = fr_cols) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(
    title    = "76% of Titles Were Won by the League’s Richest Club",
    subtitle = sprintf(
      "%d champions over 10 seasons across 5 leagues  •  92%% came from the financial top 3",
      sum(champ_fr$n)
    ),
    x       = "Financial Rank of the champion that season",
    y       = "Number of league titles",
    caption = paste0(
      "FR 6+ = 4 champions: Leicester City (FR 11, 2015–16), Napoli (FR 5, 2024–25), ",
      "Lille (FR 4, 2020–21), Liverpool (FR 4, 2024–25)."
    )
  ) +
  theme_clean +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )

path34 <- "outputs/34_champions_fr_dist.png"
ggsave(path34, p34, width = 10, height = 6, dpi = 200)
track(path34)
cat(sprintf("  Saved: %s\n", path34))
cat("PHASE 7 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 8 — Final output inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 8: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s):\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n07_beautiful_charts.R complete.\n")
