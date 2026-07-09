# ==============================================================
# 08_time_split_model.R
# Splits seasons into Early (2015-16 to 2019-20, Season_Index 1-5)
# and Late (2020-21 to 2024-25, Season_Index 6-10), refits Model H
# on each half, and formally tests whether the Financial_Rank slope
# differs between periods.
#
# Outputs (all saved to outputs/):
#   38_time_split_model_comparison.csv
#   38a_time_split_scatter.png
#   38b_time_split_coefficients.png
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr", "broom", "ggplot2", "scales"))

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

period_colours <- c(
  "Early (2015-16 to 2019-20)" = "#2C3E73",
  "Late (2020-21 to 2024-25)"  = "#C0392B"
)


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load data & assign Period
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load Dataset & Assign Period ===\n")

master_path <- "data/merged/master_dataset_corrected.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset_corrected.csv — run extensions/scripts/15_spal_correction.R first.")

master <- read_csv(master_path, show_col_types = FALSE)

model_df <- master |>
  filter(!is.na(League_Position), !is.na(Financial_Rank)) |>
  mutate(
    League = factor(as.character(League)),
    Period = ifelse(Season_Index <= 5,
                    "Early (2015-16 to 2019-20)",
                    "Late (2020-21 to 2024-25)"),
    Period = factor(Period, levels = c("Early (2015-16 to 2019-20)",
                                        "Late (2020-21 to 2024-25)"))
  )

cat(sprintf("  Complete-case rows: %d\n", nrow(model_df)))
print(table(model_df$Period))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Fit Model H separately on each period
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Fit Model H per Period ===\n")

df_early <- model_df |> filter(Period == "Early (2015-16 to 2019-20)")
df_late  <- model_df |> filter(Period == "Late (2020-21 to 2024-25)")

mod_early <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = df_early)
mod_late  <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = df_late)

sum_early <- summary(mod_early)
sum_late  <- summary(mod_late)

cat(sprintf("  Early: N = %d  R2 = %.4f  Adj-R2 = %.4f  AIC = %.2f\n",
            nrow(df_early), sum_early$r.squared, sum_early$adj.r.squared, AIC(mod_early)))
cat(sprintf("  Late:  N = %d  R2 = %.4f  Adj-R2 = %.4f  AIC = %.2f\n",
            nrow(df_late), sum_late$r.squared, sum_late$adj.r.squared, AIC(mod_late)))
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Comparison table (38_time_split_model_comparison.csv)
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Build Comparison Table ===\n")

coef_early <- coef(sum_early)
coef_late  <- coef(sum_late)
ci_early   <- confint(mod_early)
ci_late    <- confint(mod_late)

comparison_tbl <- tibble(
  Period          = c("Early (2015-16 to 2019-20)", "Late (2020-21 to 2024-25)"),
  N               = c(nrow(df_early), nrow(df_late)),
  R2              = round(c(sum_early$r.squared, sum_late$r.squared), 4),
  Adj_R2          = round(c(sum_early$adj.r.squared, sum_late$adj.r.squared), 4),
  AIC             = round(c(AIC(mod_early), AIC(mod_late)), 2),
  Slope_FR        = round(c(coef_early["Financial_Rank", "Estimate"],
                            coef_late["Financial_Rank", "Estimate"]), 4),
  Slope_FR_SE     = round(c(coef_early["Financial_Rank", "Std. Error"],
                            coef_late["Financial_Rank", "Std. Error"]), 4),
  Slope_FR_CI_low = round(c(ci_early["Financial_Rank", 1], ci_late["Financial_Rank", 1]), 4),
  Slope_FR_CI_high= round(c(ci_early["Financial_Rank", 2], ci_late["Financial_Rank", 2]), 4),
  Slope_FR2       = round(c(coef_early["I(Financial_Rank^2)", "Estimate"],
                            coef_late["I(Financial_Rank^2)", "Estimate"]), 4),
  Slope_FR2_SE    = round(c(coef_early["I(Financial_Rank^2)", "Std. Error"],
                            coef_late["I(Financial_Rank^2)", "Std. Error"]), 4)
)

path_comp <- "outputs/38_time_split_model_comparison.csv"
write_csv(comparison_tbl, path_comp)
track(path_comp)
cat(sprintf("  Saved: %s\n", path_comp))
print(as.data.frame(comparison_tbl))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Formal test — does the Financial_Rank slope differ
# between periods? Pooled model with Period interaction.
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Interaction Test for Slope Difference ===\n")

mod_pooled      <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2) + Period,
                       data = model_df)
mod_interaction <- lm(League_Position ~ Financial_Rank * Period + I(Financial_Rank^2) * Period,
                       data = model_df)

interaction_anova <- anova(mod_pooled, mod_interaction)
cat("  ANOVA — pooled (no interaction) vs interaction model:\n")
print(interaction_anova)

int_tidy <- tidy(mod_interaction, conf.int = TRUE)
slope_int_row <- int_tidy |> filter(term == "Financial_Rank:PeriodLate (2020-21 to 2024-25)")

cat(sprintf("\n  Financial_Rank x Period(Late) interaction term:\n"))
cat(sprintf("    Estimate = %.4f, SE = %.4f, t = %.4f, p = %.6f — %s\n",
            slope_int_row$estimate, slope_int_row$std.error,
            slope_int_row$statistic, slope_int_row$p.value,
            ifelse(slope_int_row$p.value < 0.05,
                   "SIGNIFICANT — slope differs between periods",
                   "not significant — no evidence slope differs")))

path_int <- "outputs/38_time_split_interaction_test.csv"
write_csv(int_tidy, path_int)
track(path_int)
cat(sprintf("  Saved: %s\n", path_int))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Plot 38a — side-by-side scatter with fitted curves
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Scatter Plot (38a_time_split_scatter.png) ===\n")

fr_seq <- seq(1, max(model_df$Financial_Rank, na.rm = TRUE), length.out = 300)

pred_early <- predict(mod_early, newdata = data.frame(Financial_Rank = fr_seq),
                      interval = "confidence", level = 0.95) |>
  as.data.frame() |>
  mutate(Financial_Rank = fr_seq, Period = "Early (2015-16 to 2019-20)")

pred_late <- predict(mod_late, newdata = data.frame(Financial_Rank = fr_seq),
                     interval = "confidence", level = 0.95) |>
  as.data.frame() |>
  mutate(Financial_Rank = fr_seq, Period = "Late (2020-21 to 2024-25)")

pred_df <- bind_rows(pred_early, pred_late) |>
  mutate(Period = factor(Period, levels = names(period_colours)))

p38a <- ggplot() +
  geom_ribbon(data = pred_df, aes(x = Financial_Rank, ymin = lwr, ymax = upr,
                                   fill = Period), alpha = 0.15) +
  geom_jitter(data = model_df, aes(x = Financial_Rank, y = League_Position,
                                    colour = Period),
              alpha = 0.35, size = 1.3, width = 0.18, height = 0.18) +
  geom_line(data = pred_df, aes(x = Financial_Rank, y = fit, colour = Period),
            linewidth = 1.3) +
  facet_wrap(~Period) +
  scale_colour_manual(values = period_colours) +
  scale_fill_manual(values = period_colours) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
  labs(
    title    = "Has the Money–Position Relationship Changed Over Time?",
    subtitle = sprintf("Model H (quadratic) fit separately  •  Early N = %d, Late N = %d",
                       nrow(df_early), nrow(df_late)),
    x       = "Financial Rank within league-season (1 = richest)",
    y       = "Final league position (reversed: top = better)",
    caption = "Shaded band = 95% confidence interval of the fitted curve."
  ) +
  theme_clean +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 11))

path38a <- "outputs/38a_time_split_scatter.png"
ggsave(path38a, p38a, width = 12, height = 6.5, dpi = 150)
track(path38a)
cat(sprintf("  Saved: %s\n", path38a))
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Plot 38b — coefficient comparison plot
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Coefficient Plot (38b_time_split_coefficients.png) ===\n")

coef_plot_df <- comparison_tbl |>
  select(Period, Slope_FR, Slope_FR_CI_low, Slope_FR_CI_high) |>
  mutate(Period = factor(Period, levels = names(period_colours)))

overlap <- !(coef_plot_df$Slope_FR_CI_high[1] < coef_plot_df$Slope_FR_CI_low[2] ||
             coef_plot_df$Slope_FR_CI_high[2] < coef_plot_df$Slope_FR_CI_low[1])

p38b <- ggplot(coef_plot_df, aes(x = Slope_FR, y = Period, colour = Period)) +
  geom_vline(xintercept = 0, colour = "grey40", linetype = "dashed", linewidth = 0.6) +
  geom_pointrange(aes(xmin = Slope_FR_CI_low, xmax = Slope_FR_CI_high),
                  linewidth = 1.1, size = 0.9) +
  geom_text(aes(label = sprintf("%.3f  [%.3f, %.3f]", Slope_FR, Slope_FR_CI_low, Slope_FR_CI_high)),
            vjust = -1.6, size = 3.6, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = period_colours) +
  labs(
    title    = "Financial Rank Slope: Early vs Late Period",
    subtitle = sprintf(
      "Point = OLS estimate, bars = 95%% CI  •  Intervals %s  •  Interaction p = %.4f",
      ifelse(overlap, "OVERLAP (no strong evidence of change)", "DO NOT overlap (evidence of change)"),
      slope_int_row$p.value
    ),
    x       = "Slope on Financial_Rank (linear term of Model H)",
    y       = NULL,
    caption = "Model H: League_Position ~ Financial_Rank + Financial_Rank^2, fit separately per period."
  ) +
  theme_clean +
  theme(legend.position = "none")

path38b <- "outputs/38b_time_split_coefficients.png"
ggsave(path38b, p38b, width = 10, height = 5, dpi = 150)
track(path38b)
cat(sprintf("  Saved: %s\n", path38b))
cat("PHASE 6 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 7: Output inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 7: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s):\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n08_time_split_model.R complete.\n")
