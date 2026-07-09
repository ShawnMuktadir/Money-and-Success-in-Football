# ==============================================================
# 05_analysis_and_regression.R
# Exploratory analysis and regression modelling on the merged
# master dataset produced by 04_merge_standings_and_market_values.R
#
# Outputs (all saved to outputs/):
#   01_descriptive_statistics.csv
#   02_financial_rank_vs_position.png
#   03_points_vs_market_value.png
#   04_points_by_financial_rank_tier.png
#   05_log_value_vs_position_by_league.png
#   06_champions_surprise_summary.csv
#   07_champions_surprise_detail.csv
#   08_regression_results.csv
#   09_model_fit_summary.csv
#   10_league_regression_breakdown.csv
#   11_ols_coefficient_plot.png
#   12_champion_probability_curve.png
#   13_regression_diagnostics_modelD.png
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr", "ggplot2", "broom", "scales", "purrr", "stringr"))

make_dir("outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load & Validate Master Dataset
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load & Validate Master Dataset ===\n")

master_path <- "data/merged/master_dataset_corrected.csv"
if (!file.exists(master_path)) {
  stop(paste0(
    "Cannot find ", master_path, ".\n",
    "Please run extensions/scripts/15_spal_correction.R first."
  ))
}

master <- read_csv(master_path, show_col_types = FALSE)

required_cols <- c("League", "Season", "Season_Index", "League_Position",
                   "Team", "Points", "Financial_Rank", "Market_Value_M_EUR",
                   "Normalized_Value", "Log_Normalized_Value", "Champion")
missing_cols <- setdiff(required_cols, names(master))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

master <- master |>
  mutate(
    Champion_binary = as.integer(Champion == "Yes"),
    League          = as.character(League)
  )

cat(sprintf("  Rows        : %d\n",  nrow(master)))
cat(sprintf("  Columns     : %d\n",  ncol(master)))
cat(sprintf("  Leagues     : %s\n",  paste(sort(unique(master$League)), collapse = ", ")))
cat(sprintf("  Seasons     : %s\n",  paste(sort(unique(master$Season)), collapse = ", ")))
cat(sprintf("  Total champions : %d\n", sum(master$Champion_binary)))
cat(sprintf("  Rows with Financial_Rank : %d (%.1f%%)\n",
            sum(!is.na(master$Financial_Rank)),
            mean(!is.na(master$Financial_Rank)) * 100))

model_df <- master |>
  filter(!is.na(Financial_Rank),
         !is.na(Log_Normalized_Value),
         !is.na(League_Position))

cat(sprintf("  Complete-case rows for modelling: %d\n", nrow(model_df)))
if (nrow(model_df) < 50) {
  stop("Too few complete rows for modelling — re-run script 04.")
}
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Descriptive Statistics  →  01_descriptive_statistics.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Descriptive Statistics ===\n")

summarise_block <- function(df) {
  df |>
    filter(!is.na(Financial_Rank)) |>
    summarise(
      N               = n(),
      Mean_Position   = round(mean(League_Position,     na.rm = TRUE), 2),
      SD_Position     = round(sd(League_Position,       na.rm = TRUE), 2),
      Mean_Fin_Rank   = round(mean(Financial_Rank,      na.rm = TRUE), 2),
      SD_Fin_Rank     = round(sd(Financial_Rank,        na.rm = TRUE), 2),
      Mean_MV_M_EUR   = round(mean(Market_Value_M_EUR,  na.rm = TRUE), 1),
      SD_MV_M_EUR     = round(sd(Market_Value_M_EUR,    na.rm = TRUE), 1),
      Mean_Points     = round(mean(Points,              na.rm = TRUE), 2),
      SD_Points       = round(sd(Points,                na.rm = TRUE), 2),
      Corr_FR_Pos     = round(cor(Financial_Rank, League_Position, use = "complete.obs"), 3),
      Spearman_FR_Pos = round(cor(Financial_Rank, League_Position,
                                  use = "complete.obs", method = "spearman"), 3),
      Corr_LogMV_Pos  = round(cor(Log_Normalized_Value, League_Position,
                                  use = "complete.obs"), 3),
      .groups = "drop"
    )
}

desc_overall <- master |>
  summarise_block() |>
  mutate(Group = "ALL LEAGUES (combined)", .before = 1)

desc_by_league <- master |>
  group_by(Group = League) |>
  summarise_block() |>
  ungroup()

desc_stats <- bind_rows(desc_overall, desc_by_league)

path01 <- "outputs/01_descriptive_statistics.csv"
write_csv(desc_stats, path01)
track(path01)

cat(sprintf("  Saved: %s\n", path01))
cat("\n=== Descriptive Statistics ===\n")
print(as.data.frame(desc_stats), digits = 4)
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Champions Surprise Analysis
#   →  06_champions_surprise_summary.csv
#   →  07_champions_surprise_detail.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Champions Surprise Analysis ===\n")

# Champions with Financial_Rank information available
champs_with_fr <- master |>
  filter(Champion == "Yes", !is.na(Financial_Rank))

total_champs_with_fr <- nrow(champs_with_fr)
surprise_detail      <- champs_with_fr |>
  filter(Financial_Rank > 3) |>
  select(League, Season, Team, Points,
         Financial_Rank, Market_Value_M_EUR, Normalized_Value) |>
  arrange(desc(Financial_Rank), League, Season)

n_surprise   <- nrow(surprise_detail)
pct_surprise <- round(n_surprise / total_champs_with_fr * 100, 1)

cat("\n══════════════════════════════════════════════\n")
cat(sprintf("  %d (%.1f%%) won despite Financial_Rank > 3\n", n_surprise, pct_surprise))
cat("══════════════════════════════════════════════\n\n")
cat(sprintf("  Total champions with Financial_Rank data : %d\n", total_champs_with_fr))
cat(sprintf("  Champions with Financial_Rank > 3        : %d\n", n_surprise))
cat(sprintf("  => %.1f%% won despite Financial_Rank > 3\n\n", pct_surprise))

surprise_by_league <- champs_with_fr |>
  group_by(League) |>
  summarise(
    Total_Titles    = n(),
    Titles_FR_gt3   = sum(Financial_Rank > 3),
    Pct_FR_gt3      = round(sum(Financial_Rank > 3) / n() * 100, 1),
    Min_FR_winner   = min(Financial_Rank),
    Max_FR_winner   = max(Financial_Rank),
    Avg_FR_winner   = round(mean(Financial_Rank), 2),
    .groups = "drop"
  )

path06 <- "outputs/06_champions_surprise_summary.csv"
path07 <- "outputs/07_champions_surprise_detail.csv"
write_csv(surprise_by_league, path06)
write_csv(surprise_detail,    path07)
track(path06)
track(path07)

cat(sprintf("  Saved: %s\n", path06))
cat(sprintf("  Saved: %s\n", path07))
cat("\n=== By-League Breakdown of 'Surprise' Champions ===\n")
print(as.data.frame(surprise_by_league))
if (nrow(surprise_detail) > 0) {
  cat("\n=== Detail: Champions with Financial_Rank > 3 ===\n")
  print(as.data.frame(surprise_detail))
}
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Exploratory Visualisations
#   →  02_financial_rank_vs_position.png
#   →  03_points_vs_market_value.png
#   →  04_points_by_financial_rank_tier.png
#   →  05_log_value_vs_position_by_league.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Exploratory Visualisations ===\n")

theme_seminar <- theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, colour = "grey40"),
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title    = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

league_colours <- c(
  "Bundesliga"     = "#E41A1C",
  "La Liga"        = "#FF7F00",
  "Ligue 1"        = "#4DAF4A",
  "Premier League" = "#984EA3",
  "Serie A"        = "#377EB8"
)

# ── Plot 02: Financial Rank vs League Position ─────────────────
cat("  Generating plot 02: Financial Rank vs League Position\n")
p02 <- ggplot(model_df, aes(x = Financial_Rank, y = League_Position)) +
  geom_jitter(aes(colour = League), alpha = 0.35, width = 0.3, height = 0.3, size = 1.6) +
  geom_smooth(method = "lm", colour = "black", linewidth = 1.1, se = TRUE) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = seq(1, 20, by = 2)) +
  scale_y_continuous(breaks = seq(1, 20, by = 2)) +
  labs(
    title    = "Financial Rank vs Final League Position (2015–2025)",
    subtitle = sprintf("N = %d team-seasons across 5 leagues  •  OLS trend (95%% CI)",
                       nrow(model_df)),
    x        = "Financial Rank  (1 = richest squad)",
    y        = "Final League Position  (1 = champion)",
    colour   = "League"
  ) +
  theme_seminar

path02 <- "outputs/02_financial_rank_vs_position.png"
ggsave(path02, p02, width = 9, height = 6, dpi = 150)
track(path02)
cat(sprintf("  Saved: %s\n", path02))

# ── Plot 03: Points vs Market Value (log scale) ────────────────
cat("  Generating plot 03: Points vs Market Value\n")
p03 <- ggplot(model_df, aes(x = Market_Value_M_EUR, y = Points)) +
  geom_point(aes(colour = Champion, shape = League), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", colour = "grey20", linewidth = 1, se = TRUE) +
  scale_colour_manual(values = c("No" = "steelblue", "Yes" = "firebrick"),
                      labels = c("No" = "Not champion", "Yes" = "Champion")) +
  scale_x_log10(labels = comma_format()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 4)) +
  labs(
    title    = "Squad Market Value vs Points (2015–2025)",
    subtitle = "Log scale on x-axis  •  Red = league champions  •  Grey band = 95%% CI",
    x        = "Squad Market Value (€M, log scale)",
    y        = "Final Points",
    colour   = "Status",
    shape    = "League"
  ) +
  theme_seminar

path03 <- "outputs/03_points_vs_market_value.png"
ggsave(path03, p03, width = 10, height = 6, dpi = 150)
track(path03)
cat(sprintf("  Saved: %s\n", path03))

# ── Plot 04: Points distribution by Financial Rank tier ────────
cat("  Generating plot 04: Points by Financial Rank tier\n")
tier_levels <- c("FR 1 (richest)", "FR 2", "FR 3", "FR 4–5", "FR 6–10", "FR 11+")
model_df_tier <- model_df |>
  mutate(
    FR_Tier = case_when(
      Financial_Rank == 1  ~ tier_levels[1],
      Financial_Rank == 2  ~ tier_levels[2],
      Financial_Rank == 3  ~ tier_levels[3],
      Financial_Rank <= 5  ~ tier_levels[4],
      Financial_Rank <= 10 ~ tier_levels[5],
      TRUE                  ~ tier_levels[6]
    ),
    FR_Tier = factor(FR_Tier, levels = tier_levels)
  )

p04 <- ggplot(model_df_tier, aes(x = FR_Tier, y = Points, fill = FR_Tier)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5, outlier.size = 1.5, linewidth = 0.6) +
  geom_jitter(width = 0.18, alpha = 0.12, size = 0.9, colour = "grey20") +
  scale_fill_manual(values = c(
    "FR 1 (richest)" = "#1a9850",
    "FR 2"           = "#66bd63",
    "FR 3"           = "#a6d96a",
    "FR 4–5"    = "#fee08b",
    "FR 6–10"   = "#f46d43",
    "FR 11+"         = "#d73027"
  )) +
  labs(
    title    = "Points Distribution by Financial Rank Tier (2015–2025)",
    subtitle = "Lower rank number = richer squad  •  Dots = individual team-seasons",
    x        = "Financial Rank Tier",
    y        = "Final Points",
    fill     = "FR Tier"
  ) +
  theme_seminar +
  theme(legend.position = "none")

path04 <- "outputs/04_points_by_financial_rank_tier.png"
ggsave(path04, p04, width = 10, height = 6, dpi = 150)
track(path04)
cat(sprintf("  Saved: %s\n", path04))

# ── Plot 05: Log-Normalised Value vs Position by League ────────
cat("  Generating plot 05: Log-Normalised Value vs Position by League\n")
p05 <- ggplot(model_df, aes(x = Log_Normalized_Value, y = League_Position)) +
  geom_point(aes(colour = Champion), alpha = 0.4, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.9) +
  scale_colour_manual(values = c("No" = "steelblue", "Yes" = "firebrick"),
                      labels = c("No" = "Not champion", "Yes" = "Champion")) +
  facet_wrap(~League, scales = "free_y", nrow = 2) +
  labs(
    title    = "Log-Normalised Squad Value vs League Position (by League, 2015–2025)",
    subtitle = "OLS fit per league  •  Red = champion",
    x        = "Log(Normalised Market Value)",
    y        = "Final League Position",
    colour   = "Status"
  ) +
  theme_seminar

path05 <- "outputs/05_log_value_vs_position_by_league.png"
ggsave(path05, p05, width = 13, height = 7, dpi = 150)
track(path05)
cat(sprintf("  Saved: %s\n", path05))

cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Regression Analysis
#   →  08_regression_results.csv
#   →  09_model_fit_summary.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Regression Analysis ===\n")

safe_tidy <- function(model, label, response, ...) {
  out <- tidy(model, conf.int = FALSE, ...)
  # Use Wald CIs (confint.default) to avoid profile-likelihood refitting,
  # which triggers "fitted probabilities numerically 0 or 1" warnings for
  # logistic models with near-separation.
  ci <- tryCatch(
    as.data.frame(confint.default(model, level = 0.95)),
    error = function(e) NULL
  )
  if (!is.null(ci)) {
    idx            <- match(out$term, rownames(ci))
    out$conf.low   <- ci[idx, 1]
    out$conf.high  <- ci[idx, 2]
  } else {
    out$conf.low   <- out$estimate - 1.96 * out$std.error
    out$conf.high  <- out$estimate + 1.96 * out$std.error
  }
  out |>
    mutate(model_label = label, response = response) |>
    select(model_label, response, term, estimate, std.error,
           statistic, p.value, conf.low, conf.high)
}

# Model A – OLS: League_Position ~ Financial_Rank
cat("  Model A: OLS — League_Position ~ Financial_Rank\n")
modA <- lm(League_Position ~ Financial_Rank, data = model_df)
sumA <- summary(modA)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f  |  p(F) = %.3e\n",
            sumA$r.squared, sumA$adj.r.squared,
            pf(sumA$fstatistic[1], sumA$fstatistic[2],
               sumA$fstatistic[3], lower.tail = FALSE)))
tidyA <- safe_tidy(modA, "A: OLS — Pos ~ FR", "League_Position")

# Model B – OLS: League_Position ~ Financial_Rank + League (fixed effects)
cat("  Model B: OLS — League_Position ~ Financial_Rank + League\n")
modB <- lm(League_Position ~ Financial_Rank + League, data = model_df)
sumB <- summary(modB)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f\n", sumB$r.squared, sumB$adj.r.squared))
tidyB <- safe_tidy(modB, "B: OLS — Pos ~ FR + League FE", "League_Position")

# Model C – OLS: League_Position ~ Log_Normalized_Value
cat("  Model C: OLS — League_Position ~ Log_Normalized_Value\n")
modC <- lm(League_Position ~ Log_Normalized_Value, data = model_df)
sumC <- summary(modC)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f\n", sumC$r.squared, sumC$adj.r.squared))
tidyC <- safe_tidy(modC, "C: OLS — Pos ~ Log(MV)", "League_Position")

# Model D – OLS full: League_Position ~ Log_Normalized_Value + League + Season_Index
cat("  Model D: OLS (full) — League_Position ~ Log_Norm + League + Season_Index\n")
modD <- lm(League_Position ~ Log_Normalized_Value + League + Season_Index, data = model_df)
sumD <- summary(modD)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f\n", sumD$r.squared, sumD$adj.r.squared))
tidyD <- safe_tidy(modD, "D: OLS full — Pos ~ Log(MV) + League + Season", "League_Position")

# Model E – Logistic: Champion_binary ~ Financial_Rank
cat("  Model E: Logit — Champion_binary ~ Financial_Rank\n")
modE <- glm(Champion_binary ~ Financial_Rank, data = model_df, family = binomial())
cat(sprintf("    Null dev: %.2f  |  Resid dev: %.2f  |  AIC: %.2f  |  McFadden-R²: %.4f\n",
            modE$null.deviance, modE$deviance, AIC(modE),
            1 - modE$deviance / modE$null.deviance))
tidyE <- safe_tidy(modE, "E: Logit — Champion ~ FR", "Champion_binary")

# Model F – Logistic full: Champion_binary ~ Log_Normalized_Value + League
cat("  Model F: Logit (full) — Champion_binary ~ Log_Norm + League\n")
modF <- glm(Champion_binary ~ Log_Normalized_Value + League, data = model_df, family = binomial())
cat(sprintf("    Null dev: %.2f  |  Resid dev: %.2f  |  AIC: %.2f  |  McFadden-R²: %.4f\n",
            modF$null.deviance, modF$deviance, AIC(modF),
            1 - modF$deviance / modF$null.deviance))
tidyF <- safe_tidy(modF, "F: Logit full — Champion ~ Log(MV) + League", "Champion_binary")

# Model G – OLS interaction: League_Position ~ Financial_Rank * League
cat("  Model G: OLS — League_Position ~ Financial_Rank * League\n")
modG <- lm(League_Position ~ Financial_Rank * League, data = model_df)
sumG <- summary(modG)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f\n", sumG$r.squared, sumG$adj.r.squared))
tidyG <- safe_tidy(modG, "G: OLS — Pos ~ FR * League", "League_Position")

# Model H – OLS quadratic (preferred): League_Position ~ FR + FR^2
cat("  Model H: OLS — League_Position ~ Financial_Rank + I(Financial_Rank^2)  [PREFERRED]\n")
modH <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df)
sumH <- summary(modH)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f  |  AIC: %.2f\n",
            sumH$r.squared, sumH$adj.r.squared, AIC(modH)))
tidyH <- safe_tidy(modH, "H: OLS — Pos ~ FR + FR^2 (*preferred*)", "League_Position")

# Model I – OLS cubic: League_Position ~ FR + FR^2 + FR^3
cat("  Model I: OLS — League_Position ~ FR + FR^2 + FR^3\n")
modI <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2) + I(Financial_Rank^3),
           data = model_df)
sumI <- summary(modI)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f  |  AIC: %.2f\n",
            sumI$r.squared, sumI$adj.r.squared, AIC(modI)))
tidyI <- safe_tidy(modI, "I: OLS — Pos ~ FR + FR^2 + FR^3", "League_Position")

# Model J – OLS log: League_Position ~ log(Financial_Rank)
cat("  Model J: OLS — League_Position ~ log(Financial_Rank)\n")
modJ <- lm(League_Position ~ log(Financial_Rank), data = model_df)
sumJ <- summary(modJ)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f  |  AIC: %.2f\n",
            sumJ$r.squared, sumJ$adj.r.squared, AIC(modJ)))
tidyJ <- safe_tidy(modJ, "J: OLS — Pos ~ Log(FR)", "League_Position")

# Model M – OLS sqrt: League_Position ~ sqrt(Financial_Rank)
cat("  Model M: OLS — League_Position ~ sqrt(Financial_Rank)\n")
modM <- lm(League_Position ~ sqrt(Financial_Rank), data = model_df)
sumM <- summary(modM)
cat(sprintf("    R² = %.4f  |  Adj-R² = %.4f  |  AIC: %.2f\n",
            sumM$r.squared, sumM$adj.r.squared, AIC(modM)))
tidyM <- safe_tidy(modM, "M: OLS — Pos ~ sqrt(FR)", "League_Position")

reg_results <- bind_rows(tidyA, tidyB, tidyC, tidyD, tidyE, tidyF,
                          tidyG, tidyH, tidyI, tidyJ, tidyM) |>
  mutate(across(where(is.numeric) & !c(statistic), ~ round(.x, 6)),
         statistic       = round(statistic, 4),
         significant_05  = !is.na(p.value) & p.value < 0.05,
         significant_01  = !is.na(p.value) & p.value < 0.01)

path08 <- "outputs/08_regression_results.csv"
write_csv(reg_results, path08)
track(path08)
cat(sprintf("\n  Saved: %s  (%d rows)\n", path08, nrow(reg_results)))

model_fit <- tibble(
  Model = c("A", "B", "C", "D", "G", "H*", "I", "J", "M", "E"),
  Type  = c(rep("OLS", 9), "Logit"),
  Formula = c(
    "Pos ~ FR",
    "Pos ~ FR + League",
    "Pos ~ Log(MV)",
    "Pos ~ Log(MV) + League + Season",
    "Pos ~ FR x League",
    "Pos ~ FR + FR^2",
    "Pos ~ FR + FR^2 + FR^3",
    "Pos ~ Log(FR)",
    "Pos ~ sqrt(FR)",
    "Champion ~ FR"
  ),
  Notes = c(
    "League_Position = a + b1 x FR",
    "League_Position = a + b1 x FR + b2 x League",
    "League_Position = a + b1 x Log(Market_Value)",
    "League_Position = a + b1 x Log(Market_Value) + b2 x League + b3 x Season",
    "League_Position = a + b1 x FR x League",
    "League_Position = a + b1 x FR + b2 x FR-squared  [preferred]",
    "League_Position = a + b1 x FR + b2 x FR-squared + b3 x FR-cubed",
    "League_Position = a + b1 x Log(FR)",
    "League_Position = a + b1 x sqrt(FR)",
    "Log(P(Champion) / (1 - P(Champion))) = a + b1 x FR"
  ),
  N = nrow(model_df),
  R2 = c(
    round(sumA$r.squared, 4), round(sumB$r.squared, 4),
    round(sumC$r.squared, 4), round(sumD$r.squared, 4),
    round(sumG$r.squared, 4), round(sumH$r.squared, 4),
    round(sumI$r.squared, 4), round(sumJ$r.squared, 4),
    round(sumM$r.squared, 4),
    round(1 - modE$deviance / modE$null.deviance, 4)
  ),
  Adj_R2 = c(
    round(sumA$adj.r.squared, 4), round(sumB$adj.r.squared, 4),
    round(sumC$adj.r.squared, 4), round(sumD$adj.r.squared, 4),
    round(sumG$adj.r.squared, 4), round(sumH$adj.r.squared, 4),
    round(sumI$adj.r.squared, 4), round(sumJ$adj.r.squared, 4),
    round(sumM$adj.r.squared, 4), NA_real_
  ),
  AIC = round(c(AIC(modA), AIC(modB), AIC(modC), AIC(modD),
                AIC(modG), AIC(modH), AIC(modI), AIC(modJ),
                AIC(modM), AIC(modE)), 2),
  BIC = round(c(BIC(modA), BIC(modB), BIC(modC), BIC(modD),
                BIC(modG), BIC(modH), BIC(modI), BIC(modJ),
                BIC(modM), BIC(modE)), 2)
)

path09 <- "outputs/09_model_fit_summary.csv"
write_csv(model_fit, path09)
track(path09)
cat(sprintf("  Saved: %s\n", path09))
cat("\n=== Model Fit Summary ===\n")
print(as.data.frame(model_fit))
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Regression Visualisations
#   →  11_ols_coefficient_plot.png
#   →  12_champion_probability_curve.png
#   →  13_regression_diagnostics_modelD.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Regression Visualisations ===\n")

# ── Plot 11: OLS coefficient plot ──────────────────────────────
cat("  Generating plot 11: OLS coefficient plot\n")

coef_focus_terms <- c("Financial_Rank", "Log_Normalized_Value", "Season_Index")
coef_ols <- bind_rows(tidyA, tidyB, tidyC, tidyD) |>
  filter(term %in% coef_focus_terms) |>
  mutate(
    term = recode(term,
                  "Financial_Rank"        = "Financial Rank",
                  "Log_Normalized_Value"  = "Log(Norm. Market Value)",
                  "Season_Index"          = "Season Index"),
    model_label = factor(model_label,
                         levels = c("A: OLS — Pos ~ FR",
                                    "B: OLS — Pos ~ FR + League FE",
                                    "C: OLS — Pos ~ Log(MV)",
                                    "D: OLS full — Pos ~ Log(MV) + League + Season"))
  )

p11 <- ggplot(coef_ols,
              aes(x = estimate, y = model_label, colour = term, shape = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                orientation = "y", width = 0.25, linewidth = 0.8) +
  geom_point(size = 4) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title    = "OLS Regression Coefficients — Key Predictors (95% CI)",
    subtitle = "Response: Final League Position  •  Positive estimate = worse position",
    x        = "Coefficient Estimate",
    y        = NULL,
    colour   = "Predictor",
    shape    = "Predictor"
  ) +
  theme_seminar

path11 <- "outputs/11_ols_coefficient_plot.png"
ggsave(path11, p11, width = 11, height = 5.5, dpi = 150)
track(path11)
cat(sprintf("  Saved: %s\n", path11))

# ── Plot 12: Champion probability curve (Model E) ──────────────
cat("  Generating plot 12: Champion probability curve\n")

fr_range  <- range(model_df$Financial_Rank, na.rm = TRUE)
pred_grid <- tibble(Financial_Rank = seq(fr_range[1], fr_range[2], by = 0.1))
pred_link <- predict(modE, newdata = pred_grid, type = "link", se.fit = TRUE)
pred_grid <- pred_grid |>
  mutate(
    log_odds = pred_link$fit,
    se_link  = pred_link$se.fit,
    prob     = plogis(log_odds),
    prob_lo  = plogis(log_odds - 1.96 * se_link),
    prob_hi  = plogis(log_odds + 1.96 * se_link)
  )

obs_prop <- model_df |>
  group_by(Financial_Rank) |>
  summarise(Obs_Prob = mean(Champion_binary), N = n(), .groups = "drop")

p12 <- ggplot() +
  geom_ribbon(data = pred_grid,
              aes(x = Financial_Rank, ymin = prob_lo, ymax = prob_hi),
              fill = "steelblue", alpha = 0.20) +
  geom_line(data = pred_grid,
            aes(x = Financial_Rank, y = prob),
            colour = "steelblue", linewidth = 1.3) +
  geom_point(data = obs_prop,
             aes(x = Financial_Rank, y = Obs_Prob, size = N),
             colour = "firebrick", alpha = 0.85) +
  annotate("text",
           x = max(fr_range) * 0.75,
           y = max(obs_prop$Obs_Prob, na.rm = TRUE) * 0.85,
           label = sprintf("%d%% won despite FR > 3", round(pct_surprise)),
           size = 4, colour = "firebrick", fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_size_continuous(range = c(2, 8), breaks = c(5, 10, 20, 50)) +
  labs(
    title    = "Predicted Probability of Championship vs Financial Rank (Model E)",
    subtitle = "Blue band = 95% CI of logistic fit  •  Red dots = observed win rate (sized by N)",
    x        = "Financial Rank  (1 = richest squad)",
    y        = "P(Champion)",
    size     = "N team-seasons"
  ) +
  theme_seminar

path12 <- "outputs/12_champion_probability_curve.png"
ggsave(path12, p12, width = 9, height = 6, dpi = 150)
track(path12)
cat(sprintf("  Saved: %s\n", path12))

# ── Plot 13: OLS diagnostic plots for Model D ──────────────────
cat("  Generating plot 13: OLS diagnostic plots (Model D)\n")

path13 <- "outputs/13_regression_diagnostics_modelD.png"
png(path13, width = 1400, height = 1000, res = 120)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1.5), oma = c(0, 0, 2, 0))
plot(modD, which = 1:4, cex.lab = 1.1, cex.caption = 0.9)
mtext("Model D: League_Position ~ Log_Norm_Value + League + Season_Index",
      outer = TRUE, cex = 1.0, font = 2)
dev.off()
track(path13)
cat(sprintf("  Saved: %s\n", path13))

cat("PHASE 6 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 7: League-Level Regression Breakdown
#   →  10_league_regression_breakdown.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 7: League-Level Regression Breakdown ===\n")

league_reg <- model_df |>
  group_by(League) |>
  group_modify(~ {
    d  <- .x
    m1 <- lm(League_Position ~ Financial_Rank,       data = d)
    m2 <- lm(League_Position ~ Log_Normalized_Value, data = d)
    s1 <- summary(m1)
    s2 <- summary(m2)
    tibble(
      N                  = nrow(d),
      # Model 1: Financial_Rank
      Coef_FR            = round(coef(m1)[["Financial_Rank"]], 4),
      R2_FR              = round(s1$r.squared, 4),
      Adj_R2_FR          = round(s1$adj.r.squared, 4),
      Pval_FR            = round(s1$coefficients["Financial_Rank", "Pr(>|t|)"], 6),
      Sig_FR             = s1$coefficients["Financial_Rank", "Pr(>|t|)"] < 0.05,
      Pearson_FR_Pos     = round(cor(d$Financial_Rank, d$League_Position, use = "complete.obs"), 4),
      Spearman_FR_Pos    = round(cor(d$Financial_Rank, d$League_Position,
                                     use = "complete.obs", method = "spearman"), 4),
      # Model 2: Log_Normalized_Value
      Coef_LogMV         = round(coef(m2)[["Log_Normalized_Value"]], 4),
      R2_LogMV           = round(s2$r.squared, 4),
      Adj_R2_LogMV       = round(s2$adj.r.squared, 4),
      Pval_LogMV         = round(s2$coefficients["Log_Normalized_Value", "Pr(>|t|)"], 6),
      Sig_LogMV          = s2$coefficients["Log_Normalized_Value", "Pr(>|t|)"] < 0.05
    )
  }) |>
  ungroup()

path10 <- "outputs/10_league_regression_breakdown.csv"
write_csv(league_reg, path10)
track(path10)
cat(sprintf("  Saved: %s\n", path10))
cat("\n=== League-Level OLS Regression Summary ===\n")
print(as.data.frame(league_reg))
cat("PHASE 7 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 8: Final Summary & Output Inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 8: Final Summary & Output Inventory ===\n")

cat("\n")
cat("══════════════════════════════════════════════\n")
cat(sprintf("  KEY RESULT:  %d (%.1f%%) won despite Financial_Rank > 3\n",
            n_surprise, pct_surprise))
cat("══════════════════════════════════════════════\n\n")

cat("  Best predictive model (OLS): Model D\n")
cat(sprintf("    Formula  : League_Position ~ Log_Norm_Value + League + Season_Index\n"))
cat(sprintf("    R²       : %.4f\n", sumD$r.squared))
cat(sprintf("    Adj-R²   : %.4f\n", sumD$adj.r.squared))
cat(sprintf("    AIC      : %.2f\n", AIC(modD)))
cat("\n")
cat("  Best champion model (Logit): Model F\n")
cat(sprintf("    Formula  : Champion ~ Log_Norm_Value + League\n"))
cat(sprintf("    McFadden : %.4f\n", 1 - modF$deviance / modF$null.deviance))
cat(sprintf("    AIC      : %.2f\n\n", AIC(modF)))

# ── Final folder check ─────────────────────────────────────────
cat("=== FINAL FOLDER CHECK: outputs/ ===\n")
all_out <- list.files("outputs", full.names = TRUE, recursive = FALSE)

if (length(all_out) == 0L) {
  cat("  WARNING: No files found in outputs/\n")
} else {
  fi      <- file.info(all_out)
  out_df  <- data.frame(
    File     = basename(all_out),
    Path     = normalizePath(all_out, winslash = "/", mustWork = FALSE),
    Size_KB  = round(fi$size / 1024, 1),
    Modified = format(fi$mtime, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  ) |>
    arrange(File)
  cat(sprintf("  Total files in outputs/ : %d\n", nrow(out_df)))
  cat(sprintf("  Total size              : %.1f KB\n\n", sum(out_df$Size_KB)))
  print(out_df, row.names = FALSE, right = FALSE)
}

cat(sprintf("\n  Script tracked %d output files:\n", length(OUTPUT_FILES)))
for (f in sort(OUTPUT_FILES)) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}

n_found <- length(all_out)
cat(sprintf("\n  Expected minimum: 7 files  |  Found: %d\n", n_found))
if (n_found < 7L) {
  warning(sprintf("Only %d file(s) in outputs/ — expected at least 7.", n_found))
} else {
  cat("  CHECK PASSED: at least 7 output files present.\n")
}
cat("\n05_analysis_and_regression.R complete.\n")
