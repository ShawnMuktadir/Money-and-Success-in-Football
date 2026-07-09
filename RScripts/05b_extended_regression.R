# ==============================================================
# 05b_extended_regression.R
# Extended regression analysis building on
# 05_analysis_and_regression.R. Adds robustness diagnostics,
# interaction/non-linear/time-trend models, an alternative
# "top-4 finish" outcome, and an ordinal-logit specification on
# the same master dataset produced by
# 04_merge_standings_and_market_values.R
#
# Outputs (all saved to outputs/):
#   14_heteroskedasticity_autocorrelation_tests.csv
#   15_modelD_robust_se.csv
#   16_vif_modelD.csv
#   17_regression_interaction_FR_League.csv
#   18_nested_model_significance_tests.csv
#   19_interaction_slopes_by_league.png
#   20_quadratic_model_results.csv
#   21_quadratic_fit_plot.png
#   22_time_trend_results.csv
#   23_time_trend_plot.png
#   24_top4_logit_results.csv
#   25_top4_probability_curve.png
#   26_ordinal_logit_results.csv
#   27_extended_model_comparison.csv
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr", "ggplot2", "broom", "scales", "purrr", "stringr"))

# car / MASS / lmtest / sandwich are used only via `::` (never attached)
# to avoid masking dplyr::select / dplyr::recode.
ensure_installed <- function(pkgs) {
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0) {
    cat("Installing:", paste(to_install, collapse = ", "), "\n")
    install.packages(to_install,
                     repos = "https://packagemanager.posit.co/cran/latest",
                     quiet = TRUE)
  }
}
ensure_installed(c("car", "MASS", "lmtest", "sandwich"))

make_dir("outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }

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

safe_tidy <- function(model, label, response, ...) {
  out <- tidy(model, conf.int = FALSE, ...)
  ci <- tryCatch(
    as.data.frame(confint.default(model, level = 0.95)),
    error = function(e) NULL
  )
  if (!is.null(ci)) {
    idx           <- match(out$term, rownames(ci))
    out$conf.low  <- ci[idx, 1]
    out$conf.high <- ci[idx, 2]
  } else {
    out$conf.low  <- out$estimate - 1.96 * out$std.error
    out$conf.high <- out$estimate + 1.96 * out$std.error
  }
  out |>
    mutate(model_label = label, response = response) |>
    select(model_label, response, term, estimate, std.error,
           statistic, p.value, conf.low, conf.high)
}

anova_row <- function(comparison_label, m1, m2) {
  a <- anova(m1, m2)
  tibble(
    Comparison  = comparison_label,
    Model1_Df   = a$Res.Df[1],
    Model2_Df   = a$Res.Df[2],
    Model1_RSS  = round(a$RSS[1], 4),
    Model2_RSS  = round(a$RSS[2], 4),
    Df          = a$Df[2],
    Sum_of_Sq   = round(a[["Sum of Sq"]][2], 4),
    F_statistic = round(a$F[2], 4),
    p_value     = signif(a[["Pr(>F)"]][2], 6)
  )
}

model_stats_row <- function(model_name, model, type, formula_str) {
  ll <- tryCatch(as.numeric(logLik(model)), error = function(e) NA_real_)
  tibble(
    Model    = model_name,
    Type     = type,
    Formula  = formula_str,
    LogLik   = round(ll, 2),
    AIC      = round(tryCatch(AIC(model), error = function(e) NA_real_), 2),
    BIC      = round(tryCatch(BIC(model), error = function(e) NA_real_), 2)
  )
}


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load & Prepare Master Dataset
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load & Prepare Master Dataset ===\n")

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

model_df <- master |>
  filter(!is.na(Financial_Rank),
         !is.na(Log_Normalized_Value),
         !is.na(League_Position)) |>
  mutate(
    Top4_binary  = as.integer(League_Position <= 4),
    Position_Tier = case_when(
      League_Position <= 4  ~ "1: Champions League (1-4)",
      League_Position <= 6  ~ "2: Europa/Conference (5-6)",
      League_Position <= 12 ~ "3: Mid-table (7-12)",
      League_Position <= 17 ~ "4: Lower-table (13-17)",
      TRUE                   ~ "5: Relegation zone (18+)"
    ),
    Position_Tier = factor(
      Position_Tier,
      levels = c("1: Champions League (1-4)", "2: Europa/Conference (5-6)",
                 "3: Mid-table (7-12)", "4: Lower-table (13-17)",
                 "5: Relegation zone (18+)"),
      ordered = TRUE
    )
  )

cat(sprintf("  Complete-case rows for modelling: %d\n", nrow(model_df)))
if (nrow(model_df) < 50) {
  stop("Too few complete rows for modelling — re-run script 04.")
}
cat(sprintf("  Top-4 finishes: %d (%.1f%%)\n",
           sum(model_df$Top4_binary), mean(model_df$Top4_binary) * 100))
cat("  Position tier distribution:\n")
print(table(model_df$Position_Tier))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Rebuild Baseline Models (from 05) for Reference
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Rebuild Baseline Models for Reference ===\n")

modA <- lm(League_Position ~ Financial_Rank, data = model_df)
modB <- lm(League_Position ~ Financial_Rank + League, data = model_df)
modD <- lm(League_Position ~ Log_Normalized_Value + League + Season_Index, data = model_df)
modF <- glm(Champion_binary ~ Log_Normalized_Value + League, data = model_df, family = binomial())
modI0 <- lm(League_Position ~ Financial_Rank + Season_Index, data = model_df)

cat("  Rebuilt: Model A (FR), Model B (FR+League), Model D (full OLS),\n")
cat("           Model F (champion logit), Model I0 (FR+Season additive)\n")
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Heteroskedasticity & Autocorrelation Diagnostics
#   →  14_heteroskedasticity_autocorrelation_tests.csv
#   →  15_modelD_robust_se.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Heteroskedasticity & Autocorrelation Diagnostics (Model D) ===\n")

bp_test <- lmtest::bptest(modD)
dw_test <- lmtest::dwtest(modD)

diag_tests <- tibble(
  Test       = c("Breusch-Pagan (heteroskedasticity)", "Durbin-Watson (autocorrelation)"),
  Statistic  = round(c(unname(bp_test$statistic), unname(dw_test$statistic)), 4),
  Parameter  = c(unname(bp_test$parameter), NA_real_),
  p_value    = signif(c(bp_test$p.value, dw_test$p.value), 6)
)

path14 <- "outputs/14_heteroskedasticity_autocorrelation_tests.csv"
write_csv(diag_tests, path14)
track(path14)
cat(sprintf("  Saved: %s\n", path14))
print(as.data.frame(diag_tests))

cat("\n  Computing HC1 heteroskedasticity-robust SEs for Model D...\n")
robust_vcov <- sandwich::vcovHC(modD, type = "HC1")
robust_ct   <- lmtest::coeftest(modD, vcov = robust_vcov)
robust_se_df <- tibble(
  term         = rownames(robust_ct),
  estimate     = round(robust_ct[, 1], 6),
  std.error    = round(robust_ct[, 2], 6),
  statistic    = round(robust_ct[, 3], 4),
  p.value      = signif(robust_ct[, 4], 6),
  std.error_OLS = round(summary(modD)$coefficients[rownames(robust_ct), "Std. Error"], 6)
)

path15 <- "outputs/15_modelD_robust_se.csv"
write_csv(robust_se_df, path15)
track(path15)
cat(sprintf("  Saved: %s\n", path15))
print(as.data.frame(robust_se_df))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Multicollinearity Check (VIF)
#   →  16_vif_modelD.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Multicollinearity Check — VIF (Model D) ===\n")

vif_raw <- car::vif(modD)
if (is.matrix(vif_raw)) {
  vif_df <- as.data.frame(vif_raw)
  vif_df$term <- rownames(vif_df)
  vif_df <- vif_df |> select(term, everything())
} else {
  vif_df <- tibble(term = names(vif_raw), VIF = round(as.numeric(vif_raw), 4))
}

path16 <- "outputs/16_vif_modelD.csv"
write_csv(vif_df, path16)
track(path16)
cat(sprintf("  Saved: %s\n", path16))
print(as.data.frame(vif_df))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Interaction Model — Financial_Rank × League
#   →  17_regression_interaction_FR_League.csv
#   →  19_interaction_slopes_by_league.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Interaction Model — Financial Rank x League ===\n")

modG <- lm(League_Position ~ Financial_Rank * League, data = model_df)
sumG <- summary(modG)
cat(sprintf("  R² = %.4f  |  Adj-R² = %.4f\n", sumG$r.squared, sumG$adj.r.squared))
tidyG <- safe_tidy(modG, "G: OLS — Pos ~ FR * League", "League_Position")

path17 <- "outputs/17_regression_interaction_FR_League.csv"
write_csv(tidyG, path17)
track(path17)
cat(sprintf("  Saved: %s\n", path17))

cat("  Generating plot 19: predicted FR slopes by league\n")
league_levels <- sort(unique(model_df$League))
pred_grid_g <- expand.grid(
  Financial_Rank = seq(min(model_df$Financial_Rank), max(model_df$Financial_Rank), length.out = 50),
  League         = league_levels,
  stringsAsFactors = FALSE
)
pred_g <- predict(modG, newdata = pred_grid_g, se.fit = TRUE)
pred_grid_g <- pred_grid_g |>
  mutate(fit = pred_g$fit, se = pred_g$se.fit,
         lo  = fit - 1.96 * se, hi = fit + 1.96 * se)

p19 <- ggplot(pred_grid_g, aes(x = Financial_Rank, y = fit, colour = League, fill = League)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = league_colours) +
  scale_fill_manual(values = league_colours) +
  labs(
    title    = "Predicted League Position vs Financial Rank, by League (Model G)",
    subtitle = "Interaction model: League_Position ~ Financial_Rank * League  •  Shaded = 95% CI",
    x        = "Financial Rank  (1 = richest squad)",
    y        = "Predicted League Position  (1 = champion)",
    colour   = "League", fill = "League"
  ) +
  theme_seminar

path19 <- "outputs/19_interaction_slopes_by_league.png"
ggsave(path19, p19, width = 10, height = 6.5, dpi = 150)
track(path19)
cat(sprintf("  Saved: %s\n", path19))
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Non-linear (Quadratic) Model — Diminishing Returns
#   →  20_quadratic_model_results.csv
#   →  21_quadratic_fit_plot.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Quadratic Model — Diminishing Returns of Financial Rank ===\n")

modH <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df)
sumH <- summary(modH)
cat(sprintf("  R² = %.4f  |  Adj-R² = %.4f\n", sumH$r.squared, sumH$adj.r.squared))
tidyH <- safe_tidy(modH, "H: OLS — Pos ~ FR + FR^2", "League_Position")

path20 <- "outputs/20_quadratic_model_results.csv"
write_csv(tidyH, path20)
track(path20)
cat(sprintf("  Saved: %s\n", path20))
print(as.data.frame(tidyH))

cat("  Generating plot 21: quadratic fit\n")
fr_seq <- seq(min(model_df$Financial_Rank), max(model_df$Financial_Rank), length.out = 100)
pred_h <- predict(modH, newdata = tibble(Financial_Rank = fr_seq), se.fit = TRUE)
pred_grid_h <- tibble(
  Financial_Rank = fr_seq,
  fit = pred_h$fit, se = pred_h$se.fit,
  lo  = fit - 1.96 * se, hi = fit + 1.96 * se
)

p21 <- ggplot() +
  geom_jitter(data = model_df, aes(x = Financial_Rank, y = League_Position),
              alpha = 0.15, width = 0.3, height = 0.3, size = 1.2, colour = "grey40") +
  geom_ribbon(data = pred_grid_h, aes(x = Financial_Rank, ymin = lo, ymax = hi),
              fill = "firebrick", alpha = 0.2) +
  geom_line(data = pred_grid_h, aes(x = Financial_Rank, y = fit),
            colour = "firebrick", linewidth = 1.2) +
  labs(
    title    = "Quadratic Fit: League Position vs Financial Rank (Model H)",
    subtitle = "Tests whether the financial advantage shows diminishing returns at high ranks",
    x        = "Financial Rank  (1 = richest squad)",
    y        = "League Position  (1 = champion)"
  ) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20)) +
  theme_seminar

path21 <- "outputs/21_quadratic_fit_plot.png"
ggsave(path21, p21, width = 9, height = 6, dpi = 150)
track(path21)
cat(sprintf("  Saved: %s\n", path21))
cat("PHASE 6 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 7: Time-Trend Model — Financial_Rank × Season_Index
#   →  22_time_trend_results.csv
#   →  23_time_trend_plot.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 7: Time-Trend Model — Has Financial Determinism Changed Over Time? ===\n")

modI <- lm(League_Position ~ Financial_Rank * Season_Index, data = model_df)
sumI <- summary(modI)
cat(sprintf("  R² = %.4f  |  Adj-R² = %.4f\n", sumI$r.squared, sumI$adj.r.squared))
tidyI <- safe_tidy(modI, "I: OLS — Pos ~ FR * Season_Index", "League_Position")

path22 <- "outputs/22_time_trend_results.csv"
write_csv(tidyI, path22)
track(path22)
cat(sprintf("  Saved: %s\n", path22))
print(as.data.frame(tidyI))

cat("  Generating plot 23: predicted FR effect, early vs mid vs late seasons\n")
season_rng <- range(model_df$Season_Index, na.rm = TRUE)
season_pts <- c(early = season_rng[1], mid = round(mean(season_rng)), late = season_rng[2])
pred_grid_i <- expand.grid(
  Financial_Rank = seq(min(model_df$Financial_Rank), max(model_df$Financial_Rank), length.out = 50),
  Season_Index   = unname(season_pts)
)
pred_grid_i$Season_Label <- factor(
  pred_grid_i$Season_Index,
  levels = unname(season_pts),
  labels = sprintf("Season_Index = %d (%s)", season_pts, names(season_pts))
)
pred_grid_i$fit <- predict(modI, newdata = pred_grid_i)

p23 <- ggplot(pred_grid_i, aes(x = Financial_Rank, y = fit, colour = Season_Label)) +
  geom_line(linewidth = 1.2) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title    = "Predicted Effect of Financial Rank, Early vs Mid vs Late Seasons (Model I)",
    subtitle = "Interaction model: League_Position ~ Financial_Rank * Season_Index",
    x        = "Financial Rank  (1 = richest squad)",
    y        = "Predicted League Position",
    colour   = NULL
  ) +
  theme_seminar

path23 <- "outputs/23_time_trend_plot.png"
ggsave(path23, p23, width = 9, height = 6, dpi = 150)
track(path23)
cat(sprintf("  Saved: %s\n", path23))
cat("PHASE 7 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 8: Nested Model Significance Tests (ANOVA F-tests)
#   →  18_nested_model_significance_tests.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 8: Nested Model Significance Tests ===\n")

modI_cubic <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2) + I(Financial_Rank^3),
                 data = model_df)

nested_tests <- bind_rows(
  anova_row("Interaction (FR*League) vs Additive (FR+League)", modB, modG),
  anova_row("Quadratic (FR+FR^2) vs Linear (FR)",               modA, modH),
  anova_row("Time-trend (FR*Season) vs Additive (FR+Season)",    modI0, modI),
  anova_row("Cubic (FR+FR^2+FR^3) vs Quadratic (FR+FR^2)",       modH, modI_cubic)
)

path18 <- "outputs/18_nested_model_significance_tests.csv"
write_csv(nested_tests, path18)
track(path18)
cat(sprintf("  Saved: %s\n", path18))
print(as.data.frame(nested_tests))
cat("PHASE 8 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 9: Alternative Outcome — Top-4 (European Qualification) Finish
#   →  24_top4_logit_results.csv
#   →  25_top4_probability_curve.png
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 9: Top-4 Finish Logistic Models ===\n")

modJ <- glm(Top4_binary ~ Financial_Rank, data = model_df, family = binomial())
cat(sprintf("  Model J  Null dev: %.2f  |  Resid dev: %.2f  |  AIC: %.2f  |  McFadden-R²: %.4f\n",
            modJ$null.deviance, modJ$deviance, AIC(modJ),
            1 - modJ$deviance / modJ$null.deviance))
tidyJ <- safe_tidy(modJ, "J: Logit — Top4 ~ FR", "Top4_binary")

modJ2 <- glm(Top4_binary ~ Log_Normalized_Value + League, data = model_df, family = binomial())
cat(sprintf("  Model J2 Null dev: %.2f  |  Resid dev: %.2f  |  AIC: %.2f  |  McFadden-R²: %.4f\n",
            modJ2$null.deviance, modJ2$deviance, AIC(modJ2),
            1 - modJ2$deviance / modJ2$null.deviance))
tidyJ2 <- safe_tidy(modJ2, "J2: Logit full — Top4 ~ Log(MV) + League", "Top4_binary")

top4_results <- bind_rows(tidyJ, tidyJ2) |>
  mutate(across(where(is.numeric) & !c(statistic), ~ round(.x, 6)),
         statistic      = round(statistic, 4),
         significant_05 = !is.na(p.value) & p.value < 0.05)

path24 <- "outputs/24_top4_logit_results.csv"
write_csv(top4_results, path24)
track(path24)
cat(sprintf("  Saved: %s\n", path24))

cat("  Generating plot 25: top-4 probability curve\n")
fr_range  <- range(model_df$Financial_Rank, na.rm = TRUE)
pred_grid_j <- tibble(Financial_Rank = seq(fr_range[1], fr_range[2], by = 0.1))
pred_link_j <- predict(modJ, newdata = pred_grid_j, type = "link", se.fit = TRUE)
pred_grid_j <- pred_grid_j |>
  mutate(
    log_odds = pred_link_j$fit,
    se_link  = pred_link_j$se.fit,
    prob     = plogis(log_odds),
    prob_lo  = plogis(log_odds - 1.96 * se_link),
    prob_hi  = plogis(log_odds + 1.96 * se_link)
  )

obs_prop_j <- model_df |>
  group_by(Financial_Rank) |>
  summarise(Obs_Prob = mean(Top4_binary), N = n(), .groups = "drop")

p25 <- ggplot() +
  geom_ribbon(data = pred_grid_j,
              aes(x = Financial_Rank, ymin = prob_lo, ymax = prob_hi),
              fill = "steelblue", alpha = 0.20) +
  geom_line(data = pred_grid_j,
            aes(x = Financial_Rank, y = prob),
            colour = "steelblue", linewidth = 1.3) +
  geom_point(data = obs_prop_j,
             aes(x = Financial_Rank, y = Obs_Prob, size = N),
             colour = "firebrick", alpha = 0.85) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_size_continuous(range = c(2, 8), breaks = c(5, 10, 20, 50)) +
  labs(
    title    = "Predicted Probability of a Top-4 Finish vs Financial Rank (Model J)",
    subtitle = "Blue band = 95% CI of logistic fit  •  Red dots = observed rate (sized by N)",
    x        = "Financial Rank  (1 = richest squad)",
    y        = "P(Top-4 Finish)",
    size     = "N team-seasons"
  ) +
  theme_seminar

path25 <- "outputs/25_top4_probability_curve.png"
ggsave(path25, p25, width = 9, height = 6, dpi = 150)
track(path25)
cat(sprintf("  Saved: %s\n", path25))
cat("PHASE 9 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 10: Ordinal Logistic Regression — Position Tier
#   →  26_ordinal_logit_results.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 10: Ordinal Logistic Regression — Position Tier ===\n")

modK <- MASS::polr(Position_Tier ~ Financial_Rank + League, data = model_df, Hess = TRUE)
ctable   <- coef(summary(modK))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

ordinal_results <- as.data.frame(ctable)
ordinal_results$term <- rownames(ordinal_results)
ordinal_results <- ordinal_results |>
  rename(estimate = Value, std.error = `Std. Error`, t.value = `t value`) |>
  mutate(
    odds_ratio = round(exp(estimate), 4),
    p.value    = signif(p_values, 6),
    estimate   = round(estimate, 6),
    std.error  = round(std.error, 6),
    t.value    = round(t.value, 4)
  ) |>
  select(term, estimate, std.error, t.value, odds_ratio, p.value)

cat(sprintf("  Residual deviance: %.2f  |  AIC: %.2f\n", modK$deviance, AIC(modK)))
path26 <- "outputs/26_ordinal_logit_results.csv"
write_csv(ordinal_results, path26)
track(path26)
cat(sprintf("  Saved: %s\n", path26))
print(as.data.frame(ordinal_results))
cat("PHASE 10 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 11: Extended Model Comparison
#   →  27_extended_model_comparison.csv
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 11: Extended Model Comparison (AIC / BIC / LogLik) ===\n")

extended_comparison <- bind_rows(
  model_stats_row("D",  modD,  "OLS",   "Pos ~ Log_Norm_Value + League + Season_Index"),
  model_stats_row("F",  modF,  "Logit", "Champion ~ Log_Norm_Value + League"),
  model_stats_row("G",  modG,  "OLS",   "Pos ~ Financial_Rank * League"),
  model_stats_row("H",  modH,  "OLS",   "Pos ~ Financial_Rank + Financial_Rank^2"),
  model_stats_row("I",  modI,  "OLS",   "Pos ~ Financial_Rank * Season_Index"),
  model_stats_row("J",  modJ,  "Logit", "Top4 ~ Financial_Rank"),
  model_stats_row("J2", modJ2, "Logit", "Top4 ~ Log_Norm_Value + League"),
  model_stats_row("K",  modK,  "Ordinal Logit", "Position_Tier ~ Financial_Rank + League")
) |>
  mutate(N = nrow(model_df))

path27 <- "outputs/27_extended_model_comparison.csv"
write_csv(extended_comparison, path27)
track(path27)
cat(sprintf("  Saved: %s\n", path27))
print(as.data.frame(extended_comparison))
cat("PHASE 11 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 12: Final Summary & Output File List
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 12: Final Summary & Output File List ===\n")

cat("\n")
cat("══════════════════════════════════════════════\n")
cat("  KEY EXTENDED RESULTS:\n")
cat(sprintf("    Interaction (FR x League) significant? %s\n",
           ifelse(nested_tests$p_value[1] < 0.05, "YES", "no")))
cat(sprintf("    Quadratic FR term significant?         %s\n",
           ifelse(nested_tests$p_value[2] < 0.05, "YES", "no")))
cat(sprintf("    Time-trend (FR x Season) significant?  %s\n",
           ifelse(nested_tests$p_value[3] < 0.05, "YES", "no")))
cat("══════════════════════════════════════════════\n\n")

cat("=== FINAL FILE LIST: outputs/ ===\n")
all_out <- list.files("outputs", full.names = TRUE, recursive = FALSE)

if (length(all_out) == 0L) {
  cat("  WARNING: No files found in outputs/\n")
} else {
  fi      <- file.info(all_out)
  out_df  <- data.frame(
    File     = basename(all_out),
    Size_KB  = round(fi$size / 1024, 1),
    Modified = format(fi$mtime, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  ) |>
    arrange(File)
  cat(sprintf("  Total files in outputs/ : %d\n", nrow(out_df)))
  cat(sprintf("  Total size              : %.1f KB\n\n", sum(out_df$Size_KB)))
  print(out_df, row.names = FALSE, right = FALSE)
}

cat(sprintf("\n  This script (05b) tracked %d new output files:\n", length(OUTPUT_FILES)))
for (f in sort(OUTPUT_FILES)) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}

n_found <- length(all_out)
n_expected_new <- 14L
cat(sprintf("\n  Expected new files from this script: %d\n", n_expected_new))
if (length(OUTPUT_FILES) < n_expected_new) {
  warning(sprintf("Only %d new file(s) tracked — expected %d.",
                  length(OUTPUT_FILES), n_expected_new))
} else {
  cat("  CHECK PASSED: all expected new output files present.\n")
}
cat("\n05b_extended_regression.R complete.\n")
