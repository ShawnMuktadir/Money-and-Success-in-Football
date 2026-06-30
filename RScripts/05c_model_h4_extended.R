# ==============================================================
# 05c_model_h4_extended.R
# Extends Model H by adding League fixed effects and Season_Index.
# Produces three output CSVs for use in Section 9.4 of the report.
#
# Outputs (all saved to outputs/):
#   E1_model_comparison_H4.csv   - 4-model AIC/R² comparison table
#   E2_ftest_H4.csv              - 5-row nested F-test table
#   E3_h4_coefficients.csv       - Full H4 coefficient table
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr", "broom"))

make_dir("outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }

safe_tidy <- function(model, label, response, ...) {
  out <- tidy(model, conf.int = FALSE, ...)
  ci  <- tryCatch(
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
    df_Model1   = a$Res.Df[1],
    df_Model2   = a$Res.Df[2],
    df_diff     = abs(a$Df[2]),
    RSS_Model1  = round(a$RSS[1], 4),
    RSS_Model2  = round(a$RSS[2], 4),
    F_statistic = round(a$F[2], 4),
    p_value     = signif(a[["Pr(>F)"]][2], 6)
  )
}


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load Dataset
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load Dataset ===\n")

master_path <- "data/merged/master_dataset.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset.csv — run script 04 first.")

master <- read_csv(master_path, show_col_types = FALSE)

model_df <- master |>
  filter(
    !is.na(League_Position),
    !is.na(Financial_Rank),
    !is.na(Market_Value_M_EUR),
    !is.na(Log_Normalized_Value)
  ) |>
  mutate(
    League = factor(as.character(League)),
    Champion_bin = as.integer(
      tolower(as.character(Champion)) %in% c("yes", "true", "1")
    )
  )

cat(sprintf("  Complete-case rows for modelling: %d\n", nrow(model_df)))
cat(sprintf("  Leagues: %s\n", paste(levels(model_df$League), collapse = ", ")))
cat(sprintf("  Season_Index range: %d – %d\n",
            min(model_df$Season_Index), max(model_df$Season_Index)))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Fit Model H Family
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Fit Models H, H2, H3, H4 ===\n")

# Model H  — baseline quadratic (preferred from 05)
modH  <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2),
            data = model_df)
sumH  <- summary(modH)
cat(sprintf("  H  (FR + FR²):              R² = %.4f  Adj-R² = %.4f  AIC = %.2f\n",
            sumH$r.squared, sumH$adj.r.squared, AIC(modH)))

# Model H2 — add League fixed effects
modH2 <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2) + League,
            data = model_df)
sumH2 <- summary(modH2)
cat(sprintf("  H2 (FR + FR² + League):     R² = %.4f  Adj-R² = %.4f  AIC = %.2f\n",
            sumH2$r.squared, sumH2$adj.r.squared, AIC(modH2)))

# Model H3 — add Season_Index only
modH3 <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2) + Season_Index,
            data = model_df)
sumH3 <- summary(modH3)
cat(sprintf("  H3 (FR + FR² + Season):     R² = %.4f  Adj-R² = %.4f  AIC = %.2f\n",
            sumH3$r.squared, sumH3$adj.r.squared, AIC(modH3)))

# Model H4 — add both League + Season_Index (full extended)
modH4 <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2) + League + Season_Index,
            data = model_df)
sumH4 <- summary(modH4)
cat(sprintf("  H4 (FR + FR² + League + Season): R² = %.4f  Adj-R² = %.4f  AIC = %.2f\n",
            sumH4$r.squared, sumH4$adj.r.squared, AIC(modH4)))
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Table E1 — Model Comparison
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Table E1 — Model Comparison ===\n")

e1 <- tibble(
  Model   = c("H", "H2", "H3", "H4"),
  Formula = c(
    "FR + FR²",
    "FR + FR² + League",
    "FR + FR² + Season",
    "FR + FR² + League + Season"
  ),
  N      = nrow(model_df),
  R2     = round(c(sumH$r.squared,  sumH2$r.squared,
                   sumH3$r.squared, sumH4$r.squared), 4),
  Adj_R2 = round(c(sumH$adj.r.squared,  sumH2$adj.r.squared,
                   sumH3$adj.r.squared, sumH4$adj.r.squared), 4),
  AIC    = round(c(AIC(modH), AIC(modH2), AIC(modH3), AIC(modH4)), 2),
  BIC    = round(c(BIC(modH), BIC(modH2), BIC(modH3), BIC(modH4)), 2)
)

path_e1 <- "outputs/E1_model_comparison_H4.csv"
write_csv(e1, path_e1)
track(path_e1)
cat(sprintf("  Saved: %s\n", path_e1))
print(as.data.frame(e1))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Table E2 — Nested F-tests (5 comparisons)
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Table E2 — Nested F-tests ===\n")

e2 <- bind_rows(
  anova_row("H2 vs H: Adding League FE",              modH,  modH2),
  anova_row("H3 vs H: Adding Season_Index",            modH,  modH3),
  anova_row("H4 vs H: Adding League + Season (joint)", modH,  modH4),
  anova_row("H4 vs H2: Season beyond League",          modH2, modH4),
  anova_row("H4 vs H3: League beyond Season",          modH3, modH4)
)

path_e2 <- "outputs/E2_ftest_H4.csv"
write_csv(e2, path_e2)
track(path_e2)
cat(sprintf("  Saved: %s\n", path_e2))
print(as.data.frame(e2))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Table E3 — H4 Full Coefficient Table
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Table E3 — H4 Coefficients ===\n")

tidyH4 <- safe_tidy(modH4,
                    "H4: OLS — Pos ~ FR + FR^2 + League + Season",
                    "League_Position")

path_e3 <- "outputs/E3_h4_coefficients.csv"
write_csv(tidyH4, path_e3)
track(path_e3)
cat(sprintf("  Saved: %s\n", path_e3))
print(as.data.frame(tidyH4))
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Validation Summary for Report
# ══════════════════════════════════════════════════════════════
cat("═══════════════════════════════════════════════════════════\n")
cat("  VALIDATION CHECKLIST SUMMARY\n")
cat("═══════════════════════════════════════════════════════════\n")

aic_H  <- round(AIC(modH),  2)
aic_H4 <- round(AIC(modH4), 2)
r2_H   <- round(sumH$r.squared,  4)
r2_H4  <- round(sumH4$r.squared, 4)

cat(sprintf("  Model H  AIC: %.2f  R² = %.4f\n", aic_H,  r2_H))
cat(sprintf("  Model H4 AIC: %.2f  R² = %.4f\n", aic_H4, r2_H4))
cat(sprintf("  AIC winner: %s  (diff = %.2f)\n",
            ifelse(aic_H4 < aic_H, "H4 (extended model)", "H (baseline)"),
            abs(aic_H - aic_H4)))

cat("\n  F-test — H4 vs H (joint test of League + Season):\n")
h4_vs_h <- e2 |> filter(grepl("joint", Comparison))
cat(sprintf("    F(%d) = %.4f,  p = %.6f  — %s\n",
            h4_vs_h$df_diff,
            h4_vs_h$F_statistic,
            h4_vs_h$p_value,
            ifelse(h4_vs_h$p_value < 0.05, "SIGNIFICANT (p < 0.05)", "not significant")))

cat("\n  F-test — adding League (H2 vs H):\n")
h2_vs_h <- e2 |> filter(grepl("Adding League", Comparison))
cat(sprintf("    F(%d) = %.4f,  p = %.6f  — %s\n",
            h2_vs_h$df_diff,
            h2_vs_h$F_statistic,
            h2_vs_h$p_value,
            ifelse(h2_vs_h$p_value < 0.05, "SIGNIFICANT", "not significant")))

cat("\n  F-test — adding Season (H3 vs H):\n")
h3_vs_h <- e2 |> filter(grepl("Adding Season", Comparison))
cat(sprintf("    F(%d) = %.4f,  p = %.6f  — %s\n",
            h3_vs_h$df_diff,
            h3_vs_h$F_statistic,
            h3_vs_h$p_value,
            ifelse(h3_vs_h$p_value < 0.05, "SIGNIFICANT", "not significant")))

cat("\n  Season_Index coefficient in Model H4:\n")
season_row <- tidyH4 |> filter(term == "Season_Index")
cat(sprintf("    Estimate = %.4f,  SE = %.4f,  t = %.4f,  p = %.6f  — %s\n",
            season_row$estimate, season_row$std.error,
            season_row$statistic, season_row$p.value,
            ifelse(season_row$p.value < 0.05, "SIGNIFICANT", "not significant")))

cat("\n  League fixed effects in Model H4 (any significant?):\n")
league_rows <- tidyH4 |> filter(grepl("^League", term))
for (i in seq_len(nrow(league_rows))) {
  cat(sprintf("    %-30s  est=%.4f  p=%.6f  %s\n",
              league_rows$term[i], league_rows$estimate[i], league_rows$p.value[i],
              ifelse(league_rows$p.value[i] < 0.05, "***", "")))
}
cat(sprintf("  Any League FE significant? %s\n",
            ifelse(any(league_rows$p.value < 0.05), "YES", "NO")))

cat("═══════════════════════════════════════════════════════════\n")
cat("\n05c_model_h4_extended.R complete.\n")
