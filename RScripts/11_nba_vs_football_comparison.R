# ==============================================================
# 11_nba_vs_football_comparison.R
# Cross-sport robustness check: does the Model H relationship
# (money predicts final standing) hold in the NBA, a salary-cap
# league, the same way it holds in Europe's top-5 football leagues?
#
# Outputs (all saved to outputs/):
#   40_nba_vs_football_comparison.png
#   40_nba_vs_football_summary.csv
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr", "broom", "ggplot2", "scales"))

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
ensure_installed(c("patchwork"))

make_dir("outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 10,   colour = "grey45", margin = margin(b = 10)),
    plot.caption      = element_text(size = 8.5,  colour = "grey55", hjust = 0, margin = margin(t = 10)),
    axis.title        = element_text(size = 10.5, colour = "grey30"),
    axis.text         = element_text(size = 9.5,  colour = "grey40"),
    panel.grid.major  = element_line(colour = "grey92", linewidth = 0.5),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = margin(14, 14, 10, 14)
  )


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load both datasets & fit Model H equivalents
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load Data & Fit Models ===\n")

master_path <- "data/merged/master_dataset_corrected.csv"
nba_path    <- "data/merged/nba_master_dataset.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset_corrected.csv — run extensions/scripts/15_spal_correction.R first.")
if (!file.exists(nba_path))    stop("Cannot find nba_master_dataset.csv — run script 10 first.")

football <- read_csv(master_path, show_col_types = FALSE) |>
  filter(!is.na(Financial_Rank), !is.na(League_Position))

nba <- read_csv(nba_path, show_col_types = FALSE) |>
  filter(!is.na(Payroll_Rank), !is.na(Final_Standing))

modH_football <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = football)
modH_nba      <- lm(Final_Standing  ~ Payroll_Rank    + I(Payroll_Rank^2),  data = nba)

sum_football <- summary(modH_football)
sum_nba      <- summary(modH_nba)

r_football <- cor(football$Financial_Rank, football$League_Position)
r_nba      <- cor(nba$Payroll_Rank, nba$Final_Standing)

cat(sprintf("  Football: N = %d  r = %.4f  R2 = %.4f  AIC = %.2f\n",
            nrow(football), r_football, sum_football$r.squared, AIC(modH_football)))
cat(sprintf("  NBA:      N = %d  r = %.4f  R2 = %.4f  AIC = %.2f\n",
            nrow(nba), r_nba, sum_nba$r.squared, AIC(modH_nba)))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Champion summary stats for both sports
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Champion Summary Stats ===\n")

football_champs <- football |> filter(Champion == "Yes")
football_unique_champs <- n_distinct(football_champs$Team)
football_pct_top3 <- mean(football_champs$Financial_Rank <= 3) * 100

nba_champs <- nba |> filter(Champion == "Yes")
nba_unique_champs <- n_distinct(nba_champs$Team)
nba_pct_top3 <- mean(nba_champs$Payroll_Rank <= 3) * 100

cat(sprintf("  Football: %d champion-seasons, %d unique clubs, %.1f%% from FR top 3\n",
            nrow(football_champs), football_unique_champs, football_pct_top3))
cat(sprintf("  NBA:      %d champion-seasons, %d unique clubs, %.1f%% from Payroll top 3\n",
            nrow(nba_champs), nba_unique_champs, nba_pct_top3))
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Summary table (40_nba_vs_football_summary.csv)
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Build Summary Table ===\n")

summary_tbl <- tibble(
  Sport                    = c("Football (Top-5 European Leagues)", "NBA"),
  N                        = c(nrow(football), nrow(nba)),
  Pearson_r                = round(c(r_football, r_nba), 4),
  R2                       = round(c(sum_football$r.squared, sum_nba$r.squared), 4),
  AIC                      = round(c(AIC(modH_football), AIC(modH_nba)), 2),
  Unique_Champions         = c(football_unique_champs, nba_unique_champs),
  Pct_Top3_Payroll_Titles  = round(c(football_pct_top3, nba_pct_top3), 1)
)

path_summary <- "outputs/40_nba_vs_football_summary.csv"
write_csv(summary_tbl, path_summary)
track(path_summary)
cat(sprintf("  Saved: %s\n", path_summary))
print(as.data.frame(summary_tbl))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Two-panel comparison plot (40_nba_vs_football_comparison.png)
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Build Comparison Plot ===\n")

fr_seq <- seq(1, max(football$Financial_Rank, na.rm = TRUE), length.out = 300)
pred_football <- predict(modH_football, newdata = data.frame(Financial_Rank = fr_seq),
                         interval = "confidence", level = 0.95) |>
  as.data.frame() |>
  mutate(Financial_Rank = fr_seq)

pr_seq <- seq(1, max(nba$Payroll_Rank, na.rm = TRUE), length.out = 300)
pred_nba <- predict(modH_nba, newdata = data.frame(Payroll_Rank = pr_seq),
                    interval = "confidence", level = 0.95) |>
  as.data.frame() |>
  mutate(Payroll_Rank = pr_seq)

p_football <- ggplot() +
  geom_ribbon(data = pred_football, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill = "#2C3E73", alpha = 0.15) +
  geom_jitter(data = football, aes(x = Financial_Rank, y = League_Position),
              colour = "#2C3E73", alpha = 0.25, size = 1.1, width = 0.18, height = 0.18) +
  geom_line(data = pred_football, aes(x = Financial_Rank, y = fit),
            colour = "#2C3E73", linewidth = 1.3) +
  annotate("label", x = max(fr_seq) * 0.62, y = 2,
           label = sprintf("Pearson r = %.2f\nR² = %.3f", r_football, sum_football$r.squared),
           colour = "#2C3E73", fill = "white", size = 3.4,
           fontface = "bold", hjust = 0) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
  labs(
    title = "European Football",
    subtitle = sprintf("N = %d team-seasons  •  5 leagues, no salary cap", nrow(football)),
    x = "Financial Rank (1 = richest)",
    y = "Final league position"
  ) +
  theme_clean

p_nba <- ggplot() +
  geom_ribbon(data = pred_nba, aes(x = Payroll_Rank, ymin = lwr, ymax = upr),
              fill = "#C0392B", alpha = 0.15) +
  geom_jitter(data = nba, aes(x = Payroll_Rank, y = Final_Standing),
              colour = "#C0392B", alpha = 0.35, size = 1.3, width = 0.18, height = 0.18) +
  geom_line(data = pred_nba, aes(x = Payroll_Rank, y = fit),
            colour = "#C0392B", linewidth = 1.3) +
  annotate("label", x = max(pr_seq) * 0.62, y = 3,
           label = sprintf("Pearson r = %.2f\nR² = %.3f", r_nba, sum_nba$r.squared),
           colour = "#C0392B", fill = "white", size = 3.4,
           fontface = "bold", hjust = 0) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  labs(
    title = "NBA",
    subtitle = sprintf("N = %d team-seasons  •  1 league, hard salary cap", nrow(nba)),
    x = "Payroll Rank (1 = highest payroll)",
    y = "Final regular-season standing"
  ) +
  theme_clean

p_combined <- p_football + p_nba +
  patchwork::plot_annotation(
    title = "Does Money Buy Success? Football vs. the Salary-Capped NBA",
    subtitle = "Same quadratic specification (Model H) fit separately on each sport — tighter scatter and higher R² means money predicts finish more strongly",
    caption = "Football: Financial_Rank + Financial_Rank² predicting League_Position (2015-16 to 2024-25).  NBA: Payroll_Rank + Payroll_Rank² predicting Final_Standing (regular-season, pooled across conferences, same seasons).",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(size = 11, colour = "grey40", hjust = 0, margin = margin(b = 8)),
      plot.caption  = element_text(size = 8.5, colour = "grey55", hjust = 0, margin = margin(t = 8))
    )
  )

path_plot <- "outputs/40_nba_vs_football_comparison.png"
ggsave(path_plot, p_combined, width = 13, height = 6.5, dpi = 150)
track(path_plot)
cat(sprintf("  Saved: %s\n", path_plot))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Output inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s):\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n11_nba_vs_football_comparison.R complete.\n")
