# ==============================================================
# 09_points_model.R
# Re-fits Model H using Points instead of League_Position as the
# outcome (higher points = better, so the FR slope should flip to
# negative). Checks whether the residual-based outlier ranking
# changes when measured in points rather than final position.
#
# Outputs (all saved to outputs/):
#   39_points_model_scatter.png
#   39_points_model_residuals.csv
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
ensure_installed(c("ggrepel"))

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

league_colours <- c(
  "Bundesliga"     = "#E41A1C",
  "La Liga"        = "#FF7F00",
  "Ligue 1"        = "#4DAF4A",
  "Premier League" = "#984EA3",
  "Serie A"        = "#377EB8"
)


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load data & fit points-based Model H
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load Data & Fit Points Model ===\n")

master_path <- "data/merged/master_dataset.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset.csv — run script 04 first.")

master <- read_csv(master_path, show_col_types = FALSE) |>
  mutate(League = factor(as.character(League)))

model_df <- master |>
  filter(!is.na(Financial_Rank), !is.na(Points))

cat(sprintf("  Complete-case rows: %d\n", nrow(model_df)))

modH_points <- lm(Points ~ Financial_Rank + I(Financial_Rank^2), data = model_df)
sumH_points <- summary(modH_points)

cat(sprintf("  Points ~ FR + FR^2:  R2 = %.4f  Adj-R2 = %.4f  AIC = %.2f\n",
            sumH_points$r.squared, sumH_points$adj.r.squared, AIC(modH_points)))
print(coef(sumH_points))

coefs_tbl <- tidy(modH_points, conf.int = TRUE) |>
  mutate(response = "Points", .before = 1)

path_coef <- "outputs/39_points_model_coefficients.csv"
write_csv(coefs_tbl, path_coef)
track(path_coef)
cat(sprintf("  Saved: %s\n", path_coef))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Compute residuals & compare outlier ranking
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Residuals & Outlier Ranking ===\n")

aug_df <- augment(modH_points, data = model_df) |>
  rename(Fitted_Points = .fitted, Residual_Points = .resid) |>
  mutate(Abs_Residual_Points = abs(Residual_Points)) |>
  arrange(desc(Abs_Residual_Points)) |>
  mutate(Rank_Points_Residual = row_number())

# For comparison: residuals from the original position-based Model H
modH_position <- lm(League_Position ~ Financial_Rank + I(Financial_Rank^2), data = model_df)
aug_position <- augment(modH_position, data = model_df) |>
  rename(Fitted_Position = .fitted, Residual_Position = .resid) |>
  mutate(Abs_Residual_Position = abs(Residual_Position)) |>
  arrange(desc(Abs_Residual_Position)) |>
  mutate(Rank_Position_Residual = row_number()) |>
  select(League, Season, Team, Rank_Position_Residual, Residual_Position)

comparison_df <- aug_df |>
  left_join(aug_position, by = c("League", "Season", "Team")) |>
  select(League, Season, Team, Financial_Rank, Points, League_Position,
         Fitted_Points, Residual_Points, Abs_Residual_Points, Rank_Points_Residual,
         Residual_Position, Rank_Position_Residual)

path_resid <- "outputs/39_points_model_residuals.csv"
write_csv(comparison_df, path_resid)
track(path_resid)
cat(sprintf("  Saved: %s\n", path_resid))

top10_points <- head(comparison_df, 10)
cat("\n  Top 10 largest |residual| — Points model:\n")
print(as.data.frame(top10_points[, c("Team", "Season", "Financial_Rank", "Points",
                                     "Residual_Points", "Rank_Position_Residual")]))

leicester_row <- comparison_df |>
  filter(grepl("Leicester", Team), Season == "2015-2016")
cat(sprintf("\n  Leicester City 2015-16: Points-residual rank = %d (Residual = %.2f)  |  Position-residual rank = %d\n",
            leicester_row$Rank_Points_Residual, leicester_row$Residual_Points,
            leicester_row$Rank_Position_Residual))
cat(sprintf("  Leicester still has the single largest points residual? %s\n",
            ifelse(leicester_row$Rank_Points_Residual == 1, "YES", "NO")))

new_top10_points_only <- setdiff(top10_points$Team, head(comparison_df |> arrange(Rank_Position_Residual), 10)$Team)
cat(sprintf("\n  Clubs newly appearing in points-model top 10 (not in position-model top 10): %s\n",
            ifelse(length(new_top10_points_only) == 0, "none", paste(new_top10_points_only, collapse = ", "))))
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Scatter plot with quadratic fit + story-club labels
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Scatter Plot (39_points_model_scatter.png) ===\n")

fr_seq <- seq(1, max(model_df$Financial_Rank, na.rm = TRUE), length.out = 300)
quad_pred <- predict(modH_points, newdata = data.frame(Financial_Rank = fr_seq),
                     interval = "confidence", level = 0.95) |>
  as.data.frame() |>
  mutate(Financial_Rank = fr_seq)

label_df <- aug_df |>
  filter(
    (grepl("Leicester",  Team) & Season == "2015-2016") |
    (grepl("Leverkusen", Team) & Season == "2023-2024") |
    (grepl("Lille",      Team) & Season == "2020-2021")
  ) |>
  mutate(Label = paste0(Team, "\n", Season))

p39 <- ggplot() +
  geom_ribbon(data = quad_pred, aes(x = Financial_Rank, ymin = lwr, ymax = upr),
              fill = "#2C3E73", alpha = 0.12) +
  geom_jitter(data = aug_df, aes(x = Financial_Rank, y = Points, colour = League),
              alpha = 0.35, size = 1.3, width = 0.18, height = 0.5) +
  geom_line(data = quad_pred, aes(x = Financial_Rank, y = fit),
            colour = "#2C3E73", linewidth = 1.4) +
  geom_point(data = label_df, aes(x = Financial_Rank, y = Points),
             colour = "#F39C12", fill = "#F39C12", shape = 23, size = 5, stroke = 0.8) +
  geom_label_repel(
    data = label_df,
    aes(x = Financial_Rank, y = Points, label = Label),
    fill = "white", colour = "#2C3E50", size = 3.1, fontface = "bold",
    label.size = 0.3, label.r = unit(0.2, "lines"),
    box.padding = 0.6, point.padding = 0.4,
    segment.colour = "#F39C12", segment.size = 0.55,
    min.segment.length = 0.2, max.overlaps = 20
  ) +
  scale_colour_manual(values = league_colours) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), labels = function(x) paste0("FR ", x)) +
  labs(
    title    = "Richer Squads Earn More Points — But Points Tell a Sharper Story",
    subtitle = sprintf(
      "N = %d team-seasons  •  R2 = %.3f  •  Model H quadratic fit  •  Gold = key story clubs",
      nrow(aug_df), sumH_points$r.squared
    ),
    x       = "Financial Rank within league-season (FR 1 = richest squad)",
    y       = "Points won that season",
    colour  = "League",
    caption = "Financial Rank normalised within each league-season. Points ~ Financial_Rank + Financial_Rank^2."
  ) +
  theme_clean

path39 <- "outputs/39_points_model_scatter.png"
ggsave(path39, p39, width = 12, height = 7.5, dpi = 150)
track(path39)
cat(sprintf("  Saved: %s\n", path39))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Output inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Output Inventory ===\n\n")
cat(sprintf("  Script produced %d output file(s):\n", length(OUTPUT_FILES)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("    %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n09_points_model.R complete.\n")
