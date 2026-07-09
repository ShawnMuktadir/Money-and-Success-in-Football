# ==============================================================
# 15_spal_correction.R
# Fixes the single missing-value merge artifact identified in
# scripts 12-14: SPAL (Serie A, 2017-18) never matched a
# Transfermarkt name during the standings/market-value merge in
# RScripts/04, leaving Market_Value_M_EUR and Financial_Rank as NA
# for that one team-season. This inserts SPAL's real squad value
# (Transfermarkt 17/18 squad page, total €63.55M across all
# positions), recomputes Financial_Rank for Serie A 2017-18 only,
# and re-checks whether any "genuine" inflation-adjustment rank
# changes survive once the gap is filled.
#
# Outputs:
#   data/merged/master_dataset_corrected.csv   (original master_dataset.csv untouched)
#   extensions/outputs/E6_SPAL_correction_summary.csv
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "tidyr"))

make_dir("extensions/outputs")
OUTPUT_FILES <- character(0)
track <- function(path) { OUTPUT_FILES <<- c(OUTPUT_FILES, path); invisible(path) }

season_order <- paste0(2015:2024, "-", 2016:2025)

SPAL_MARKET_VALUE <- 63.55  # €M — Transfermarkt SPAL squad page, 2017-18 season total


# ══════════════════════════════════════════════════════════════
# PHASE 1: Load master dataset & locate the SPAL row
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Load Master Dataset & Locate SPAL ===\n")

master_path <- "data/merged/master_dataset.csv"
if (!file.exists(master_path)) stop("Cannot find master_dataset.csv — run RScripts/04 first.")

master <- read_csv(master_path, show_col_types = FALSE) |>
  mutate(League = as.character(League))

spal_before <- master |> filter(League == "Serie A", Season == "2017-2018", grepl("SPAL", Team))
if (nrow(spal_before) != 1) stop("Expected exactly 1 SPAL row for Serie A 2017-2018.")

cat("  SPAL row BEFORE correction:\n")
print(as.data.frame(spal_before))
cat(sprintf("\n  Total rows with missing Market_Value_M_EUR in whole dataset: %d\n",
            sum(is.na(master$Market_Value_M_EUR))))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Insert SPAL's market value & recompute Financial_Rank
#           for Serie A 2017-18 ONLY
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Insert Value & Recompute Serie A 2017-18 Financial_Rank ===\n")

master_corrected <- master |>
  mutate(
    Market_Value_M_EUR = ifelse(
      League == "Serie A" & Season == "2017-2018" & grepl("SPAL", Team),
      SPAL_MARKET_VALUE, Market_Value_M_EUR
    )
  ) |>
  group_by(League, Season) |>
  mutate(
    is_target_group      = League == "Serie A" & Season == "2017-2018",
    Financial_Rank        = ifelse(is_target_group,
                                    rank(-Market_Value_M_EUR, ties.method = "min"),
                                    Financial_Rank),
    Normalized_Value       = ifelse(is_target_group,
                                    Market_Value_M_EUR / max(Market_Value_M_EUR, na.rm = TRUE),
                                    Normalized_Value),
    Log_Normalized_Value   = ifelse(is_target_group, log(Normalized_Value), Log_Normalized_Value)
  ) |>
  ungroup() |>
  select(-is_target_group)

spal_after <- master_corrected |> filter(League == "Serie A", Season == "2017-2018", grepl("SPAL", Team))
cat("\n  SPAL row AFTER correction:\n")
print(as.data.frame(spal_after))

cat("\n  --- Full Serie A 2017-18 ranking, FR 1 to FR 20 (post-correction) ---\n")
serie_a_1718 <- master_corrected |>
  filter(League == "Serie A", Season == "2017-2018") |>
  select(Financial_Rank, Team, Market_Value_M_EUR, League_Position, Points) |>
  arrange(Financial_Rank)
print(as.data.frame(serie_a_1718), row.names = FALSE)
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Save corrected master dataset (original preserved)
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Save Corrected Master Dataset ===\n")

path_corrected <- "data/merged/master_dataset_corrected.csv"
write_csv(master_corrected, path_corrected)
cat(sprintf("  Saved: %s  (original %s left untouched)\n", path_corrected, master_path))
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Re-run inflation adjustment check on corrected data
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Re-run Inflation Adjustment Check ===\n")

run_inflation_check <- function(df, label) {
  season_avg <- df |>
    filter(!is.na(Market_Value_M_EUR)) |>
    group_by(Season) |>
    summarise(Avg_Market_Value_M_EUR = mean(Market_Value_M_EUR, na.rm = TRUE), .groups = "drop") |>
    mutate(Season = factor(Season, levels = season_order)) |>
    arrange(Season)

  base_avg <- season_avg$Avg_Market_Value_M_EUR[season_avg$Season == "2019-2020"]
  season_avg <- season_avg |> mutate(Inflation_Index = round(Avg_Market_Value_M_EUR / base_avg, 4))

  df <- df |>
    left_join(season_avg |> mutate(Season = as.character(Season)) |> select(Season, Inflation_Index),
              by = "Season") |>
    mutate(Adjusted_Market_Value_M_EUR = Market_Value_M_EUR / Inflation_Index) |>
    group_by(League, Season) |>
    mutate(Financial_Rank_Adjusted = ifelse(
      is.na(Adjusted_Market_Value_M_EUR), NA_real_,
      rank(-Adjusted_Market_Value_M_EUR, ties.method = "min")
    )) |>
    ungroup()

  rank_compare <- df |>
    filter(!is.na(Financial_Rank), !is.na(Financial_Rank_Adjusted)) |>
    mutate(Rank_Changed = Financial_Rank != Financial_Rank_Adjusted)

  n_changed <- sum(rank_compare$Rank_Changed)
  cat(sprintf("  [%s] Team-seasons compared: %d  |  Genuine rank changes: %d\n",
              label, nrow(rank_compare), n_changed))

  if (n_changed > 0) {
    print(as.data.frame(
      rank_compare |> filter(Rank_Changed) |>
        select(Team, Season, League, Financial_Rank, Financial_Rank_Adjusted)
    ))
  }

  list(df = df, rank_compare = rank_compare, n_changed = n_changed)
}

cat("  BEFORE correction (original master_dataset.csv):\n")
before_check <- run_inflation_check(master, "BEFORE fix")

cat("\n  AFTER correction (master_dataset_corrected.csv):\n")
after_check <- run_inflation_check(master_corrected, "AFTER fix")

cat(sprintf("\n  RESULT: genuine rank changes went from %d (before) to %d (after) the SPAL fix.\n",
            before_check$n_changed, after_check$n_changed))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: E6 — one-page summary of the fix
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: E6 Summary CSV ===\n")

affected_teams <- before_check$rank_compare |>
  filter(Rank_Changed) |>
  distinct(Team, Season, League) |>
  bind_rows(tibble(Team = "SPAL", Season = "2017-2018", League = "Serie A"))

before_lookup <- master |>
  semi_join(affected_teams, by = c("Team", "Season", "League")) |>
  select(Team, Season, League, Market_Value_M_EUR, Financial_Rank) |>
  rename(Market_Value_Before = Market_Value_M_EUR, Financial_Rank_Before = Financial_Rank)

before_adj_lookup <- before_check$rank_compare |>
  select(Team, Season, League, Financial_Rank_Adjusted) |>
  rename(Financial_Rank_Adjusted_Before = Financial_Rank_Adjusted)

after_lookup <- master_corrected |>
  semi_join(affected_teams, by = c("Team", "Season", "League")) |>
  select(Team, Season, League, Market_Value_M_EUR, Financial_Rank) |>
  rename(Market_Value_After = Market_Value_M_EUR, Financial_Rank_After = Financial_Rank)

after_adj_lookup <- after_check$rank_compare |>
  select(Team, Season, League, Financial_Rank_Adjusted) |>
  rename(Financial_Rank_Adjusted_After = Financial_Rank_Adjusted)

e6_summary <- affected_teams |>
  left_join(before_lookup,     by = c("Team", "Season", "League")) |>
  left_join(before_adj_lookup, by = c("Team", "Season", "League")) |>
  left_join(after_lookup,      by = c("Team", "Season", "League")) |>
  left_join(after_adj_lookup,  by = c("Team", "Season", "League")) |>
  mutate(
    Note = ifelse(
      Team == "SPAL",
      "Root cause: unmatched during 04_merge — Market_Value_M_EUR & Financial_Rank were NA",
      "Rank was compacted by 1 because SPAL was excluded from the adjusted-rank calc; now matches its true Financial_Rank"
    )
  ) |>
  arrange(desc(Team == "SPAL"), Financial_Rank_After)

path_e6 <- "extensions/outputs/E6_SPAL_correction_summary.csv"
write_csv(e6_summary, path_e6)
track(path_e6)
cat(sprintf("  Saved: %s\n", path_e6))
print(as.data.frame(e6_summary), row.names = FALSE)
cat("PHASE 5 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 6: Output Inventory
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 6: Output Inventory ===\n\n")
cat(sprintf("  data/merged/master_dataset_corrected.csv  (%d rows x %d cols)\n",
            nrow(master_corrected), ncol(master_corrected)))
for (f in OUTPUT_FILES) {
  size_kb <- round(file.info(f)$size / 1024, 1)
  cat(sprintf("  %-55s  %6.1f KB\n", f, size_kb))
}
cat("\n15_spal_correction.R complete.\n")
