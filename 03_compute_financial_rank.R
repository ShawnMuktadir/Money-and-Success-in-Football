# ==============================================================
# 03_compute_financial_rank.R
# Reads raw Transfermarkt data from 02_, recomputes Financial_Rank
# properly (rank with ties), renames value column, and saves in
# a 4-level CSV structure.
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr"))

# ── Read and rename ────────────────────────────────────────────
raw <- read_csv("data/transfermarkt_market_values_2015_2025.csv",
                show_col_types = FALSE) |>
  select(-Financial_Rank)   # drop old rank — recompute correctly below

# ── Compute Financial_Rank ─────────────────────────────────────
# Within each League + Season: rank 1 = richest squad
# ties.method = "min" so tied clubs share the lower rank number
mv <- raw |>
  group_by(League, Season) |>
  mutate(Financial_Rank = rank(-Market_Value_M_EUR, ties.method = "min")) |>
  ungroup() |>
  select(League, Season, Financial_Rank, Team, Market_Value_M_EUR) |>
  arrange(League, Season, Financial_Rank)

leagues <- unique(mv$League)
seasons <- sort(unique(mv$Season))

# ── Level 1 — master combined (1 file) ────────────────────────
make_dir("data/market_values")
l1 <- save_csv(mv, "data/market_values/all_leagues_all_seasons.csv")
cat("L1:", l1, "\n")

# ── Level 2 — one file per league, all seasons (5 files) ──────
l2_files <- character(length(leagues))
for (i in seq_along(leagues)) {
  lg <- leagues[i]
  p  <- file.path("data/market_values", safe_name(lg))
  make_dir(p)
  l2_files[i] <- save_csv(filter(mv, League == lg),
                           file.path(p, "all_seasons.csv"))
}
cat("L2:", length(l2_files), "files\n")

# ── Level 3 — one file per season, all leagues (10 files) ─────
make_dir("data/market_values/by_season")
l3_files <- character(length(seasons))
for (i in seq_along(seasons)) {
  s <- seasons[i]
  l3_files[i] <- save_csv(filter(mv, Season == s),
                           file.path("data/market_values/by_season",
                                     paste0(s, ".csv")))
}
cat("L3:", length(l3_files), "files\n")

# ── Level 4 — one file per league × season (50 files) ─────────
l4_files <- character()
for (lg in leagues) {
  lg_seasons <- sort(unique(mv$Season[mv$League == lg]))
  for (s in lg_seasons) {
    p    <- file.path("data/market_values", safe_name(lg))   # folder already created
    path <- save_csv(filter(mv, League == lg, Season == s),
                     file.path(p, paste0(s, ".csv")))
    l4_files <- c(l4_files, path)
  }
}
cat("L4:", length(l4_files), "files\n")

total_files <- 1L + length(l2_files) + length(l3_files) + length(l4_files)
cat(sprintf("Total CSV files written: %d  (expected >= 66)\n\n", total_files))

# ── Validation checks ──────────────────────────────────────────
chk <- function(label, pass, detail) {
  cat(sprintf("[%s] %s\n      %s\n", if (pass) "PASS" else "WARN", label, detail))
}

# Check 1: PL 2024-25 richest club should be > €1bn
pl25_top <- mv |> filter(League == "Premier League", Season == "2024-2025",
                          Financial_Rank == 1)
chk("Check 1 — PL 2024-25 richest > €1 000m",
    nrow(pl25_top) > 0 && pl25_top$Market_Value_M_EUR >= 1000,
    if (nrow(pl25_top) > 0)
      sprintf("%s  €%.0fm", pl25_top$Team, pl25_top$Market_Value_M_EUR)
    else "NO DATA for PL 2024-2025")

# Check 2: Bundesliga 2023-24 richest = Bayern Munich ~€831M
bun24_top <- mv |> filter(League == "Bundesliga", Season == "2023-2024",
                            Financial_Rank == 1)
chk("Check 2 — Bundesliga 2023-24 richest = Bayern Munich",
    nrow(bun24_top) > 0 && grepl("Bayern", bun24_top$Team),
    if (nrow(bun24_top) > 0)
      sprintf("%s  €%.0fm  (ref ~€831M)", bun24_top$Team, bun24_top$Market_Value_M_EUR)
    else "NO DATA for Bundesliga 2023-2024")

# Check 3: La Liga 2023-24 richest = Real Madrid ~€898M
ll24_top <- mv |> filter(League == "La Liga", Season == "2023-2024",
                           Financial_Rank == 1)
chk("Check 3 — La Liga 2023-24 richest = Real Madrid",
    nrow(ll24_top) > 0 && grepl("Real Madrid", ll24_top$Team),
    if (nrow(ll24_top) > 0)
      sprintf("%s  €%.0fm  (ref ~€898M)", ll24_top$Team, ll24_top$Market_Value_M_EUR)
    else "NO DATA for La Liga 2023-2024")

# Check 4: Leicester 2015-16 Financial_Rank between 8 and 14
leic <- mv |> filter(League == "Premier League", Season == "2015-2016",
                      grepl("Leicester", Team, ignore.case = TRUE))
chk("Check 4 — Leicester City 2015-16 Financial_Rank 8-14",
    nrow(leic) > 0 && leic$Financial_Rank >= 8 && leic$Financial_Rank <= 14,
    if (nrow(leic) > 0)
      sprintf("Financial_Rank = %d, €%.0fm  (won title despite modest squad value)",
              leic$Financial_Rank, leic$Market_Value_M_EUR)
    else "NOT FOUND")

# Check 5: total files >= 66
chk("Check 5 — total CSV files >= 66",
    total_files >= 66L,
    sprintf("%d files created  (L1=%d, L2=%d, L3=%d, L4=%d)",
            total_files, 1L, length(l2_files), length(l3_files), length(l4_files)))

# ── Summary report ─────────────────────────────────────────────
cat("\n=== Rows per league ===\n")
print(as.data.frame(league_summary(mv)))

cat("\n=== Season files with < 15 rows (PARTIAL DATA WARNING) ===\n")
small_seasons <- mv |>
  group_by(League, Season) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n < 15L)
if (nrow(small_seasons) == 0L) {
  cat("None — all season files have >= 15 rows.\n")
} else {
  print(as.data.frame(small_seasons))
}

cat("\n=== head(15) of master combined data ===\n")
print(head(mv, 15))
