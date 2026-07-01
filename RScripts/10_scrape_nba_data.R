# ==============================================================
# 10_scrape_nba_data.R
# NBA team payroll (Spotrac) + final standings (ESPN API) for
# 2015-16 through 2024-25 (10 seasons x 30 teams), built to be
# directly comparable to the football master dataset:
#   Payroll_Rank   (1 = highest payroll)   <-> Financial_Rank
#   Final_Standing (1 = best record)       <-> League_Position
#
# Basketball-Reference is blocked (403, Cloudflare) even with a
# browser UA, same issue the project already hit with FBref — so
# ESPN's public standings API is used instead (same family of API
# already used for soccer in 01_scrape_ESPN_standings.R).
# HoopsHype no longer serves historical team payroll pages (site
# restructured, only current season available) — Spotrac's
# year-indexed cap table is used instead.
#
# Outputs:
#   data/NBA/espn_standings/{season}.csv   (per season, 30 rows)
#   data/NBA/payroll/{season}.csv          (per season, 30 rows)
#   data/merged/nba_master_dataset.csv     (all seasons, 300 rows)
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("httr", "jsonlite", "rvest", "dplyr", "readr", "tidyr", "stringr"))

make_dir("data/NBA/espn_standings")
make_dir("data/NBA/payroll")
make_dir("data/merged")


# ══════════════════════════════════════════════════════════════
# PHASE 1: Static lookups — team names, champions, seasons
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 1: Static Lookups ===\n")

# Spotrac 3-letter abbreviation -> full team name (matches ESPN displayName).
# Stable for 2015-16 .. 2024-25 - no NBA team relocated in this window.
NBA_TEAM_LOOKUP <- c(
  MIN = "Minnesota Timberwolves", UTA = "Utah Jazz",           DEN = "Denver Nuggets",
  BKN = "Brooklyn Nets",          PHX = "Phoenix Suns",        PHI = "Philadelphia 76ers",
  HOU = "Houston Rockets",        OKC = "Oklahoma City Thunder", MIL = "Milwaukee Bucks",
  BOS = "Boston Celtics",         CHI = "Chicago Bulls",       IND = "Indiana Pacers",
  LAL = "Los Angeles Lakers",     ATL = "Atlanta Hawks",       SAC = "Sacramento Kings",
  GSW = "Golden State Warriors",  NOP = "New Orleans Pelicans", MIA = "Miami Heat",
  CHA = "Charlotte Hornets",      WAS = "Washington Wizards",  NYK = "New York Knicks",
  DAL = "Dallas Mavericks",       ORL = "Orlando Magic",       DET = "Detroit Pistons",
  SAS = "San Antonio Spurs",      MEM = "Memphis Grizzlies",   TOR = "Toronto Raptors",
  POR = "Portland Trail Blazers", LAC = "LA Clippers",         CLE = "Cleveland Cavaliers"
)

# Actual NBA Finals winners (verified against NBA.com / ESPN history),
# used only for the champion summary stats — NOT the same as
# "best regular-season record", unlike the football Champion column.
NBA_CHAMPIONS <- c(
  "2015-2016" = "Cleveland Cavaliers",
  "2016-2017" = "Golden State Warriors",
  "2017-2018" = "Golden State Warriors",
  "2018-2019" = "Toronto Raptors",
  "2019-2020" = "Los Angeles Lakers",
  "2020-2021" = "Milwaukee Bucks",
  "2021-2022" = "Golden State Warriors",
  "2022-2023" = "Denver Nuggets",
  "2023-2024" = "Boston Celtics",
  "2024-2025" = "Oklahoma City Thunder"
)

NBA_SEASONS <- data.frame(y1 = 2015L:2024L, y2 = 2016L:2025L)
cat(sprintf("  %d teams mapped, %d champions, %d seasons\n",
            length(NBA_TEAM_LOOKUP), length(NBA_CHAMPIONS), nrow(NBA_SEASONS)))
cat("PHASE 1 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 2: Fetch functions
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 2: Define Fetch Functions ===\n")

fetch_espn_standings <- function(end_year) {
  url <- sprintf("https://site.api.espn.com/apis/v2/sports/basketball/nba/standings?season=%d",
                end_year)
  resp <- tryCatch(
    GET(url, add_headers(`User-Agent` = BROWSER_UA, `Accept` = "application/json"), timeout(30)),
    error = function(e) { cat("    [ERROR] ESPN GET failed:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(resp) || status_code(resp) != 200L) {
    cat("    [WARN] ESPN standings HTTP failure for", end_year, "\n")
    return(NULL)
  }
  parsed <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) return(NULL)

  rows <- list()
  for (conf in parsed$children) {
    for (e in conf$standings$entries) {
      stat_vals <- setNames(sapply(e$stats, function(s) s$value), sapply(e$stats, function(s) s$name))
      rows[[length(rows) + 1]] <- data.frame(
        Team       = e$team$displayName,
        Wins       = as.numeric(stat_vals["wins"]),
        Losses     = as.numeric(stat_vals["losses"]),
        WinPercent = as.numeric(stat_vals["winPercent"]),
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

fetch_spotrac_payroll <- function(end_year) {
  url <- sprintf("https://www.spotrac.com/nba/cap/_/year/%d", end_year)
  resp <- tryCatch(
    GET(url, add_headers(`User-Agent` = BROWSER_UA), timeout(30)),
    error = function(e) { cat("    [ERROR] Spotrac GET failed:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(resp) || status_code(resp) != 200L) {
    cat("    [WARN] Spotrac payroll HTTP failure for", end_year, "\n")
    return(NULL)
  }
  page <- read_html(content(resp, "text", encoding = "UTF-8"))
  tbl_node <- page |> html_element("table.dataTable.premium")
  if (is.na(tbl_node)) { cat("    [WARN] Spotrac table not found for", end_year, "\n"); return(NULL) }

  tbl <- html_table(tbl_node, header = TRUE, fill = TRUE)
  names(tbl) <- make.names(trimws(gsub("\\s+", " ", names(tbl))), unique = TRUE)

  tbl |>
    filter(!is.na(Rank)) |>
    mutate(
      Team_Abbr          = str_extract(trimws(gsub("\\s+", " ", Team)), "^[A-Z]+"),
      Total_Cap_Allocations = as.numeric(gsub("[$,]", "", Total.Cap.Allocations))
    ) |>
    filter(!is.na(Team_Abbr), Team_Abbr %in% names(NBA_TEAM_LOOKUP)) |>
    mutate(Team = unname(NBA_TEAM_LOOKUP[Team_Abbr])) |>
    select(Team, Total_Cap_Allocations)
}
cat("PHASE 2 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 3: Loop seasons, fetch, merge, rank
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 3: Fetch & Merge Per Season ===\n")

all_seasons <- vector("list", nrow(NBA_SEASONS))

for (i in seq_len(nrow(NBA_SEASONS))) {
  y1 <- NBA_SEASONS$y1[i]
  y2 <- NBA_SEASONS$y2[i]
  season_label <- paste0(y1, "-", y2)
  cat(sprintf("  Season %s ... ", season_label))

  standings <- fetch_espn_standings(y2)
  Sys.sleep(2)
  payroll <- fetch_spotrac_payroll(y2)
  Sys.sleep(2)

  if (is.null(standings) || is.null(payroll) || nrow(standings) != 30L || nrow(payroll) != 30L) {
    cat(sprintf("FAILED (standings=%s, payroll=%s)\n",
               ifelse(is.null(standings), "NULL", nrow(standings)),
               ifelse(is.null(payroll), "NULL", nrow(payroll))))
    next
  }

  save_csv(standings, file.path("data/NBA/espn_standings", paste0(season_label, ".csv")))
  save_csv(payroll,   file.path("data/NBA/payroll",        paste0(season_label, ".csv")))

  merged <- standings |>
    inner_join(payroll, by = "Team") |>
    mutate(
      Season          = season_label,
      Season_Index    = i,
      Final_Standing  = rank(-WinPercent, ties.method = "min"),
      Payroll_Rank    = rank(-Total_Cap_Allocations, ties.method = "min"),
      Champion        = ifelse(Team == NBA_CHAMPIONS[[season_label]], "Yes", "No")
    )

  if (nrow(merged) != 30L) {
    cat(sprintf("FAILED (joined only %d/30 teams)\n", nrow(merged)))
    next
  }

  all_seasons[[i]] <- merged
  cat(sprintf("OK (30 teams, champion = %s)\n", NBA_CHAMPIONS[[season_label]]))
}
cat("PHASE 3 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 4: Combine & save merged NBA master dataset
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 4: Save Merged NBA Master Dataset ===\n")

nba_master <- bind_rows(Filter(Negate(is.null), all_seasons)) |>
  select(Season, Season_Index, Team, Final_Standing, Wins, Losses, WinPercent,
         Payroll_Rank, Total_Cap_Allocations, Champion) |>
  arrange(Season_Index, Final_Standing)

if (nrow(nba_master) == 0L) stop("No NBA seasons were successfully scraped.")

path_master <- "data/merged/nba_master_dataset.csv"
save_csv(nba_master, path_master)
cat(sprintf("  Saved: %s (%d rows)\n", path_master, nrow(nba_master)))
cat("PHASE 4 complete.\n\n")


# ══════════════════════════════════════════════════════════════
# PHASE 5: Validation summary
# ══════════════════════════════════════════════════════════════
cat("=== PHASE 5: Validation Summary ===\n")

seasons_ok <- n_distinct(nba_master$Season)
cat(sprintf("  Seasons successfully merged: %d / %d\n", seasons_ok, nrow(NBA_SEASONS)))
print(as.data.frame(nba_master |> count(Season, name = "Teams")))

champ_check <- nba_master |> filter(Champion == "Yes")
cat(sprintf("\n  Champions found in merged data: %d (expected %d)\n",
            nrow(champ_check), length(NBA_CHAMPIONS)))
print(as.data.frame(champ_check |> select(Season, Team, Final_Standing, Payroll_Rank)))

if (any(is.na(nba_master$Final_Standing)) || any(is.na(nba_master$Payroll_Rank))) {
  cat("\n  [WARNING] Missing Final_Standing/Payroll_Rank values detected.\n")
} else {
  cat("\n  No missing Final_Standing/Payroll_Rank values.\n")
}

cat("\n10_scrape_nba_data.R complete.\n")
