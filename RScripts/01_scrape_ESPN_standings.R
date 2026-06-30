# ==============================================================
# Top-5 European League Standings  |  2015-16 to 2024-25
# Source: ESPN public standings API (FBref blocked by Cloudflare)
# ESPN API mirrors FBref standings data identically
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("httr", "jsonlite", "dplyr", "readr"))


# ── PHASE 2: Define scrape_one_season() ───────────────────────
#
# ESPN standings API:
#   https://site.api.espn.com/apis/v2/sports/soccer/{league}/standings?season={end_year}
#
# season parameter = START year of the season
#   2015-16  -> season=2015
#   2024-25  -> season=2024

scrape_one_season <- function(league_name, espn_code, y1, y2) {

  season_label <- paste0(y1, "-", y2)
  url <- sprintf(
    "https://site.api.espn.com/apis/v2/sports/soccer/%s/standings?season=%d",
    espn_code, y1
  )

  # --- fetch with real browser UA ---
  resp <- tryCatch(
    GET(url,
        add_headers(`User-Agent`      = BROWSER_UA,
                    `Accept`          = "application/json",
                    `Accept-Language` = "en-US,en;q=0.9"),
        timeout(30)),
    error = function(e) {
      cat("    [ERROR] GET failed:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(resp)) return(NULL)

  if (status_code(resp) != 200L) {
    cat("    [WARN] HTTP", status_code(resp), "for", url, "\n")
    return(NULL)
  }

  # --- parse JSON ---
  parsed <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8"),
             simplifyVector = FALSE),
    error = function(e) { cat("    [ERROR] JSON parse:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(parsed)) return(NULL)

  entries <- tryCatch(
    parsed$children[[1]]$standings$entries,
    error = function(e) NULL
  )
  if (is.null(entries) || length(entries) == 0L) {
    cat("    [WARN] No entries found in JSON\n")
    return(NULL)
  }

  # --- extract each team's row ---
  rows <- lapply(entries, function(e) {
    team_name <- e$team$displayName

    # flatten stats list into named vector
    stat_vals <- setNames(
      sapply(e$stats, function(s) s$value),
      sapply(e$stats, function(s) s$name)
    )

    data.frame(
      Team            = team_name,
      League_Position = as.integer(stat_vals["rank"]),
      Points          = as.integer(stat_vals["points"]),
      W               = as.integer(stat_vals["wins"]),
      D               = as.integer(stat_vals["ties"]),
      L               = as.integer(stat_vals["losses"]),
      GF              = as.integer(stat_vals["pointsFor"]),
      GA              = as.integer(stat_vals["pointsAgainst"]),
      stringsAsFactors = FALSE
    )
  })

  df <- bind_rows(rows)
  df <- df[!is.na(df$League_Position), ]
  df$League <- league_name
  df$Season <- season_label
  df
}


# ── PHASE 3: Leagues and seasons ──────────────────────────────
cat("Leagues:", length(LEAGUES_ESPN), "\n")
cat("Seasons:", nrow(SEASONS),
    "(", paste0(SEASONS$y1[1], "-", SEASONS$y2[1]),
    "to", paste0(SEASONS$y1[nrow(SEASONS)], "-", SEASONS$y2[nrow(SEASONS)]), ")\n")
cat("Total requests planned:", length(LEAGUES_ESPN) * nrow(SEASONS), "\n\n")


# ── PHASE 4: Nested loop with error handling ──────────────────
all_data <- vector("list", length(LEAGUES_ESPN) * nrow(SEASONS))
idx <- 0L

for (lg in LEAGUES_ESPN) {
  cat(sprintf("=== %s ===\n", lg$name))

  for (s in seq_len(nrow(SEASONS))) {
    y1 <- SEASONS$y1[s]
    y2 <- SEASONS$y2[s]
    idx <- idx + 1L

    cat(sprintf("  Fetching %s %d-%d ... ", lg$name, y1, y2))

    result <- tryCatch(
      scrape_one_season(lg$name, lg$code, y1, y2),
      error = function(e) {
        cat("\n    [ERROR]", conditionMessage(e), "\n")
        NULL
      }
    )

    if (!is.null(result) && nrow(result) > 0L) {
      all_data[[idx]] <- result
      cat(sprintf("OK (%d teams)\n", nrow(result)))
    } else {
      cat("FAILED\n")
    }

    Sys.sleep(4)   # 4-second pause between every request
  }
  cat("\n")
}


# ── PHASE 5: Clean, validate, and save ────────────────────────
cat("Combining results ...\n")
combined <- bind_rows(Filter(Negate(is.null), all_data))

if (nrow(combined) == 0L) {
  stop("No data collected. Check internet connection.")
}

combined <- combined[, c("League", "Season", "League_Position",
                          "Team", "Points", "W", "D", "L", "GF", "GA")]
combined <- arrange(combined, League, Season, League_Position)

all_leagues <- unique(combined$League)
all_seasons <- sort(unique(combined$Season))

# Level 1 — master combined (1 file)
make_dir("data/ESPN_standings/all_leagues")
save_csv(combined, "data/ESPN_standings/all_leagues/all_leagues_all_seasons.csv")
cat("L1: data/ESPN_standings/all_leagues/all_leagues_all_seasons.csv\n")

# Level 2 — one CSV per league, all seasons (5 files)
l2 <- 0L
for (lg in all_leagues) {
  p <- file.path("data/ESPN_standings", safe_name(lg)); make_dir(p)
  save_csv(filter(combined, League == lg), file.path(p, "all_seasons.csv"))
  l2 <- l2 + 1L
}
cat(sprintf("L2: %d files\n", l2))

# Level 3 — one CSV per season, all leagues (10 files)
make_dir("data/ESPN_standings/by_season")
l3 <- 0L
for (s in all_seasons) {
  save_csv(filter(combined, Season == s),
           file.path("data/ESPN_standings/by_season", paste0(s, ".csv")))
  l3 <- l3 + 1L
}
cat(sprintf("L3: %d files\n", l3))

# Level 4 — one CSV per league × season (50 files)
l4 <- 0L
for (lg in all_leagues) {
  p <- file.path("data/ESPN_standings", safe_name(lg))
  for (s in sort(unique(combined$Season[combined$League == lg]))) {
    save_csv(filter(combined, League == lg, Season == s),
             file.path(p, paste0(s, ".csv")))
    l4 <- l4 + 1L
  }
}
cat(sprintf("L4: %d files\n", l4))

total_files <- 1L + l2 + l3 + l4
cat(sprintf("Total CSV files: %d  (expected >= 66)\n\n", total_files))


# ── PHASE 6: Summary report ───────────────────────────────────
cat("=== head(10) ===\n")
print(head(combined, 10))

cat("\n=== Rows per league ===\n")
summary_tbl <- league_summary(combined)
print(as.data.frame(summary_tbl))

flagged <- summary_tbl$League[summary_tbl$Total_rows < 150L]
if (length(flagged) > 0L) {
  cat("\n[WARNING] Leagues with < 150 rows:\n  ",
      paste(flagged, collapse = "\n  "), "\n")
} else {
  cat("\nAll leagues passed the 150-row threshold.\n")
}
