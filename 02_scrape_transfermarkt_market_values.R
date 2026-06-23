# ==============================================================
# 02_scrape_transfermarkt_market_values.R
# Squad market values — Transfermarkt
# 5 top European leagues, 2015-16 through 2024-25
# Financial_Rank: 1 = highest squad market value (richest)
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("httr", "rvest", "dplyr", "readr"))


# ── PHASE 2: scrape_tm_season() ───────────────────────────────

# Parse "€553.50m" -> 553.50,  "€1.23bn" -> 1230.0
parse_tm_value <- function(x) {
  if (is.na(x) || trimws(x) %in% c("", "-")) return(NA_real_)
  mult <- ifelse(grepl("bn", x, ignore.case = TRUE), 1000, 1)
  suppressWarnings(as.numeric(gsub("[^0-9.]", "", x))) * mult
}

scrape_tm_season <- function(league_name, tm_slug, tm_code, y1, y2) {
  season_label <- paste0(y1, "-", y2)
  url <- sprintf(
    "https://www.transfermarkt.com/%s/startseite/wettbewerb/%s/plus/?saison_id=%d",
    tm_slug, tm_code, y1
  )

  cat(sprintf("  %-16s %s ... ", league_name, season_label))
  Sys.sleep(4)

  resp <- tryCatch(
    GET(url,
        add_headers(
          "User-Agent"      = BROWSER_UA,
          "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "en-US,en;q=0.9",
          "Referer"         = "https://www.transfermarkt.com/"
        ),
        timeout(30)),
    error = function(e) { cat("[ERROR] GET:", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(resp) || status_code(resp) != 200L) {
    cat(sprintf("HTTP %d\n", status_code(resp)))
    return(NULL)
  }

  page <- tryCatch(
    read_html(content(resp, "text", encoding = "UTF-8")),
    error = function(e) { cat("[ERROR] parse\n"); NULL }
  )
  if (is.null(page)) return(NULL)

  # Only odd/even rows — skips header/footer/total rows
  rows <- html_nodes(page,
    "table.items > tbody > tr.odd, table.items > tbody > tr.even")

  if (length(rows) == 0L) { cat("no rows\n"); return(NULL) }

  out <- lapply(rows, function(row) {
    # Team name via title attribute (immune to nested badge/trophy links)
    team <- tryCatch(
      html_attr(html_node(row, "td.hauptlink.no-border-links a[title]"), "title"),
      error = function(e) NA_character_
    )
    if (is.na(team) || team == "") return(NULL)

    # Total squad value: td.rechts that wraps an <a> (not the plain avg cell)
    val_raw <- tryCatch(
      html_text(html_node(row, "td.rechts > a"), trim = TRUE),
      error = function(e) NA_character_
    )

    data.frame(Team = team, Market_Value_Raw = val_raw,
               stringsAsFactors = FALSE)
  })

  df <- bind_rows(Filter(Negate(is.null), out))
  if (nrow(df) == 0L) { cat("empty\n"); return(NULL) }

  df <- df |>
    mutate(
      Market_Value_M_EUR = sapply(Market_Value_Raw, parse_tm_value),
      League = league_name,
      Season = season_label
    ) |>
    filter(!is.na(Market_Value_M_EUR), Market_Value_M_EUR > 0) |>
    arrange(desc(Market_Value_M_EUR)) |>
    mutate(Financial_Rank = row_number()) |>
    select(League, Season, Financial_Rank, Team, Market_Value_M_EUR)

  cat(sprintf("OK (%d teams)\n", nrow(df)))
  df
}


# ── PHASE 3: Leagues and seasons ──────────────────────────────
cat(sprintf("Leagues: %d  |  Seasons: %d  |  Requests planned: %d\n\n",
            length(LEAGUES_TM), nrow(SEASONS), length(LEAGUES_TM) * nrow(SEASONS)))


# ── PHASE 4: Nested loop with error handling ──────────────────
all_data <- vector("list", length(LEAGUES_TM) * nrow(SEASONS))
idx <- 0L

for (lg in LEAGUES_TM) {
  cat(sprintf("=== %s ===\n", lg$name))
  for (s in seq_len(nrow(SEASONS))) {
    idx <- idx + 1L
    all_data[[idx]] <- tryCatch(
      scrape_tm_season(lg$name, lg$slug, lg$code, SEASONS$y1[s], SEASONS$y2[s]),
      error = function(e) { cat("    [ERROR]", conditionMessage(e), "\n"); NULL }
    )
  }
  cat("\n")
}


# ── PHASE 5: Clean, validate, and save ────────────────────────
combined <- bind_rows(Filter(Negate(is.null), all_data))
if (nrow(combined) == 0L) stop("No data collected.")

combined <- arrange(combined, League, Season, Financial_Rank)

make_dir("data")
save_csv(combined, "data/transfermarkt_market_values_2015_2025.csv")
cat(sprintf("Combined CSV: %d rows x %d cols => data/transfermarkt_market_values_2015_2025.csv\n\n",
            nrow(combined), ncol(combined)))

# Per-league / per-season files under data/{League}/
file_count <- 0L
for (lg_name in unique(combined$League)) {
  folder <- file.path("data", safe_name(lg_name))
  make_dir(folder)
  for (seas in sort(unique(combined$Season[combined$League == lg_name]))) {
    sl    <- combined[combined$League == lg_name & combined$Season == seas, ]
    save_csv(sl, file.path(folder, paste0("tm_", seas, ".csv")))
    file_count <- file_count + 1L
  }
}
cat(sprintf("%d individual CSV files saved under data/{League}/tm_*.csv\n\n", file_count))

cat("=== head(15) of combined data ===\n")
print(head(combined, 15))


# ── PHASE 6: Summary report ───────────────────────────────────
cat("\n=== Rows per league ===\n")
print(as.data.frame(league_summary(combined)))

# Validation: Leicester 2015-16 Financial_Rank should be 8-14
leic <- combined[combined$League == "Premier League" &
                 combined$Season == "2015-2016" &
                 grepl("Leicester", combined$Team, ignore.case = TRUE), ]
if (nrow(leic) > 0L) {
  cat(sprintf(
    "\nValidation — Leicester City 2015-16: Financial_Rank = %d, Market Value = €%.1fm\n",
    leic$Financial_Rank, leic$Market_Value_M_EUR
  ))
  if (leic$Financial_Rank >= 8L && leic$Financial_Rank <= 14L) {
    cat("PASS: rank is between 8 and 14 (won title despite modest squad value)\n")
  } else {
    cat("WARN: rank outside expected 8-14 range — check data\n")
  }
} else {
  cat("\nValidation: Leicester City not found in 2015-16 Premier League rows\n")
}
