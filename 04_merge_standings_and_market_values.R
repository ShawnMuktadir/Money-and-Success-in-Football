# ==============================================================
# 04_merge_standings_and_market_values.R
# Merges ESPN league standings with Transfermarkt market values
# into one clean master dataset for analysis.
# ==============================================================

source("utils/packages.R")
source("utils/constants.R")
source("utils/helpers.R")
load_packages(c("dplyr", "readr", "stringr", "tidyr"))


# ── PHASE 1: Load both files and print diagnostics ─────────────

standings <- read_csv(
  "data/ESPN_standings/all_leagues/all_leagues_all_seasons.csv",
  show_col_types = FALSE
)

# Mirror the all_leagues/ subfolder layout for market values
# (creates the expected path if it only exists at the flat level)
mv_expected <- "data/market_values/all_leagues/all_leagues_all_seasons.csv"
mv_flat     <- "data/market_values/all_leagues_all_seasons.csv"
if (!file.exists(mv_expected)) {
  make_dir(dirname(mv_expected))
  file.copy(mv_flat, mv_expected)
}
market_vals <- read_csv(mv_expected, show_col_types = FALSE)

diag <- function(label, df) {
  cat(sprintf("\n=== %s ===\n", label))
  cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n")
  cat("Columns:", paste(names(df), collapse = ", "), "\n")
  cat("Unique leagues (", n_distinct(df$League), "):",
      paste(sort(unique(df$League)), collapse = ", "), "\n")
  cat("Unique seasons (", n_distinct(df$Season), "):",
      paste(sort(unique(df$Season)), collapse = ", "), "\n")
  cat("head(5):\n"); print(head(df, 5))
}
diag("STANDINGS (ESPN)", standings)
diag("MARKET VALUES (Transfermarkt)", market_vals)

stopifnot(
  "Standings must have 5 leagues"  = n_distinct(standings$League)   == 5L,
  "Market values must have 5 leagues" = n_distinct(market_vals$League) == 5L,
  "Standings must have 10 seasons" = n_distinct(standings$Season)   == 10L,
  "Market values must have 10 seasons" = n_distinct(market_vals$Season) == 10L
)
cat("\nPHASE 1 checks passed.\n")


# ── PHASE 2: Standardise team names ───────────────────────────

clean_team_name <- function(x) {
  # Step 1: lowercase
  x <- tolower(x)

  # Step 2: accented characters
  x <- gsub("[éèê]", "e", x)
  x <- gsub("[üúû]", "u", x)
  x <- gsub("[äáâà]", "a", x)
  x <- gsub("[öóô]", "o", x)
  x <- gsub("ñ",     "n", x)
  x <- gsub("ç",     "c", x)
  x <- gsub("ß",    "ss", x)

  # Step 3: remove generic club-type words (with surrounding spaces).
  # NOTE: "united", "city", "town" are deliberately EXCLUDED from this
  # list.  Including them would collapse "Manchester City" and
  # "Manchester United" to the same token "manchester", producing
  # incorrect cross-joins within each League+Season group.
  club_words <- c(
    " fc ", " afc ", " cf ", " sc ", " ac ", " rc ",
    " fk ", " sk ", " if ", " bv ", " sv ", " vfb "
  )
  padded <- paste0(" ", x, " ")
  for (w in club_words) {
    candidate <- gsub(w, " ", padded, fixed = TRUE)
    # ifelse() keeps vectorisation: only apply removal where result stays >= 3 chars
    padded <- ifelse(nchar(trimws(candidate)) >= 3L, candidate, padded)
  }
  x <- trimws(padded)

  # Step 4: remove all punctuation except hyphens
  x <- gsub("[^a-z0-9 \\-]", "", x)

  # Step 5: collapse multiple spaces
  x <- gsub("\\s+", " ", x)

  # Step 6: trim
  trimws(x)
}

standings   <- mutate(standings,   Team_clean = clean_team_name(Team))
market_vals <- mutate(market_vals, Team_clean = clean_team_name(Team))

cat("\n--- Sample cleaned names (standings) ---\n")
print(standings |> distinct(Team, Team_clean) |> arrange(Team) |> head(15))
cat("\n--- Sample cleaned names (market values) ---\n")
print(market_vals |> distinct(Team, Team_clean) |> arrange(Team) |> head(15))


# ── PHASE 3: Manual crosswalk on standings Team_clean ─────────
# Maps ESPN-cleaned names → Transfermarkt-cleaned names.
# Applied ONLY to standings (File A).

crosswalk <- c(
  # ── Premier League ──────────────────────────────────────────────────────────
  "manchester utd"       = "manchester united",
  "newcastle utd"        = "newcastle united",
  "brighton"             = "brighton hove albion",
  "nottm forest"         = "nottingham forest",
  "sheffield utd"        = "sheffield united",
  "west brom"            = "west bromwich albion",
  "wolves"               = "wolverhampton wanderers",
  "spurs"                = "tottenham hotspur",

  # ── La Liga ─────────────────────────────────────────────────────────────────
  "atletico madrid"      = "atletico de madrid",
  "real betis"           = "real betis balompie",
  "athletic bilbao"      = "athletic bilbao",
  "athletic club"        = "athletic bilbao",
  "alaves"               = "deportivo alaves",
  # TM "RCD Espanyol Barcelona" → "rcd espanyol barcelona" (full registered name on TM)
  "espanol"              = "rcd espanyol barcelona",
  "espanyol"             = "rcd espanyol barcelona",
  "mallorca"             = "rcd mallorca",
  "celta vigo"           = "celta de vigo",
  "eibar"                = "sd eibar",
  "las palmas"           = "ud las palmas",
  "deportivo la coruna"  = "deportivo de la coruna",
  "levante"              = "levante ud",
  "leganes"              = "cd leganes",
  "osasuna"              = "ca osasuna",
  "huesca"               = "sd huesca",
  # "Almería" → í stripped by punct → "almera"; TM "UD Almería" → "ud almera"
  "almera"               = "ud almera",
  # Valladolid, Girona, Granada, Getafe, Rayo, Cádiz, Sevilla, Valencia all match directly

  # ── Bundesliga ──────────────────────────────────────────────────────────────
  "mgladbach"            = "borussia monchengladbach",
  "b monchengladbach"    = "borussia monchengladbach",
  "bayer leverkusen"     = "bayer 04 leverkusen",
  "leverkusen"           = "bayer 04 leverkusen",
  "dortmund"             = "borussia dortmund",
  "hertha berlin"        = "hertha bsc",
  "hertha"               = "hertha bsc",
  "ein frankfurt"        = "eintracht frankfurt",
  "mainz"                = "1fsv mainz 05",
  # ESPN "FC Cologne" → "cologne" (fc stripped); TM "1.FC Köln" → "1fc koln" (no space → fc NOT stripped)
  "fc cologne"           = "1fc koln",
  "cologne"              = "1fc koln",
  "hamburg"              = "hamburger",
  "hoffenheim"           = "tsg 1899 hoffenheim",
  "tsg hoffenheim"       = "tsg 1899 hoffenheim",
  "1 union berlin"       = "1fc union berlin",
  # TM uses English "Nuremberg", not German "Nürnberg"
  "1 nurnberg"           = "1fc nuremberg",
  "1 heidenheim 1846"    = "1fc heidenheim 1846",
  "wolfsburg"            = "vfl wolfsburg",
  "bochum"               = "vfl bochum 1848",
  "schalke"              = "schalke 04",
  "greuther furth"       = "spvgg greuther furth",
  # Arminia Bielefeld: TM has no DSC prefix → both sides → "arminia bielefeld" directly

  # ── Serie A ─────────────────────────────────────────────────────────────────
  "inter"                = "inter milan",
  # ESPN used "Internazionale" in early seasons; TM consistently shows "Inter Milan"
  "internazionale"       = "inter milan",
  "roma"                 = "as roma",
  "napoli"               = "ssc napoli",
  "lazio"                = "ss lazio",
  "fiorentina"           = "acf fiorentina",
  "sampdoria"            = "uc sampdoria",
  "sassuolo"             = "us sassuolo",
  "genoa"                = "genoa cfc",
  "atalanta"             = "atalanta bc",
  # TM "Bologna FC 1909" → "bologna 1909" (fc stripped); ESPN "Bologna" → "bologna"
  "bologna"              = "bologna 1909",
  "udinese"              = "udinese calcio",
  "cagliari"             = "cagliari calcio",
  "lecce"                = "us lecce",
  "benevento"            = "benevento calcio",
  "salernitana"          = "us salernitana 1919",
  "spezia"               = "spezia calcio",
  # Smaller clubs that appear for 1-3 seasons
  "chievo"               = "chievo verona",
  "chievo verona"        = "chievo verona",
  "parma"                = "parma calcio 1913",
  "pescara"              = "delfino pescara 1936",
  "frosinone"            = "frosinone calcio",
  "cremonese"            = "us cremonese",
  "palermo"              = "us palermo",
  "carpi"                = "carpi 1909",
  "como"                 = "como 1907",
  # TM shows "Brescia Calcio (- 2025)" because of club liquidation; punct removes parens
  "brescia"              = "brescia calcio - 2025",

  # ── Ligue 1 ─────────────────────────────────────────────────────────────────
  "paris sg"             = "paris saint-germain",
  "paris saint germain"  = "paris saint-germain",
  "marseille"            = "olympique marseille",
  "lyon"                 = "olympique lyon",
  "monaco"               = "as monaco",
  "lille"                = "losc lille",
  "rennes"               = "stade rennais",
  "bordeaux"             = "girondins bordeaux",
  "nice"                 = "ogc nice",
  # TM uses "Stade Reims" (no "de"); ESPN shows "Stade de Reims" AND sometimes "Reims"
  "reims"                = "stade reims",
  "stade de reims"       = "stade reims",
  # TM "Stade Brestois 29" → "stade brestois 29" (includes founding year)
  "brest"                = "stade brestois 29",
  "strasbourg"           = "strasbourg alsace",
  "montpellier"          = "montpellier hsc",
  "troyes"               = "estac troyes",
  "angers"               = "angers sco",
  "saint-etienne"        = "as saint-etienne",
  # TM "EA Guingamp" → "ea guingamp" (abbreviated, not "En Avant")
  "guingamp"             = "ea guingamp",
  # TM "SM Caen" → "sm caen"
  "caen"                 = "sm caen",
  # "Nîmes" → î stripped by punct → "nmes"; TM "Nîmes Olympique" → "nmes olympique"
  "nimes"                = "nmes olympique",
  "nmes"                 = "nmes olympique",
  "as nancy lorraine"    = "as nancy-lorraine",
  "auxerre"              = "aj auxerre",
  # TM "Clermont Foot 63" → "clermont foot 63"; ESPN "Clermont Foot" → "clermont foot"
  "clermont foot"        = "clermont foot 63"
)

standings <- standings |>
  mutate(Team_clean = ifelse(
    Team_clean %in% names(crosswalk),
    crosswalk[Team_clean],
    Team_clean
  ))

cat("\n--- Crosswalk applied. Entries triggered ---\n")
triggered <- standings |>
  filter(Team_clean %in% crosswalk) |>
  distinct(Team, Team_clean)
if (nrow(triggered) == 0L) cat("(none triggered yet — names already matched)\n") else print(triggered)


# ── PHASE 4: Join + match quality report ──────────────────────

merged <- left_join(
  standings,
  select(market_vals, League, Season, Team_clean, Financial_Rank, Market_Value_M_EUR),
  by = c("League", "Season", "Team_clean")
)

n_total   <- nrow(merged)
n_matched <- sum(!is.na(merged$Market_Value_M_EUR))
n_missed  <- n_total - n_matched
rate      <- n_matched / n_total

cat(sprintf(
  "\n=== MATCH QUALITY REPORT ===\n  Total standings rows : %d\n  Matched             : %d\n  Unmatched           : %d\n  Match rate          : %.1f%%\n",
  n_total, n_matched, n_missed, rate * 100
))

if (n_missed > 0L) {
  cat("\n--- 20 worst unmatched rows (by league) ---\n")
  unmatched <- merged |>
    filter(is.na(Market_Value_M_EUR)) |>
    select(League, Season, League_Position, Team, Team_clean) |>
    arrange(League, Season, League_Position)
  print(head(unmatched, 20L), n = 20L)

  # Also show what TM names are available to help debug
  cat("\n--- TM clean names for unmatched leagues (sample 10 per league) ---\n")
  missed_leagues <- unique(unmatched$League)
  for (lg in missed_leagues) {
    cat(sprintf("  %s:\n", lg))
    sample_tm <- market_vals |>
      filter(League == lg) |>
      distinct(Team, Team_clean) |>
      head(10L)
    print(sample_tm)
  }
}

if (rate < 0.85) {
  stop(paste0(
    "WARNING: Match rate too low (", round(rate * 100, 1), "%). ",
    "Review unmatched teams above before proceeding. ",
    "Add them to the crosswalk table."
  ))
}
cat("\nMatch rate acceptable — proceeding to post-merge cleaning.\n")


# ── PHASE 5: Post-merge cleaning ──────────────────────────────

# 1. Champion flag
merged <- merged |>
  mutate(Champion = ifelse(League_Position == 1L, "Yes", "No"))

# 2. Normalized_Value  (0-1; 1.0 = richest squad that season)
merged <- merged |>
  group_by(League, Season) |>
  mutate(Normalized_Value = Market_Value_M_EUR / max(Market_Value_M_EUR, na.rm = TRUE)) |>
  ungroup()

# 3. Log_Normalized_Value  (model input per professor feedback)
merged <- merged |>
  mutate(Log_Normalized_Value = log(Normalized_Value))

# 4. Season_Index  (2015-2016 = 1, ..., 2024-2025 = 10)
season_order <- paste0(2015:2024, "-", 2016:2025)
merged <- merged |>
  mutate(Season_Index = match(Season, season_order))

# 5. Final column order
merged <- merged |>
  select(
    League, Season, Season_Index,
    League_Position, Team, Team_clean,
    Points, W, D, L, GF, GA,
    Financial_Rank, Market_Value_M_EUR,
    Normalized_Value, Log_Normalized_Value,
    Champion
  ) |>
  arrange(League, Season, League_Position)

# Save master dataset
make_dir("data/merged")
save_csv(merged, "data/merged/master_dataset.csv")
cat(sprintf("\nMaster dataset saved: %d rows x %d cols => data/merged/master_dataset.csv\n",
            nrow(merged), ncol(merged)))

# Champions analysis: title winners NOT in the top-3 richest squads
champions_surprise <- merged |>
  filter(Champion == "Yes", Financial_Rank > 3L) |>
  select(League, Season, League_Position, Team,
         Points, Financial_Rank, Market_Value_M_EUR, Normalized_Value) |>
  arrange(desc(Financial_Rank))

save_csv(champions_surprise, "data/merged/champions_surprise.csv")

cat(sprintf("\n=== head(15) of master dataset ===\n"))
print(head(merged, 15L))

cat(sprintf("\n=== Champions who were NOT in top-3 richest ===\n"))
cat(sprintf("(Financial_Rank > 3, sorted by rank descending — biggest surprise first)\n\n"))
print(as.data.frame(champions_surprise))

# ── Summary report ─────────────────────────────────────────────
cat("\n=== Rows per league ===\n")
summary_tbl <- merged |>
  group_by(League) |>
  summarise(
    Seasons        = n_distinct(Season),
    Total_rows     = n(),
    Matched_rows   = sum(!is.na(Market_Value_M_EUR)),
    Match_pct      = round(mean(!is.na(Market_Value_M_EUR)) * 100, 1),
    .groups = "drop"
  ) |>
  mutate(Flag = ifelse(Total_rows < 150L, "*** POSSIBLE DATA GAP ***", "OK"))
print(as.data.frame(summary_tbl))
