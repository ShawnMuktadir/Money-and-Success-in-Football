BROWSER_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

SEASONS <- data.frame(y1 = 2015L:2024L, y2 = 2016L:2025L)

LEAGUES_ESPN <- list(
  list(name = "Premier League", code = "eng.1"),
  list(name = "La Liga",        code = "esp.1"),
  list(name = "Bundesliga",     code = "ger.1"),
  list(name = "Serie A",        code = "ita.1"),
  list(name = "Ligue 1",        code = "fra.1")
)

LEAGUES_TM <- list(
  list(name = "Premier League", slug = "premier-league", code = "GB1"),
  list(name = "La Liga",        slug = "laliga",         code = "ES1"),
  list(name = "Bundesliga",     slug = "1-bundesliga",   code = "L1"),
  list(name = "Serie A",        slug = "serie-a",        code = "IT1"),
  list(name = "Ligue 1",        slug = "ligue-1",        code = "FR1")
)
