safe_name <- function(n) gsub("[^A-Za-z0-9]", "_", n)
make_dir  <- function(p) dir.create(p, showWarnings = FALSE, recursive = TRUE)
save_csv  <- function(df, path) { readr::write_csv(df, path); invisible(path) }

league_summary <- function(df) {
  df |>
    dplyr::group_by(League) |>
    dplyr::summarise(
      Seasons        = dplyr::n_distinct(Season),
      Total_rows     = dplyr::n(),
      Avg_per_season = round(dplyr::n() / dplyr::n_distinct(Season), 1),
      .groups        = "drop"
    ) |>
    dplyr::mutate(Flag = ifelse(Total_rows < 150L, "*** POSSIBLE DATA GAP ***", "OK"))
}
