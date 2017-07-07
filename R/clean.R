eq_clean_date <- function(df) {
  df %>%
    dplyr::mutate(date = paste(YEAR, MONTH, DAY, ''),
           date = lubridate::ymd(date))
}

eq_clean_location <- function(df) {
  df %>%
    dplyr::mutate(LOCATION = stringr::str_split(LOCATION_NAME, ': ', simplify = TRUE)[,2],
           LOCATION = stringr::str_to_title(LOCATION_NAME))
}
