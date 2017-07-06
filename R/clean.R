eq_clean_date <- function(df) {
  df %>%
    dplyr::mutate(date = paste(YEAR, MONTH, DAY, ''),
           date = lubridate::ymd(date))
}

eq_clean_location <- function(df) {
  df %>%
    dplyr::mutate(LOCATION_NAME = stringr::tr_split(LOCATION_NAME,
                                             ': ',
                                             simplify = TRUE)[,2],
           LOCATION_NAME = stringr::str_to_title(LOCATION_NAME))
}