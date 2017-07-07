eq_create_label <- function(df) {
  df %>%
  mutate(location = ifelse(is.na(LOCATION), '', paste("<b>Location: </b>", LOCATION, "<br>")),
         magnitude = ifelse(is.na(EQ_PRIMARY), '', paste("<b>Magnitude: </b>", EQ_PRIMARY, "<br>")),
         deaths = ifelse(is.na(DEATHS), '', paste("<b>Total deaths: </b>", DEATHS))) %>%
    rowwise() %>%
    do(popup_text = paste(.$location, .$magnitude, .$death)) %>%
    unlist(recursive = FALSE) %>%
    as.character
}


eq_map <- function(df) {
  leaflet(df) %>%
    addProviderTiles(providers$CartoDB) %>%
    addCircleMarkers(~LONGITUDE, ~LATITUDE,
                     popup = ~popup_text,
                     labelOptions = labelOptions(direction = 'bottom'),
                     radius = 5)
}
