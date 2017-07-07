GeomTimelineLabel <- ggproto("GeomTimeline",
                        Geom,
                        required_aes = c("date", "xmin", "xmax", "location"),
                        draw_panel = function(data, panel_params, coord) {
                          # prepare data
                          ## subset by xmin and xmax
                          data <- data %>%
                            filter(date >= xmin, date <= xmax) %>%
                            mutate(x = as.numeric(date))
                          coords <- coord$transform(data, panel_params)

                          txt <- grid::textGrob(label = coords$location,
                                                x = unit(coords$x, 'npc'),
                                                y = unit(coords$y + .2, 'npc'),
                                                hjust = 0,
                                                rot = 45)
                          segments <- grid::segmentsGrob(x0 = unit(coords$x, 'npc'),
                                                         x1 = unit(coords$x, 'npc'),
                                                         y0 = unit(coords$y, 'npc'),
                                                         y1 = unit(coords$y + .15, 'npc'),
                                                         gp = grid::gpar(col = 'gray'))
                          grobTree(txt, segments)
                        }
)

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = FALSE,
                          inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimelineLabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

