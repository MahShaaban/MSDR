GeomTimeline <- ggproto("GeomTimeline",
                        Geom,
                        required_aes = c("date", "xmin", "xmax", "y", "colour", "size"),
                        default_aes = aes(shape = 19, stroke = .5, alpha = .7),
                        draw_key = function(data, params, size) {
                          pointsGrob(0.5, 0.5,
                                     pch = data$shape,
                                     gp = gpar(fontsize = data$size * .pt + data$stroke * .stroke / 2)
                                     )

                        },
                        draw_panel = function(data, panel_scales, coord) {
                          # prepare data
                          ## subset by xmin and xmax
                          data <- data %>%
                            filter(date >= xmin, date <= xmax) %>%
                            mutate(x = as.numeric(date))

                          coords <- coord$transform(data, panel_scales)
                          print(coords)
                          grobTree(
                            grid::pointsGrob(
                              coords$x, coords$y,
                              pch = coords$shape,
                              gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                              fill = alpha(coords$fill, coords$alpha),
                                              fontsize = coords$size * .pt + coords$stroke * .stroke / 2)
                            ),
                            grid::segmentsGrob(
                              x0 = unit(coords$xmin, 'npc'),
                              x1 = unit(coords$xmax, 'npc'),
                              y0 = unit(coords$y, 'npc'),
                              y1 = unit(coords$y, 'npc'),
                              gp = gpar(col = 'gray')
                            )
                          )
                        }
)

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
