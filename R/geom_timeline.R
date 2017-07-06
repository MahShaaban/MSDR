#' Drawing function for geom_timeline
#'
#' @param data
#' @param panel_params
#' @param coord
#'
#' @return A gTree object to draw
#' @export
draw_panel_function = function(data, panel_params, coord) {
  # subset data by xmin and xmax
  data <- data %>%
    filter(date >= xmin, date <= xmax) %>%
    mutate(x = as.numeric(date))
  coords <- coord$transform(data, panel_params)

  # points
  points <- grid::pointsGrob(
    coords$x, coords$y,
    pch = coords$shape,
    size = unit(coords$size * 2, 'pt'),
    gp = gpar(col = alpha(coords$colour, coords$alpha))
  )

  # segments
  segments <- grid::segmentsGrob(
    x0 = unit(coords$xmin, 'npc'),
    x1 = unit(coords$xmax, 'npc'),
    y0 = unit(coords$y, 'npc'),
    y1 = unit(coords$y, 'npc'),
    grid::gpar(col = alpha('gray', .5))
  )
  gTree(children = gList(points, segments))
}


GeomTimeline <- ggproto("GeomTimeline",
                        Geom,
                        required_aes = c("date", "xmin", "xmax", "y"),
                        non_missing_aes = c("size", "shape", "colour"),
                        default_aes = aes(shape = 19, alpha = .7),
                        draw_panel = draw_panel_function,
                        draw_key = draw_key_point
                        )

#' geom_timeline
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

