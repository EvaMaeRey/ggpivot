compute_panel_prop_within_x <- function(data, scales) {

  data %>%
    group_by(x, y) %>%
    summarise(value = sum(value)) %>%
    mutate(fill = fill/sum(fill)) %>%
    ungroup()

}

StatPropwithinx <- ggplot2::ggproto("StatPropovertime",
                                     Stat,
                                     compute_panel = compute_panel_prop_within_x,
                                     required_aes = c("x", "y", "fill")
)

#' Title
#'
#' @param mapping
#' @param data
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
#' library(ggplot2)
#' ggplot(data = mtcars) +
#' aes(x = cyl, y = gear) +
#' geom_tile_prop_within_x() +
#' aes(fill = 1) +
#' geom_text_twowaycount(color = "oldlace")
geom_tile_prop_within_x <- function(mapping = NULL,
                                     data = NULL,
                                     position = "identity",
                                     na.rm = FALSE,
                                     show.legend = NA,
                                     inherit.aes = TRUE, ...) {
  layer(
    stat = StatPropwithinx, geom = ggplot2::GeomTile, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

