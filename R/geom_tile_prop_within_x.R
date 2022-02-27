compute_panel_prop_within_x <- function(data, scales) {

  data %>%
    group_by(x, y) %>%
    summarise(fill = sum(fill)) %>%
    mutate(fill = fill/sum(fill)) %>%
    ungroup()

}

StatPropovertime <- ggplot2::ggproto("StatPropovertime",
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
#' geom_tile_prop_over_time() +
#' aes(fill = 1)
geom_tile_prop_over_time <- function(mapping = NULL, data = NULL,
                                     position = "identity", na.rm = FALSE, show.legend = NA,
                                     inherit.aes = TRUE, ...) {
  layer(
    stat = StatPropovertime, geom = ggplot2::GeomTile, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

