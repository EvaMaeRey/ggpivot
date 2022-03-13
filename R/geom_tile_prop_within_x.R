#' Title
#'
#' @param data
#' @param scales
#'
#' @return
#' @export
#'
#' @examples
#' mtcars %>%
#' rename(x = cyl, y = gear, value = 1) %>%
#' compute_panel_prop_within
compute_panel_prop_withinx <- function(data, scales, drop = F) {

  data %>%
    group_by(x, y) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    group_by(x) %>%
    mutate(prop = value/sum(value)) %>%
    ungroup()

}


StatPropwithinx <- ggplot2::ggproto("StatPropwithinx",
                                     Stat,
                                     compute_panel = compute_panel_prop_withinx,
                                     required_aes = c("x", "y", "value"),
                                     default_aes = aes(fill = after_stat(prop))
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
#' geom_tile_prop_withinx() +
#' aes(value = 1, within = cyl) +
#' geom_text_twowaycount(color = "oldlace")
geom_tile_prop_withinx <- function(mapping = NULL,
                                     data = NULL,
                                     position = "identity",
                                     na.rm = FALSE,
                                     show.legend = NA,
                                     inherit.aes = TRUE, ...) {
  layer(
    stat = StatPropwithinx,
    geom = ggplot2::GeomTile,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

