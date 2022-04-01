compute_panel_prop_within_x_text <- function(data, scales) {

  data %>%
    group_by(x, y) %>%
    summarise(label = sum(fill)) %>%
    mutate(label = label/sum(label)) %>%
    mutate(label = round(label, 2)) %>%
    ungroup()

}

StatPropwithinxtext <- ggplot2::ggproto(`_class` = "StatPropwithinxtext",
                                        `_inherit` = ggplot2::Stat,
                                        compute_panel = compute_panel_prop_within_x_text,
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
#' geom_text_prop_within_x(color = "oldlace")
geom_text_prop_within_x <- function(mapping = NULL, data = NULL,
                                    position = "identity", na.rm = FALSE,
                                    show.legend = NA,
                                    inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPropwithinxtext, # proto object from Step 2
    geom = ggplot2::GeomText, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

