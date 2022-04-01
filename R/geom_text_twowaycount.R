

#' Title
#'
#' @param data
#' @param scales
#'
#' @return
#' @export
#'
#' @examples
#' # step 1b test the computation function
#' library(dplyr)
#' mtcars %>%
#'   # input must have required aesthetic inputs as columns
#'   rename(x = cyl, y = gear) %>%
#'   compute_group_twowaycount() %>%
#'   head()
compute_group_twowaycount <- function(data, scales){

  data %>%
    # add an additional column called label
    # the geom we inherit from requires the label aesthetic
    dplyr::count(x, y)

}


StatTwowaycount <- ggplot2::ggproto(`_class` = "Twowaycount",
                                  `_inherit` = ggplot2::Stat,
                                  required_aes = c("x", "y"),
                                  compute_group = compute_group_twowaycount,
                                  default_aes = ggplot2::aes(label = ggplot2::after_stat(n)))

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
#' geom_text_twowaycount()
geom_text_twowaycount <- function(mapping = NULL, data = NULL,
                                  position = "identity", na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTwowaycount, # proto object from Step 2
    geom = ggplot2::GeomText, # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
