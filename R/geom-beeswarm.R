#' Beeswarm points
#' 
#' @description The beeswarm geom is a convenient shortcut for 
#' `geom_point(position = "beeswarm")`. It shifts points to avoid 
#' overplotting using the \code{\link{swarmx}} function from the `beeswarm` 
#' package.
#' 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_beeswarm
#' @inherit position_beeswarm details
#' @section Aesthetics:
#' `geom_beeswarm()` understands the following aesthetics (required aesthetics
#' are in bold):
#' - **x**
#' - **y**
#' - alpha
#' - colour
#' - fill
#' - group
#' - shape
#' - size
#' - stroke
#' 
#' Learn more about setting these aesthetics in `vignette("ggplot2-specs")`.
#' 
#' @seealso [position_beeswarm()] for the underlying function to this geom. 
#' [geom_quasirandom()] for another method of shifting points to avoid
#' overplotting.
#'
#' @examples
#' #
#' @export
geom_beeswarm <- function(mapping = NULL, data = NULL,
                          stat = "identity", ...,
                          method = "swarm", spacing = 1,
                          side = 0L, priority = "ascending",
                          dodge.width = NULL, corral = "none", corral.width = 0.2,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  position <- position_beeswarm(
    method = method,
    spacing = spacing,
    side = side, 
    priority = priority,
    dodge.width = dodge.width,
    corral = corral,
    corral.width = corral.width
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
