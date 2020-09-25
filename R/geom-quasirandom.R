#' Quasirandom points
#' 
#' @description The quasirandom geom is a convenient shortcut for 
#' `geom_point(position = "quasirandom")`. It shifts points to avoid 
#' overplotting using the \code{\link{offsetSingleGroup}} function from the 
#' `vipor` package.
#' 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_quasirandom
#' @inherit position_quasirandom details
#' @section Aesthetics:
#' `geom_quasirandom()` understands the following aesthetics (required aesthetics
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
#' @seealso [position_quasirandom()] for the underlying function to this geom. 
#' [geom_beeswarm()] for another method of shifting points to avoid
#' overplotting.
#'
#' @examples
#' #
#' @export
geom_quasirandom <- function(mapping = NULL, data = NULL,
                             stat = "identity", ..., 
                             method = "quasirandom",
                             width = NULL, varwidth = FALSE,
                             bandwidth = 0.5, nbins = NULL, dodge.width = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  position <- position_quasirandom(
    method = method,
    width = width, 
    varwidth = varwidth, 
    bandwidth = bandwidth,
    nbins = nbins, 
    dodge.width = dodge.width
  )
  
  ggplot2::layer(
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
