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
