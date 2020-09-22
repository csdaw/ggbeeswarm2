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
