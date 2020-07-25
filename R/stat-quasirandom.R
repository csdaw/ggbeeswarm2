#' @export
StatQuasirandom <- ggproto("StatQuasirandom", Stat,
                           compute_group = function(data, scales) {
                             x.offset <- vipor::offsetX(
                               data$y,
                               data$x,
                               width = 0.4,
                               varwidth = FALSE,
                               adjust = 0.5,
                               method = "quasirandom",
                               nbins = NULL
                             )
                             data$x <- data$x + x.offset
                             data
                           },
                           
                           required_aes = c("x", "y")
)

#' @export
stat_quasirandom <- function(mapping = NULL, data = NULL, 
                             position = "identity", ..., 
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    stat = StatQuasirandom, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      ...
    )
  )
}