#' @export
StatQuasirandom <- ggproto("StatQuasirandom", Stat,
                           setup_params = function(data, params) {
                             params$flipped_aes <- has_flipped_aes(
                               data, params,
                               main_is_orthogonal = TRUE,
                               group_has_equal = TRUE,
                               main_is_optional = TRUE
                             )
                             
                             # find size of largest data$group
                             max.length <- max(data.frame(table(data$group))$Freq)
                             params$max.length <- max.length
                             params
                           },
                           
                           extra_params = c("na.rm", "orientation"),
                           
                           compute_group = function(data, scales, flipped_aes = FALSE,
                                                    width = 0.4, vary.width = FALSE,
                                                    max.length = NULL, bandwidth = 0.5, 
                                                    bins = NULL, method = "quasirandom") {
                             data <- flip_data(data, flipped_aes)
                             
                             x.offset <- offset_x(
                               data$y,
                               data$x,
                               width = width,
                               vary.width = vary.width,
                               max.length = max.length,
                               adjust = bandwidth,
                               method = method,
                               nbins = bins
                             )

                             data$x <- data$x + x.offset
                             flip_data(data, flipped_aes)
                           },
                           
                           required_aes = c("x", "y")
)

#' @export
stat_quasirandom <- function(mapping = NULL, data = NULL, 
                             position = "identity", ..., 
                             width = 0.4, vary.width = FALSE, bandwidth = 0.5,
                             bins = NULL, method = "quasirandom",
                             na.rm = FALSE, orientation = NA,
                             show.legend = NA, inherit.aes = TRUE) {
  layer(
    stat = StatQuasirandom, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      orientation = orientation,
      width = width,
      vary.width = vary.width,
      bandwidth = bandwidth,
      bins = bins,
      method = method,
      ...
    )
  )
}

offset_x <- function(y, x = rep(1, length(y)), width = 0.4, vary.width = FALSE,
                     max.length = NULL, ...) {
  if (length(x) != length(y)) stop("x and y not the same length in offset_x")
  
  offsets <- vipor::aveWithArgs(
    y, x, 
    FUN = vipor::offsetSingleGroup,
    maxLength = if (vary.width) {max.length} else {NULL},
    ...
  )
  out <- offsets * width
  
  return(out)
}