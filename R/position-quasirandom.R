#' Separate coincident points with the vipor package
#' 
#' @description Offset points to avoid overplotting using the 
#' \code{\link{offsetSingleGroup}} function from the `vipor` package.
#' 
#' @details 
#' **method:** specifies the algorithm used to distribute the points. 
#' 
#' `"quasirandom"`: points are distributed within a kernel density estimate of
#' the distribution with offset determined by quasirandom Van de Corput noise.
#' 
#' `"pseudorandom"`: points are distributed within a kernel density estimate of 
#' the distribution with offset determined by pseudorandom noise a la jitter.
#' 
#' `"maxout"`: points are distributed within a kernel density with points in a 
#' band distributed with highest value points on the outside and lowest in 
#' the middle.
#' 
#' `"minout"`: points are distributed within a kernel density with points in a 
#' band distributed with highest value points in the middle and lowest on 
#' the outside.
#' 
#' `"tukey"`: points are distributed as described in Tukey and Tukey, 
#' "Strips displaying empirical distributions: I. textured dot strips".
#' 
#' `"tukeyDense"`: points are distributed as described in Tukey and Tukey but 
#' are constrained with the kernel density estimate.
#'
#' @param method `string`. Method for arranging points, default is 
#' `"quasirandom"`. See details below.
#' @param width `numeric`. Maximum spacing away from the centre for each group
#' of points, default is `1`.
#' @param varwidth `boolean`. Adjust the width of each group based on the number
#' of points in the group. Default is `FALSE`.
#' @param bandwidth `numeric`. Specifies the bandwidth used to calculate the
#' kernel density, default is `1`. Smaller values = tighter fit. larger values
#' = looser fit.
#' @param nbins `integer` The number of points used to calculate density 
#' (default is `1000` for quasirandom and pseudorandom, and 100 for others).
#' @param dodge.width `numeric`. Amount to dodge points from different aesthetic
#' groups, default is `NULL` for no dodging.
#' 
#' @seealso [position_beeswarm()]
#'
#' @examples
#' #
#' @export
position_quasirandom <- function(method = "quasirandom",
                                 width = NULL, varwidth = FALSE,
                                 bandwidth = 0.5, nbins = NULL, 
                                 dodge.width = NULL) {
  ggproto(NULL, PositionQuasirandom,
          method = method,
          width = width, 
          varwidth = varwidth,
          bandwidth = bandwidth, 
          nbins = nbins, 
          dodge.width = dodge.width
  )
}

PositionQuasirandom <- ggproto("PositionQuasirandom", Position, 
                               required_aes = c("x", "y"),

                               setup_params = function(self, data) {
                                 flipped_aes <- has_flipped_aes(data)
                                 data <- flip_data(data, flipped_aes)
                                 
                                 # get number of points in each x axis group and 
                                 # find the largest group
                                 max.length <- max(data.frame(table(data$x))$Freq)
                                 
                                 list(
                                   method = self$method,
                                   width = self$width,
                                   varwidth = self$varwidth,
                                   bandwidth = self$bandwidth,
                                   nbins = self$nbins,
                                   max.length = max.length,
                                   dodge.width = self$dodge.width,
                                   flipped_aes = flipped_aes
                                 )
                               },
                               
                               compute_panel = function(data, params, scales) {
                                 data <- flip_data(data, params$flipped_aes)
                                 
                                 # set width if not specified
                                 if (is.null(params$width)) {
                                   params$width <- ggplot2::resolution(
                                     data$x, zero = FALSE) * 0.05
                                 }
                                 
                                 data <- .beeint$collide(
                                   data,
                                   params$dodge.width,
                                   name = "position_quasirandom",
                                   strategy = .beeint$pos_dodge,
                                   check.width = FALSE
                                 )
                                 
                                 # split data.frame into list of data.frames
                                 if(!is.null(params$dodge.width)) {
                                   data <- split(data, data$group)
                                 } else {
                                   data <- split(data, data$x)
                                 }
                                 
                                 # perform swarming separately for each data.frame
                                 data <- lapply(
                                   data,
                                   pos_quasirandom,
                                   method = params$method,
                                   width = params$width,
                                   vary.width = params$varwidth,
                                   adjust = params$bandwidth,
                                   nbins = params$nbins,
                                   max.length = params$max.length
                                 )
                                 
                                 # recombine list of data.frames into one
                                 data <- Reduce(rbind, data)
                                 
                                 flip_data(data, params$flipped_aes)
                               }
)

pos_quasirandom <- function(df, width = 0.4, vary.width = FALSE,
                            max.length = NULL, ...) {
  x.offset <- vipor::aveWithArgs(
    df$y, df$x, 
    FUN = vipor::offsetSingleGroup,
    maxLength = if (vary.width) {max.length} else {NULL},
    ...
  )
  
  x.offset <- x.offset * width
  df$x <- df$x + x.offset
  df
}
