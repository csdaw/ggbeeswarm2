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
                                     data$x, zero = FALSE) * 0.4
                                 }
                                 
                                 data <- ggplot2:::collide(
                                   data,
                                   params$dodge.width,
                                   name = "position_quasirandom",
                                   strategy = ggplot2:::pos_dodge,
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
