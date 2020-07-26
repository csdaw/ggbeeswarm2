#' @export
position_quasirandom <- function(width = NULL) {
  ggproto(NULL, PositionQuasirandom,
          width = width
  )
}

PositionQuasirandom <- ggproto("PositionQuasirandom", ggplot2::Position, 
                               width = NULL,
                               setup_params = function(self, data) {
                                 flipped_aes <- ggplot2::has_flipped_aes(data)
                                 data <- ggplot2::flip_data(data, flipped_aes)
                                 
                                 list(
                                   width = self$width,
                                   flipped_aes = flipped_aes
                                 )
                               },
                               compute_panel = function(data, params, scales) {
                                 data <- flip_data(data, params$flipped_aes)
                                 data$x <- data$x.orig
                                 
                                 collided <- ggplot2:::collide(
                                   data,
                                   params$width,
                                   name = "position_quasirandom",
                                   strategy = ggplot2:::pos_dodge,
                                   n = NULL,
                                   check.width = FALSE
                                 )
                                 collided$x <- collided$x + collided$x.offset
                                 
                                 flip_data(collided, params$flipped_aes)
                               },
                               required_aes=c('x','y')
                               
                               
)

