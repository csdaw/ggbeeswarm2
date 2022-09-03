#' @export
geom_test <- function(mapping = NULL, 
                      data = NULL,
                      stat = "identity", 
                      position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTest,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
GeomTest <- ggproto("GeomTest", Geom,
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "shape", "colour"),
                    default_aes = aes(
                      shape = 19, colour = "black", size = 1.5, fill = NA,
                      alpha = NA, stroke = 0.5
                    ),
                    
                    draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
                      if (is.character(data$shape)) {
                        data$shape <- ggplot2:::translate_shape_string(data$shape)
                      }
                      
                      coords <- coord$transform(data, panel_params)
                      stroke_size <- coords$stroke
                      stroke_size[is.na(stroke_size)] <- 0

                      ggplot2:::ggname("geom_test", 
                                       testGrob(
                                         coords$x, coords$y,
                                         pch = coords$shape,
                                         gp = grid::gpar(
                                           col = alpha(coords$colour, coords$alpha),
                                           fill = alpha(coords$fill, coords$alpha),
                                           # Stroke is added around the outside of the point
                                           fontsize = coords$size * .pt + stroke_size * .stroke / 2,
                                           lwd = coords$stroke * .stroke / 2
                                         ))
                      )
                    },
                    
                    draw_key = ggplot2::draw_key_point
)

testGrob <- function (x = stats::runif(10), y = stats::runif(10), pch = 1, 
                      size = unit(1, "char"), default.units = "native", name = NULL, 
                      gp = gpar(), vp = NULL) {
  if (!grid::is.unit(x)) 
    x <- unit(x, default.units)
  if (!grid::is.unit(y)) 
    y <- unit(y, default.units)
  grid::grob(x = x, y = y, pch = pch, size = size, name = name, 
             gp = gp, vp = vp, cl = "thisisatest")
}

#' @export
#' @importFrom grid makeContent gpar
makeContent.thisisatest <- function(x) {
  print("Here!!!")
  utils::str(x)
  class(x)[1] <- "points"
  x
}

