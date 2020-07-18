StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        setup_params = function(data, params) {
                          params$flipped_aes <- has_flipped_aes(
                            data, params, 
                            main_is_orthogonal = TRUE,
                            group_has_equal = TRUE,
                            main_is_optional = TRUE
                          )
                          params
                        },
                        
                        extra_params = c("na.rm", "orientation"),
                        
                        compute_group = function(data, scales, flipped_aes = FALSE, 
                                                 spacing = 1, side = 0L, 
                                                 priority = "ascending",
                                                 corral = "none", corral.width = 0.889) {
                          data <- flip_data(data, flipped_aes)
                          
                          x.offset <- beeswarm::swarmx(
                            x = rep(0, legnth(data$y)), y = data$y,
                            cex = spacing, side = side, priority = priority
                          )$x
                          
                          if (corral != "none") {
                            corral.low <- (side - 1) * corral.width / 2
                            corral.high <- (side + 1) * corral.width / 2
                            
                            if (corral == "gutter") {
                              x.offset <- sapply(x.offset, function(zz) pmin(corral.high, pmax(corral.low, zz)))
                            }
                            if (corral == "wrap") {
                              if (side == -1L) {
                                # special case with side=-1: reverse the corral to avoid artefacts at zero
                                x.offset <- sapply(x.offset, function(zz) corral.high - ((corral.high - zz) %% corral.width))
                              } else {
                                x.offset <- sapply(x.offset, function(zz) ((zz - corral.low) %% corral.width) + corral.low)
                              }
                            }
                            if (corral == 'random') {
                              x.offset <- sapply(
                                x.offset, 
                                function(zz) ifelse(
                                  zz > corral.high | zz < corral.low, 
                                  yes = runif(length(zz), corral.low, corral.high), 
                                  no = zz
                                )
                              )
                            }
                            if (corral == 'omit') {
                              x.offset <- sapply(
                                x.offset, 
                                function(zz) ifelse(
                                  zz > corral.high | zz < corral.low, 
                                  yes = NA, 
                                  no = zz
                                )
                              )
                            }
                          }
                          
                          data$x <- data$x + x.offset
                          
                          flip_data(data, flipped_aes)
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL, 
                          position = "identity", ..., 
                          spacing = 1, side = 0L, priority = "ascending",
                          corral = "none", corral.width = 0.889,
                          na.rm = FALSE, orientation = NA, 
                          show.legend = NA, inherit.aes = TRUE) {
  stopifnot(priority %in% c("ascending", "descending", "density", "random", "none"))
  stopifnot(corral %in% c("none", "gutter", "wrap", "random", "omit"))
  stopifnot(is.numeric(corral.width))
  stopifnot(
    length(corral.width) == 1 & corral.width > 0
  )
  
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      spacing = spacing,
      side = side,
      priority = priority,
      corral = corral,
      corral.width = corral.width,
      ...
    )
  )
}
