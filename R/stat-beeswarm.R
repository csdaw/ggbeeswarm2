StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        compute_group = function(data, scales, 
                                                 spacing = 1, side = 0L, 
                                                 priority = "ascending") {
                          x.offset <- beeswarm::swarmx(
                            x = rep(0, legnth(data$y)), y = data$y,
                            cex = spacing, side = side, priority = priority
                          )$x
                          
                          data$x <- data$x + x.offset
                          data
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL, 
                          position = "identity", ..., 
                          spacing = 1, side = 0L, priority = "ascending",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  stopifnot(priority %in% c("ascending", "descending", "density", "random", "none"))
  
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      spacing = spacing,
      side = side,
      priority = priority,
      ...
    )
  )
}
