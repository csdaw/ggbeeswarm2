StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        compute_group = function(data, scales) {
                          data
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL, 
                          position = "identity", ..., 
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      ...
    )
  )
}
