#' @export
StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        setup_params = function(data, params) {
                          params$flipped_aes <- has_flipped_aes(
                            data, params, 
                            main_is_orthogonal = TRUE,
                            group_has_equal = TRUE,
                            main_is_optional = TRUE
                          )
                          
                          # define n.groups 
                          params$n.groups <- length(unique(data$group))
                          
                          # get y range of data and extend it a little
                          if (params$flipped_aes) {
                            params$y.lim <- grDevices::extendrange(data$x, f = 0.01)
                          } else {
                            params$y.lim <- grDevices::extendrange(data$y, f = 0.01)
                          }
                          params
                        },
                        
                        extra_params = c("na.rm", "orientation"),
                        
                        compute_group = function(data, scales, flipped_aes = FALSE, 
                                                 n.groups, y.lim, method = "swarm",
                                                 spacing = 1, breaks = NULL, side = 0L,
                                                 priority = "ascending",
                                                 corral = "none", corral.width = 0.889) {
                          data <- flip_data(data, flipped_aes)
                          
                          if (method == "swarm") {
                          x.offset <- beeswarm::swarmx(
                            x = rep(0, length(data$y)), y = data$y,
                            cex = spacing, side = side, priority = priority
                          )$x
                          } else {
                            ## non-swarm methods

                            # define size.x and size.y
                            sizeMultiplier <- par('cex') * 1 * spacing
                            print(sizeMultiplier)
                            size.x <- xinch(0.08, warn.log = FALSE) * spacing
                            size.y <- yinch(0.08, warn.log = FALSE) * spacing
                            
                            # hex method specific step
                            if (method == "hex") size.y <- size.y * sqrt(3) / 2
                            
                            ## first determine positions along the y axis
                            if(is.null(breaks))
                              breaks <- seq(y.lim[1], y.lim[2] + size.y, by = size.y)
                            
                            if(length(breaks) == 1 && is.na(breaks[1])) {
                              y.index <- data$y
                              d.pos <- data$y
                            } else {
                              mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
                              y.index <- sapply(data$y, cut, breaks = breaks, labels = FALSE)
                              
                              y.pos <- sapply(y.index, function(a) mids[a])  
                              data$y <- y.pos
                            }
                            
                            ## now determine offset along the x axis
                            x.index <- determine_pos(y.index, method, side)
                            
                            x.offset <- x.index * size.x
                          }
                          
                          if (corral != "none") {
                            corral.low <- (side - 1) * corral.width / 2
                            corral.high <- (side + 1) * corral.width / 2
                            
                            if (corral == "gutter") {
                              x.offset <- sapply(
                                x.offset, 
                                function(zz) pmin(corral.high, pmax(corral.low, zz))
                              )
                            }
                            if (corral == "wrap") {
                              if (side == -1L) {
                                # special case with side=-1: reverse the corral to avoid artefacts at zero
                                x.offset <- sapply(
                                  x.offset, 
                                  function(zz) corral.high - ((corral.high - zz) %% corral.width)
                                )
                              } else {
                                x.offset <- sapply(
                                  x.offset, 
                                  function(zz) ((zz - corral.low) %% corral.width) + corral.low
                                )
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
                          data$x.orig <- data$x
                          data$x.offset <- x.offset
                          data$x <- data$x + data$x.offset
                          
                          flip_data(data, flipped_aes)
                        },
                        
                        required_aes = c("x", "y")
)

#' @export
stat_beeswarm <- function(mapping = NULL, data = NULL, 
                          position = "identity", ..., method = "swarm",
                          spacing = 1, breaks = NULL, side = 0L, priority = "ascending",
                          corral = "none", corral.width = 0.889,
                          dodge.width = NULL,
                          na.rm = FALSE, orientation = NA, 
                          show.legend = NA, inherit.aes = TRUE) {
  stopifnot(method %in% c("swarm", "centre", "center", "hex", "square"))
  if (method == "center") method <- "centre"
  stopifnot(priority %in% c("ascending", "descending", "density", "random", "none"))
  stopifnot(corral %in% c("none", "gutter", "wrap", "random", "omit"))
  stopifnot(is.numeric(corral.width))
  stopifnot(
    length(corral.width) == 1 & corral.width > 0
  )
  
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = "point", 
    position = position_beeswarm(width = dodge.width), 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      method = method,
      spacing = spacing,
      breaks = breaks,
      side = side,
      priority = priority,
      corral = corral,
      corral.width = corral.width,
      ...
    )
  )
}

determine_pos <- function(v, method, side) {
  if(length(na.omit(v)) == 0) 
    return(v)
  
  v.s <- lapply(split(v, v), seq_along)
  
  if(method %in% c("centre", "square") && side == -1)
    v.s <- lapply(v.s, function(a) a - max(a))
  else if(method %in% c("centre", "square") && side == 1)
    v.s <- lapply(v.s, function(a) a - 1)
  else if(method == "centre")
    v.s <- lapply(v.s, function(a) a - mean(a))
  else if(method == "square")
    v.s <- lapply(v.s, function(a) a - floor(mean(a)))
  else if(method == "hex") {
    odd.row <- (as.numeric(names(v.s)) %% 2) == 1
    if(side == 0) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - floor(mean(a)) - 0.25)
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - ceiling(mean(a)) + 0.25)
    } else if(side == -1) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - max(a))
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - max(a) - 0.5)
    } else if(side ==  1) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - 1)
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - 0.5)
    }
  }
  unsplit(v.s, v)
} 
