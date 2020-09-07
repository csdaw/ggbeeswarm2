#' @export
position_beeswarm <- function(method = "swarm", spacing = 1, breaks = NULL,
                              side = 0L, priority = "ascending",
                              dodge.width = NULL,
                              corral = "none", corral.width = 0.2) {
  ggproto(NULL, PositionBeeswarm,
          method = method,
          spacing = spacing,
          breaks = breaks,
          side = side, 
          priority = priority,
          dodge.width = dodge.width,
          corral = corral,
          corral.width = corral.width
  )
}

PositionBeeswarm <- ggproto("PositionBeeswarm", Position,
                            required_aes = c("x", "y"),
                            
                            setup_params = function(self, data) {
                              flipped_aes <- has_flipped_aes(data)
                              data <- flip_data(data, flipped_aes)
                              
                              # get y range of data and extend it a little
                              y.lim <- grDevices::extendrange(data$y, f = 0.01)
                              
                              list(
                                method = self$method,
                                spacing = self$spacing,
                                breaks = self$breaks,
                                side = self$side,
                                priority = self$priority,
                                dodge.width = self$dodge.width,
                                corral = self$corral,
                                corral.width = self$corral.width,
                                y.lim = y.lim,
                                flipped_aes = flipped_aes
                              )
                            },
                            
                            compute_panel = function(data, params, scales) {
                              data <- flip_data(data, params$flipped_aes)
                              
                              # get plot limits
                              if (params$flipped_aes) {
                                plot.ylim.short <- scales$x$get_limits()
                                plot.ylim <- ggplot2:::expand_range4(scales$x$get_limits(), c(0.045, 0))
                                plot.xlim <- ggplot2:::expand_range4(c(1, length(scales$y$get_limits())), c(0, 0.6))
                              } else {
                                plot.ylim.short <- scales$y$get_limits()
                                plot.ylim <- ggplot2:::expand_range4(scales$y$get_limits(), c(0.045, 0))
                                plot.xlim <- ggplot2:::expand_range4(c(1, length(scales$x$get_limits())), c(0, 0.6))
                              }
                              
                              # capture current par values
                              current.par <- par("usr")
                              
                              data <- ggplot2:::collide(
                                data,
                                params$dodge.width,
                                name = "position_beeswarm",
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
                                pos_beeswarm,
                                plot.ylim.short = plot.ylim.short,
                                plot.xlim = plot.xlim, plot.ylim = plot.ylim,
                                y.lim = params$y.lim,
                                method = params$method,
                                spacing = params$spacing,
                                breaks = params$breaks,
                                side = params$side,
                                priority = params$priority,
                                corral = params$corral,
                                corral.width = params$corral.width
                              )
                              
                              # recombine list of data.frames into one
                              data <- Reduce(rbind, data)
                              
                              # return par("usr") to normal
                              par("usr" = current.par)
                              
                              flip_data(data, params$flipped_aes)
                            }
)

pos_beeswarm <- function(df, plot.ylim.short, plot.xlim, plot.ylim, y.lim, 
                         method = "swarm", spacing = 1, breaks = NULL,
                         side = 0L, priority = "ascending", corral = "none",
                         corral.width = 0.2) {
  if (method == "swarm") {
    # adjust par("usr") based on input data
    par("usr" = c(plot.xlim, plot.ylim.short))
    
    x.offset <- beeswarm::swarmx(
      x = rep(0, length(df$y)), y = df$y,
      cex = spacing, side = side, priority = priority
    )$x
  } else {
    ## non-swarm methods
    # adjust par("usr") based on input data
    par("usr" = c(plot.xlim, plot.ylim))
    
    # define size.x and size.y
    size.x <- xinch(0.08, warn.log = FALSE) * spacing
    size.y <- yinch(0.08, warn.log = FALSE) * spacing
    
    # hex method specific step
    if (method == "hex") size.y <- size.y * sqrt(3) / 2
    
    ## first determine positions along the y axis
    if(is.null(breaks))
      breaks <- seq(y.lim[1], y.lim[2] + size.y, by = size.y)
    
    if(length(breaks) == 1 && is.na(breaks[1])) {
      y.index <- df$y
      d.pos <- df$y
    } else {
      mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
      y.index <- sapply(df$y, cut, breaks = breaks, labels = FALSE)
      
      y.pos <- sapply(y.index, function(a) mids[a])  
      df$y <- y.pos
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
  
  df$x <- df$x + x.offset
  df
}
