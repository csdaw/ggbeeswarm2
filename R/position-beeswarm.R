#' Separate coincident points with the beeswarm package
#' 
#' @description Offset points to avoid overplotting using the 
#' \code{\link{swarmx}} function from the `beeswarm` package.
#' 
#' @details 
#' **method:** specifies the algorithm used to avoid overlapping points. The 
#' default `"swarm"` method places points in increasing order. If a point would
#' overlap with an existing point, it is shifted sideways (along the group axis)
#' by a minimal amount sufficient to avoid overlap. 
#' 
#' Whereas the `"swarm"` method places points in a predetermined order, the 
#' `"compactswarm"` method uses a greedy strategy to determine which point will
#' be placed next. This often leads to a more tightly-packed layout. The 
#' strategy is very simple: on each iteration, a point that can be placed as 
#' close as possible to the non-data axis is chosen and placed. If there are two 
#' or more equally good points, `priority` is used to break ties.
#' 
#' The other 3 methods first discretise the values along the data axis, in order
#' to create more efficient packing. The `"square"` method places points on a 
#' square grid, whereas `"hex"` uses a hexagonal grid. `"centre"`/`"center"` 
#' uses a square grid to produce a symmetric swarm. The number of break points 
#' for discretisation is determined by a combination of the available plotting 
#' area and the `spacing` argument.
#' 
#' **priority:** controls the order in which points are placed, which generally 
#' has a noticeable effect on the plot appearance. `"ascending"` gives the 
#' 'traditional' beeswarm plot. `"descending"` is the opposite. `"density"` 
#' prioritizes points with higher local density. `"random"` places points in a 
#' random order. `"none"` places points in the order provided.
#' 
#' **corral:** By default, swarms from different groups are not prevented from
#' overlapping, i.e. `"corral = "none"`. Thus, datasets that are very large or 
#' unevenly distributed may produce ugly overlapping beeswarms. To control 
#' runaway points one can use the following methods. `"gutter"` collects runaway
#' points along the boundary between groups. `"wrap"` implement periodic boundaries.
#' `"random"` places runaway points randomly in the region. `"omit"` omits runaway
#' points.
#'
#' @param method `string`. Method for arranging points, default is `"swarm"`. See
#' details below.
#' @param spacing `numeric`. Relative spacing between points, default is `1`. 
#' You should adjust this if you change the size of the points. Generally the
#' spacing should be 2/3 of the point size i.e. if `size = 3`, then `spacing = 2`,
#' but this is ultimately up to personal preference.
#' @param side `integer`. Direction to perform jittering: use `0L` for both directions;
#' `1L` for right/upwards; `-1L` for left/downwards.
#' @param priority `string`. Method used to perform point layout when method is 
#' `"swarm"` or `"compactswarm`, default is 
#' `"ascending"`; ignored otherwise. See details below.
#' @param fast Use compiled version of algorithm? This option is ignored for all
#' methods except `"swarm"` and `"compactswarm"`.
#' @param dodge.width `numeric`. Amount to dodge points from different aesthetic
#' groups, default is `NULL` for no dodging.
#' @param corral `string`. Method used to adjust points that would be placed to
#' wide horizontally, default is `"none"`. See details below.
#' @param corral.width `numeric`. Width of the corral, default is `0.2`.
#' 
#' @seealso [position_quasirandom()]
#'
#' @examples
#' #
#' @export
position_beeswarm <- function(method = "swarm", spacing = 1,
                              side = 0L, priority = "ascending",
                              fast = TRUE, dodge.width = NULL,
                              corral = "none", corral.width = 0.2) {
  match.arg(method, c("swarm", "compactswarm", "square", "hex", "centre", "center"))
  
  if (method %in% "center") method <- "centre"
  
  ggproto(NULL, PositionBeeswarm,
          method = method,
          spacing = spacing,
          side = side, 
          priority = priority,
          fast = fast,
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
                                side = self$side,
                                priority = self$priority,
                                fast = self$fast,
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
                                plot.ylim <- .beeint$expand_range4(scales$x$get_limits(), c(0.045, 0))
                                plot.xlim <- .beeint$expand_range4(c(1, length(scales$y$get_limits())), c(0, 0.6))
                              } else {
                                plot.ylim.short <- scales$y$get_limits()
                                plot.ylim <- .beeint$expand_range4(scales$y$get_limits(), c(0.045, 0))
                                plot.xlim <- .beeint$expand_range4(c(1, length(scales$x$get_limits())), c(0, 0.6))
                              }
                              
                              # capture current par values
                              current.usr <- graphics::par("usr")
                              current.mar <- graphics::par("mar")
                              # on exit return par to normal
                              on.exit(graphics::par("usr" = current.usr, "mar" = current.mar), add = TRUE)
                              
                              data <- .beeint$collide(
                                data,
                                params$dodge.width,
                                name = "position_beeswarm",
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
                                pos_beeswarm,
                                plot.ylim.short = plot.ylim.short,
                                plot.xlim = plot.xlim, plot.ylim = plot.ylim,
                                y.lim = params$y.lim,
                                method = params$method,
                                spacing = params$spacing,
                                side = params$side,
                                priority = params$priority,
                                fast = params$fast,
                                corral = params$corral,
                                corral.width = params$corral.width
                              )
                              
                              # recombine list of data.frames into one
                              data <- Reduce(rbind, data)
                              
                              flip_data(data, params$flipped_aes)
                            }
)

pos_beeswarm <- function(df, plot.ylim.short, plot.xlim, plot.ylim, y.lim, 
                         method = "swarm", spacing = 1,
                         side = 0L, priority = "ascending", fast = TRUE,
                         corral = "none", corral.width = 0.2) {
  if (method %in% c("swarm", "compactswarm")) {
    # adjust par("usr") based on input data
    graphics::par("usr" = c(plot.xlim, plot.ylim.short),
                  "mar" = c(1.9, 1.9, 0.3, 0.3))
    
    compact <- method == "compactswarm"
    
    x.offset <- beeswarm::swarmx(
      x = rep(0, length(df$y)), y = df$y,
      cex = spacing, side = side, priority = priority,
      fast = fast, compact = compact
    )$x
  } else {
    ## non-swarm methods
    # adjust par("usr") based on input data
    graphics::par("usr" = c(plot.xlim, plot.ylim.short),
                  "mar" = c(1.9, 1.9, 0.3, 0.3))
    
    # define size.x and size.y
    size.x <- graphics::xinch(0.08, warn.log = FALSE) * spacing
    size.y <- graphics::yinch(0.08, warn.log = FALSE) * spacing
    
    # hex method specific step
    if (method == "hex") size.y <- size.y * sqrt(3) / 2
    
    ## first determine positions along the y axis
    breaks <- seq(y.lim[1], y.lim[2] + size.y, by = size.y)
    
    mids <- (utils::head(breaks, -1) + utils::tail(breaks, -1)) / 2
    y.index <- sapply(df$y, cut, breaks = breaks, labels = FALSE)
    
    y.pos <- sapply(y.index, function(a) mids[a])  
    df$y <- y.pos
    
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
          yes = stats::runif(length(zz), corral.low, corral.high), 
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

determine_pos <- function(v, method, side) {
  # if(length(stats::na.omit(v)) == 0) 
  #   return(v)
  
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
