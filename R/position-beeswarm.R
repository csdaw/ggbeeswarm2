#' An internal function to calculate new positions for geom_beeswarm
#' 
#' @family position adjustments
#' @param data A data.frame containing plotting data in columns x and y. Usually obtained from data processed by ggplot2.
#' @param xRange span of the x axis e.g. yMax-yMin
#' @param yRange span of the y axis e.g. yMax-yMin
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @param cex Scaling for adjusting point spacing (see \code{\link{swarmx}})
#' @param groupOnX if TRUE then jitter is added to the x axis and if FALSE jitter is added to the y axis. Prior to v0.6.0, the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each. This could result in unexpected results when the x variable has few unique values and so in v0.6.0 the default was changed to always jitter on the x axis unless groupOnX=FALSE. Also consider \code{\link[ggplot2]{coord_flip}}.
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @param beeswarmArgs A list of additional arguments to be passed to the \link{swarmx} function of beeswarm e.g. \code{list(side=1)} or \code{list(side=-1)} to only distribute points to the right/left side
#' @param oSize A 2 element vector giving the width and height of a cex=1 character "o" in user coordinates
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{geom_beeswarm}}, \code{\link{position_quasirandom}}, \code{\link[beeswarm]{swarmx}}
offset_beeswarm= function(data,xRange=1,yRange=1,priority = c("ascending", "descending", "density", "random", "none"), cex=1, groupOnX=NULL, dodge.width=0, beeswarmArgs=list(),oSize=c(1/200,1/200)){
  # Adjust function is used to calculate new positions (from ggplot2:::Position)
  data <- remove_missing(data, vars = c("x","y"), name = "position_beeswarm")
  if (nrow(data)==0) return(data.frame())

  if(is.null(groupOnX)){
    groupOnX<-TRUE
    if(length(unique(data$y)) <= length(unique(data$x))) warning('The default behavior of beeswarm has changed in version 0.6.0. In versions <0.6.0, this plot would have been dodged on the y-axis.  In versions >=0.6.0, groupOnX=FALSE must be explicitly set to group on y-axis. Please set groupOnX=TRUE/FALSE to avoid this warning and ensure proper axis choice.')
  }

  #divisors are magic numbers to get a reasonable base spacing
  #note that the base beeswarm package is not 100% precise with differing plotting sizes resulting in variation in points overlap/spacing
  xSize<-xRange*oSize[1]/.71
  ySize<-yRange*oSize[2]/.92
  offset<-stats::ave(
    data[,ifelse(groupOnX,'y','x')],
    data[,ifelse(groupOnX,'x','y')],
    FUN=function(yy){
      if (length(yy) == 1) return(0)
      else do.call(beeswarm::swarmx,c(list(
        0,
        yy,
        cex=1,
        priority=priority,
        xsize=ifelse(groupOnX,xSize,ySize),
        ysize=ifelse(groupOnX,ySize,xSize)
      ),beeswarmArgs)
    )$x
    }
  )
  data[,ifelse(groupOnX,'x','y')]<-data[,ifelse(groupOnX,'x','y')]+offset

  return(data)
}

#' @export
position_beeswarm <- function(method = "swarm", spacing = 1, breaks = NULL,
                              side = 0L, priority = "ascending",
                              corral = "none", corral.width = 0.889) {
  
  ggproto(NULL, PositionBeeswarm,
          method = method,
          spacing = spacing,
          breaks = breaks,
          side = side, 
          priority = priority,
          corral = corral,
          corral.width = corral.width
  )
}

PositionBeeswarm <- ggproto("PositionBeeswarm", Position,
                            method = "swarm", spacing = 1, breaks = NULL,
                            side = 0L, priority = "ascending", corral = "none",
                            corral.width = 0.889,
                            
                            setup_params = function(self, data) {
                              flipped_aes <- has_flipped_aes(data)
                              data <- flip_data(data, flipped_aes)
                              
                              # define n.groups 
                              n.groups <- length(unique(data$group))
                              
                              # get y range of data and extend it a little
                              y.lim <- grDevices::extendrange(data$y, f = 0.01)
                              
                              list(
                                method = self$method,
                                spacing = self$spacing,
                                breaks = self$breaks,
                                side = self$side,
                                priority = self$priority,
                                corral = self$corral,
                                corral.width = self$corral.width,
                                n.groups = n.groups,
                                y.lim = y.lim,
                                flipped_aes = flipped_aes
                              )
                            },
                            # setup_data = function(self, data, params) {
                            #   data <- flip_data(data, params$flipped_aes)
                            #   if (!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
                            #     data$x <- (data$xmin + data$xmax) / 2
                            #   }
                            #   flip_data(data, params$flipped_aes)
                            # },
                            compute_panel = function(data, params, scales) {
                              data <- flip_data(data, params$flipped_aes)
                              print(params)
                              flip_data(data, params$flipped_aes)
                            }
)
# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodge <- function(df, width, n = NULL) {
  if (is.null(n)) {
    n <- length(unique(df$group))
  }
  if (n == 1)
    return(df)
  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }
  d_width <- max(df$xmax - df$xmin)
  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))
  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2
  df
}
