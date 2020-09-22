#### Get required internal ggplot2 functions -----------------------------------

# Copied from https://github.com/teunbrand/ggh4x/tree/master/R/utils.R
# Function for grabbing internal function of ggplot2 that are also used here
.grab_ggplot_internals <- function() {
  objects <- c(
    "collide",
    "expand_range4",
    "pos_dodge"
  )
  
  objects <- stats::setNames(objects, objects)
  out <- lapply(objects, function(i) utils::getFromNamespace(i, "ggplot2"))
}

# Store the needed ggplot internals here
.beeint <- .grab_ggplot_internals()
