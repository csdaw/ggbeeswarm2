
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggbeeswarm2: Beeswarm-style plots with ggplot2

## Note

This is a fork of ggbeeswarm under active development.

The `position_beeswarm` function has been rewritten and is compatible
with R \>= v4.0.0. Additionally, `position_beeswarm` works almost
identically to the `beeswarm` function from the
[beeswarm](https://cran.r-project.org/web/packages/beeswarm/index.html)
package.

For example:

``` r
set.seed(123)
distro <- list(runif = runif(100, min = -3, max = 3), 
               rnorm = rnorm(100))

# beeswarm
beeswarm::beeswarm(distro, 
                   col = 2:3, pch = 16,
                   method = "hex",
                   main = "title")
```

    ## [1] "-3.05587006024551 dlim!" "3.02523735994007 dlim!" 
    ## [1] 0.01680672
    ## [1] 0.01680672

<img src="man/figures/README-ex-1.png" width="576" />

    ## [1] 0.03630252
    ## [1] 1
    ## [1] 0.03630252

``` r
# ggbeeswarm2
library(dplyr)
library(ggbeeswarm2)

distro2 <- as.data.frame(rev(distro)) %>% 
  tidyr::pivot_longer(
    everything(), 
    values_to = "value", 
    names_to = "variable"
    )

 ggplot2::ggplot(distro2, aes(x = rev(variable), y = value)) + 
   geom_point(position = ggbeeswarm2::position_beeswarm(method = "hex")) + 
   scale_y_continuous(limits = c(-3, 3)) + 
   theme(
     plot.margin = unit(c(0.11, 0.07, 0.11, 0.07), "npc")
   )
```

<img src="man/figures/README-ex-2.png" width="576" />

-----

Author: Charlotte Dawson

Original authors: Erik Clarke and Scott Sherrill-Mix
