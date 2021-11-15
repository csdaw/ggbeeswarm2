
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggbeeswarm2: Beeswarm-style plots with ggplot2

<!-- badges: start -->

[![R-CMD-check](https://github.com/csdaw/ggbeeswarm2/workflows/R-CMD-check/badge.svg)](https://github.com/csdaw/ggbeeswarm2/actions)
[![Codecov test
coverage](https://codecov.io/gh/csdaw/ggbeeswarm2/branch/master/graph/badge.svg)](https://codecov.io/gh/csdaw/ggbeeswarm2?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Note

This is a fork of ggbeeswarm which is in an experimental and unfinished
state. **Some of changes made here including additional swarming and
corral methods are now available in the development version of
[ggbeeswarm](https://github.com/eclarke/ggbeeswarm), which is probably
what you should be using.**

I plan on trying out re-writing these functions (again) so that the
swarming is performed based on the plotting area and size of the
plotting character as opposed to just the x/y range of the data (as in
ggbeeswarm).

The `position_beeswarm` function has been rewritten and is compatible
with R \>= v4.0.0. Additionally, `position_beeswarm` works almost
identically to the `beeswarm` function from the
[beeswarm](https://cran.r-project.org/web/packages/beeswarm/index.html)
package with all the additional methods implemented (including the new
`compactswarm`).

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

    ## [1] "par(\"usr\") = 0" "par(\"usr\") = 1" "par(\"usr\") = 0" "par(\"usr\") = 1"
    ## [1] "cex.out[1]:  1"

    ## [1] "par(\"cex\"): 1"
    ## [1] "cex: 1"
    ## [1] "spacing: 1"
    ## [1] "sizeMultiplier: 1"
    ## [1] "VERTICAL!"
    ## [1] "x.size = 0.0363025210084034"
    ## [1] "y.size = 0.243244296807423"

``` r
# ggbeeswarm2
library(dplyr)
library(ggbeeswarm2)
```

<img src="man/figures/README-ex-1.png" width="576" />

``` r
distro2 <- as.data.frame(rev(distro)) %>% 
  tidyr::pivot_longer(
    everything(), 
    values_to = "value", 
    names_to = "variable"
    )

 ggplot2::ggplot(distro2, aes(x = rev(variable), y = value)) + 
   geom_beeswarm(method = "hex") + 
   scale_y_continuous(limits = c(-3, 3))
```

<img src="man/figures/README-ex-2.png" width="576" />

    ## [1] "plot.ylim.short = -3" "plot.ylim.short = 3" 
    ## [1] "y.range = 6"

------------------------------------------------------------------------

Author: Charlotte Dawson

Original authors: Erik Clarke and Scott Sherrill-Mix
