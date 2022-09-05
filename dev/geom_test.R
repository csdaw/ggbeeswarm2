library(ggbeeswarm2)
library(dplyr)
library(ggplot2)

debugonce(grid:::makeContent)

data.frame(
  x = 1:3,
  y = rep(1, 3)
) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_test()
