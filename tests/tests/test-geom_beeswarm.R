#### Setup ---------------------------------------------------------------------
tg <- ToothGrowth
tg$dose <- factor(tg$dose)

#### Tests ---------------------------------------------------------------------

# test that geom_beeswarm defaults work
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm()

expect_silent(ggplotGrob(g))
