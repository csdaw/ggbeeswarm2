#### Setup ---------------------------------------------------------------------
tg <- ToothGrowth
tg$dose <- factor(tg$dose)

#### Tests ---------------------------------------------------------------------

# test that geom_beeswarm defaults work
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm()

expect_silent(ggplotGrob(g))

# test geom_beeswarm square method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "square")

expect_silent(ggplotGrob(g))

# test geom_beeswarm square method with side = -1L
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "square", side = -1L)

expect_silent(ggplotGrob(g))

# test geom_beeswarm square method with side = 1L
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "square", side = 1L)

expect_silent(ggplotGrob(g))

# test geom_beeswarm hex method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "hex")

expect_silent(ggplotGrob(g))

# test geom_beeswarm hex method with side = -1L
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "hex", side = -1L)

expect_silent(ggplotGrob(g))

# test geom_beeswarm hex method with side = 1L
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "hex", side = 1L)

expect_silent(ggplotGrob(g))

# test geom_beeswarm centre method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(method = "centre")

expect_silent(ggplotGrob(g))

# test that geom_beeswarm corral method gutter works
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(spacing = 3, corral = "gutter")

expect_silent(ggplotGrob(g))

# test that geom_beeswarm corral method wrap works
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(spacing = 3, corral = "wrap")

expect_silent(ggplotGrob(g))

# test that geom_beeswarm corral method wrap with side = -1L works
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(spacing = 3, corral = "wrap", side = -1L)

expect_silent(ggplotGrob(g))

# test that geom_beeswarm corral method random works
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(spacing = 3, corral = "random")

expect_silent(ggplotGrob(g))

# test that geom_beeswarm corral method omit works with a warning
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_beeswarm(spacing = 3, corral = "omit")

expect_warning(ggplotGrob(g))
