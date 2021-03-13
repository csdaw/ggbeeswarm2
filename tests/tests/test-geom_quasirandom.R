#### Setup ---------------------------------------------------------------------
tg <- ToothGrowth
tg$dose <- factor(tg$dose)

#### Tests ---------------------------------------------------------------------
# test that geom_quasirandom defaults work
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_quasirandom()

expect_silent(ggplotGrob(g))

# test geom_quasirandom pseudorandom method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_quasirandom(method = "pseudorandom")

expect_silent(ggplotGrob(g))

# test geom_quasirandom maxout method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_quasirandom(method = "maxout")

expect_silent(ggplotGrob(g))

# test geom_quasirandom minout method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_quasirandom(method = "minout")

expect_silent(ggplotGrob(g))

# test geom_quasirandom tukey method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_quasirandom(method = "tukey")

expect_silent(ggplotGrob(g))

# test geom_quasirandom tukeyDense method
g <- ggplot(tg, aes(x = dose, y = len)) + 
  geom_quasirandom(method = "tukeyDense")

expect_silent(ggplotGrob(g))

# test that geom_quasirandom width can be varied
g <- ggplot(tg, aes(x = dose, y = len, colour = supp)) + 
  geom_quasirandom(varwidth = TRUE)

expect_silent(ggplotGrob(g))

# test that geom_quasirandom can be dodged
g <- ggplot(tg, aes(x = dose, y = len, colour = supp)) + 
  geom_quasirandom(dodge.width = 0.8)

expect_silent(ggplotGrob(g))
