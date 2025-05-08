library("ggplot2")
f <- function(x) 5*x^4 + 4*x^3 + x^2 + 3*x + 10 + 10*sin(x)
gradient <- function(x) 20*x^3 + 12*x^2 + 2*x + 3 + 10*cos(x)
hessian <- function(x) matrix(60*x^2 + 24*x + 2 - 10*sin(x), 1, 1)
track <- nlm_track(f = f, p = 2, gradient = gradient, hessian = hessian)
background <- autoplot(track) +
  theme_void() +
  scale_y_log10() +
  labs(title = NULL, subtitle = NULL) +
  geom_point(size = 4, col = "red", shape = 3)
sticker <- oeli::package_logo(
  "trackopt", background = background, s_width = 1.5, s_height = 1,
  white_around_sticker = FALSE, brackets = TRUE, s_y = 0.9
)
print(sticker)
ggsave("sticker.png", path = "man", scale = 0.3)
usethis::use_logo("man/sticker.png")
