require(ggplot2)


# Uniform
x <- seq(-1,2,0.0001)
y <- dunif(x)
data <- data.frame(x,y)


ggplot(data = data) + aes(x = x, y = y) + geom_line(size = 1.5) +
  theme_bw() + theme(axis.title.y = element_text(angle = 0, hjust = 1)) +
  ylab("f(x)") + scale_x_continuous(limits = c(-0.25, 1.25)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25))


# Normal
x <- seq(-5,5,0.001)
y <- dnorm(x)
data <- data.frame(x,y)


ggplot(data = data) + aes(x = x, y = y) + geom_line(size = 1.5) +
  theme_bw() + theme(axis.title.y = element_text(angle = 0, hjust = 1)) +
  ylab("f(x)") + scale_x_continuous(limits = c(-4, 460)) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.10))




