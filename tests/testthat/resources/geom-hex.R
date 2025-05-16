library(ggplot2)

d <- ggplot(diamonds, aes(carat, price))
d + geom_hex()
