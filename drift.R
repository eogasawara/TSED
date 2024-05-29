source("header.R")
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)
library(ggplot2)


x <- seq(0,3.1,0.1)
y <- sin(x)

x <- seq(3.2,4.7,0.1)
y <- (x - 3.2)*1/1.5
x <- seq(3.2,6.2,0.1)
y <- c(y[1:(length(y)-1)], rev(y))

data <- data.frame(x, y)

ggplot(data, aes(x=x, y=y)) +
  geom_line() +
  theme_classic()