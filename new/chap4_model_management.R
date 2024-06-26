source("header.R")
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)
library(ggplot2)


x <- seq(0,3.1,0.1)
y1 <- sin(x)

x <- seq(3.2,4.7,0.1)
y <- (x - 3.2)*1/1.5
x <- seq(3.2,6.2,0.1)
y2 <- c(y[1:(length(y)-1)], rev(y))

x <- seq(4.8,5.8,0.1)
y <- (x - 4.8)*1/2
x <- seq(4.8,7.8,0.1)
y3 <- c(y[1:(length(y)-1)], rep(0.5, 10), rev(y))

y4 <- 0.8*y1

x <- seq(0,12.5,0.1)
y <- c(y1, y2, y3, y4)

data <- data.frame(x, y)

grf <- ggplot(data, aes(x=x, y=y)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1) +  
  annotate(geom="text", x=-0.2, y=0.1, label="M[0]", color="black", parse = TRUE) +  
  geom_vline(xintercept = 3.2, linetype="dotted", color = "black", size=1) +
  annotate(geom="text", x=1.6, y=0.1, label="M[1]", color="black", parse = TRUE) +
  geom_vline(xintercept = 6.25, linetype="dotted", color = "black", size=1) +
  annotate(geom="text", x=4.7, y=0.1, label="M[2]", color="black", parse = TRUE) +
  geom_vline(xintercept = 9.35, linetype="dotted", color = "black", size=1) +
  annotate(geom="text", x=7.8, y=0.1, label="M[3]", color="black", parse = TRUE) +  
  annotate(geom="text", x=10.9, y=0.1, label="?M", color="black", parse = TRUE) +    
  theme_classic()

save_png(grf, "new/chap4_model_management.png", 1280, 720)