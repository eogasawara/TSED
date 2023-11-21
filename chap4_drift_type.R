source("header.R")
library(daltoolbox)
library(harbinger)

set.seed(1)

data_a <- function() {
  data <- NULL
  data <- c(data, c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, 10+c(2, 1, 2, 3, 2, 1, 2, 3))
  data <- c(data, 10+c(2, 1, 2, 3, 2, 1, 2, 3))
  return(data+10)
}

data_b <- function() {
  data <- NULL
  data <- c(data, c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, c(0, 0, 0, 0, 1.2, 2.4, 3.6, 4.8)+c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, c(6.0,7.2,8.4,9.6,10,10,10,10)+c(2, 1, 2, 3, 2, 1, 2, 3))
  data <- c(data, 10+c(2, 1, 2, 3, 2, 1, 2, 3))
  return(data+10)
}

data_c <- function() {
  data <- NULL
  data <- c(data, c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, c(0, 0, 10, 0, 0, 10, 0, 0)+c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, c(10, 10, 0, 10, 10, 0, 10, 10)+c(2, 1, 2, 3, 2, 1, 2, 3))
  data <- c(data, 10+c(2, 1, 2, 3, 2, 1, 2, 3))
  return(data+10)
}

data_d <- function() {
  data <- NULL
  data <- c(data, c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, 10+c(2, 1, 2, 3, 2, 1, 2, 3))
  data <- c(data, c(2, 1, 2, 3, 2, 1, 2, 3)) 
  data <- c(data, 10+c(2, 1, 2, 3, 2, 1, 2, 3))
  return(data+10)
}

a <- data_a()
model <- fit(harbinger(), a)
detection <- detect(model, a)
detection$event[c(16)] <- TRUE
detection$type[c(16)] <- "anomaly"

grf <- har_plot(model, a, detection)
grf <- grf + scale_x_continuous(breaks=seq(0,32,8))
grf <- grf + ylab(" ") + ylim(0, 30)
grf <- grf + geom_segment(aes(x = 16, y = 8, xend = 16, yend = 25), col = "black", linetype = "dashed")
grf <- grf + font 
grf <- grf + annotate(geom="text", x=16, y=1, label="(a)", color="black")
grf <- grf + annotate(geom="text", x=4, y=29, label="SD[1]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=12, y=29, label="SD[2]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=20, y=29, label="SD[3]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=28, y=29, label="SD[4]", color="darkblue", parse=TRUE)
grf <- grf + geom_segment(aes(x = 0, y = 27, xend = 0, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 8, y = 27, xend = 8, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 16, y = 27, xend = 16, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 24, y = 27, xend = 24, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 32, y = 27, xend = 32, yend = 30), col = "darkblue")
grfA <- grf

b <- data_b()
model <- fit(harbinger(), b)
detection <- detect(model, b)
detection$event[c(10,20)] <- TRUE
detection$type[c(10,20)] <- "anomaly"

grf <- har_plot(model, b, detection)
grf <- grf + scale_x_continuous(breaks=seq(0,32,8))
grf <- grf + ylab(" ") + ylim(0, 30)
grf <- grf + geom_segment(aes(x = 8, y = 8, xend = 8, yend = 25), col = "black", linetype = "dashed")
grf <- grf + geom_segment(aes(x = 24, y = 8, xend = 24, yend = 25), col = "black", linetype = "dashed")
grf <- grf + font
grf <- grf + annotate(geom="text", x=16, y=1, label="(b)", color="black")
grf <- grf + annotate(geom="text", x=4, y=29, label="SD[1]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=12, y=29, label="SD[2]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=20, y=29, label="SD[3]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=28, y=29, label="SD[4]", color="darkblue", parse=TRUE)
grf <- grf + geom_segment(aes(x = 0, y = 27, xend = 0, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 8, y = 27, xend = 8, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 16, y = 27, xend = 16, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 24, y = 27, xend = 24, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 32, y = 27, xend = 32, yend = 30), col = "darkblue")
grfB <- grf

c <- data_c()
model <- fit(harbinger(), c)
detection <- detect(model, c)
detection$event[c(10,23)] <- TRUE
detection$type[c(10,23)] <- "anomaly"

grf <- har_plot(model, c, detection)
grf <- grf + scale_x_continuous(breaks=seq(0,32,8))
grf <- grf + ylab(" ") + ylim(0, 30)
grf <- grf + geom_segment(aes(x = 8, y = 8, xend = 8, yend = 25), col = "black", linetype = "dashed")
grf <- grf + geom_segment(aes(x = 24, y = 8, xend = 24, yend = 25), col = "black", linetype = "dashed")
grf <- grf + font
grf <- grf + annotate(geom="text", x=16, y=1, label="(c)", color="black")
grf <- grf + annotate(geom="text", x=4, y=29, label="SD[1]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=12, y=29, label="SD[2]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=20, y=29, label="SD[3]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=28, y=29, label="SD[4]", color="darkblue", parse=TRUE)
grf <- grf + geom_segment(aes(x = 0, y = 27, xend = 0, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 8, y = 27, xend = 8, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 16, y = 27, xend = 16, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 24, y = 27, xend = 24, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 32, y = 27, xend = 32, yend = 30), col = "darkblue")
grfC <- grf

d <- data_d()
model <- fit(harbinger(), d)
detection <- detect(model, d)
detection$event[c(8,16,24)] <- TRUE
detection$type[c(8,16,24)] <- "anomaly"

grf <- har_plot(model, d, detection)
grf <- grf + scale_x_continuous(breaks=seq(0,32,8))
grf <- grf + ylab(" ") + ylim(0, 30)
grf <- grf + geom_segment(aes(x = 8, y = 8, xend = 8, yend = 25), col = "black", linetype = "dashed")
grf <- grf + geom_segment(aes(x = 16, y = 8, xend = 16, yend = 25), col = "black", linetype = "dashed")
grf <- grf + geom_segment(aes(x = 24, y = 8, xend = 24, yend = 25), col = "black", linetype = "dashed")
grf <- grf + font
grf <- grf + annotate(geom="text", x=16, y=1, label="(d)", color="black")
grf <- grf + annotate(geom="text", x=4, y=29, label="SD[1]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=12, y=29, label="SD[2]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=20, y=29, label="SD[3]", color="darkblue", parse=TRUE)
grf <- grf + annotate(geom="text", x=28, y=29, label="SD[4]", color="darkblue", parse=TRUE)
grf <- grf + geom_segment(aes(x = 0, y = 27, xend = 0, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 8, y = 27, xend = 8, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 16, y = 27, xend = 16, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 24, y = 27, xend = 24, yend = 30), col = "darkblue")
grf <- grf + geom_segment(aes(x = 32, y = 27, xend = 32, yend = 30), col = "darkblue")
grfD <- grf

mypng(file="figures/chap4_drift_type.png", width = 1280, height = 720) #144 #720*1.75
gridExtra::grid.arrange(grfA, grfB, grfC, grfD,
                        layout_matrix = matrix(c(1,1,2,2,3,3,4,4), byrow = TRUE, ncol = 4))
dev.off()  

