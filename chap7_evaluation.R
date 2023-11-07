source("header.R")

set.seed(1)

data_a <- function() {
  data <- NULL
  data <- c(data, rep(10, 10)) 
  data <- c(data, rev(seq(1, 10, 1)^2/10))
  data <- c(data, rep(0.1, 10)) 
  return(data+5)
}


data <- data_a()
model <- fit(harbinger(), data, detection, event)
detection <- detect(model, data)
event <- rep(FALSE, length(data))

grfA <- function(data, detection, event) {
  t <- 11
  i <- 1:t
  data <- data[i]
  detection <- detection[i,]
  event <- event[i]
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(0, 20) + xlim(0, 30)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=11, y=16.2, label="E[1]", color="red", parse=TRUE)
  grf <- grf + geom_point(aes(11, 15), colour = "red", size = 1)  
  grf <- grf + annotate(geom="text", x=2, y=19, label="(a) - time = 11", color="black", parse=FALSE)
}

grfB <- function(data, detection, event) {
  t <- 13
  i <- 1:t
  data <- data[i]
  detection <- detection[i,]
  event <- event[i]
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(0, 20) + xlim(0, 30)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=11, y=16.2, label="E[1]", color="red", parse=TRUE)
  grf <- grf + annotate(geom="text", x=11, y=16.2, label="E[1]", color="red", parse=TRUE)
  grf <- grf + annotate(geom="text", x=12.2, y=14.2, label="B", color="blue", parse=TRUE)
  grf <- grf + geom_point(aes(11, 15), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(12, data[12]), colour = "blue", size = 1.25)  
  grf <- grf + annotate(geom="text", x=2, y=19, label="(b) - time = 13", color="black", parse=FALSE)
}

grfC <- function(data, detection, event) {
  t <- 15
  i <- 1:t
  data <- data[i]
  detection <- detection[i,]
  event <- event[i]
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(0, 20) + xlim(0, 30)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=11, y=16.2, label="E[1]", color="red", parse=TRUE)
  grf <- grf + annotate(geom="text", x=12.2, y=14.2, label="B", color="blue", parse=TRUE)
  grf <- grf + annotate(geom="text", x=9, y=16.2, label="A", color="blue", parse=TRUE)
  grf <- grf + geom_point(aes(11, 15), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(9, 15), colour = "blue", size = 1.25)  
  grf <- grf + geom_point(aes(12, data[12]), colour = "blue", size = 1.25)  
  grf <- grf + annotate(geom="text", x=2, y=19, label="(c) - time = 15", color="black", parse=FALSE)
}

grfD <- function(data, detection, event) {
  t <- 28
  i <- 1:t
  data <- data[i]
  detection <- detection[i,]
  event <- event[i]
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(0, 20) + xlim(0, 30)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=11, y=16.2, label="E[1]", color="red", parse=TRUE)
  grf <- grf + annotate(geom="text", x=20, y=6.2, label="E[2]", color="red", parse=TRUE)
  grf <- grf + annotate(geom="text", x=12.2, y=14.2, label="B", color="blue", parse=TRUE)
  grf <- grf + annotate(geom="text", x=10, y=16.2, label="A", color="blue", parse=TRUE)
  grf <- grf + geom_point(aes(11, 15), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(20, 5), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(10, 15), colour = "blue", size = 1.25)  
  grf <- grf + geom_point(aes(12, data[12]), colour = "blue", size = 1.25)  
  grf <- grf + annotate(geom="text", x=2, y=19, label="(d) - time = 28", color="black", parse=FALSE)
}

grfa <- grfA(data, detection, event)

grfb <- grfB(data, detection, event)

grfc <- grfC(data, detection, event)

grfd <- grfD(data, detection, event)

mypng(file="figures/chap7_evaluation.png", width = 1600, height = 720) #144 #720*1.75
gridExtra::grid.arrange(grfa, grfb, grfc, grfd, layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2))
dev.off()  



