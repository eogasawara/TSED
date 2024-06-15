source("header.R")

set.seed(1)

data_a <- function(offset=0) {
  data <- NULL
  data <- c(data, rep(0, 29)) 
  return(data)
}

grfa <- function(data) {
  model <- fit(harbinger(), data)
  detection <- detect(model, data)
  event <- rep(FALSE, length(data))

  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(-0.1, 1.0)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=16, y=0.95, label="mu[e[j]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=15, y=-0.05, label="t[e[j]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=11, y=-0.05, label="t[e[j]]-k", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=19, y=-0.05, label="t[e[j]]+k", color="black", parse=TRUE)

  grf <- grf + geom_segment(aes(x = 11, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_segment(aes(x = 15, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="dashed")
  grf <- grf + geom_segment(aes(x = 19, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  
  grf <- grf + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  grf <- grf + annotate(geom="text", x=1, y=0.95, label="(a)", color="black", parse=TRUE)
  
  return(grf)
}

grfb <- function(data) {
  model <- fit(harbinger(), data)
  detection <- detect(model, data)
  event <- rep(FALSE, length(data))

  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(-0.1, 1.0)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=15, y=-0.05, label="t[e[j]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=11, y=-0.05, label="t[e[j]]-k", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=19, y=-0.05, label="t[e[j]]+k", color="black", parse=TRUE)
  grf <- grf + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  grf <- grf + geom_segment(aes(x = 16, y = 0, xend = 16, yend = 0.70), col="red", linewidth = 0.125, linetype="dotdash")
  grf <- grf + annotate(geom="text", x=16, y=0.95, label="mu[e[j]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=22, y=0.05, label="mu[e[j]](t[d[2]])", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=17.25, y=0.75, label="mu[e[j]](t[d[1]])", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=16, y=-0.05, label="t[d[1]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=22, y=-0.05, label="t[d[2]]", color="black", parse=TRUE)
  grf <- grf + geom_segment(aes(x = 11, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_segment(aes(x = 19, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_point(aes(16, 0), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(22, 0), colour = "red", size = 1)  
  
  grf <- grf + annotate(geom="text", x=1, y=0.95, label="(b)", color="black", parse=TRUE)
  
  return(grf)
}

grfc <- function(data) {
  model <- fit(harbinger(), data)
  detection <- detect(model, data)
  event <- rep(FALSE, length(data))
  
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(-0.1, 1.0)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=15, y=-0.05, label="t[e[1]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=15+4, y=-0.05, label="t[e[2]]", color="black", parse=TRUE)
  grf <- grf + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  grf <- grf + geom_segment(aes(x = 16, y = 0.3, xend = 16, yend = 0.70), col="red", linewidth = 0.125, linetype="dotdash")
  grf <- grf + geom_segment(aes(x = 16, y = 0, xend = 16, yend = 0.20), col="red", linewidth = 0.125, linetype="dotdash")
  
  grf <- grf + geom_segment(aes(x = 11, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_segment(aes(x = 19, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_segment(aes(x = 11+4, y = 0, xend = 15+4, yend = 1), col="blue", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_segment(aes(x = 19+4, y = 0, xend = 15+4, yend = 1), col="blue", linewidth = 0.125, linetype="solid")
  
  grf <- grf + annotate(geom="text", x=16, y=-0.05, label="t[d[1]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=15.5, y=1, label="mu[e[1]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=15.25, y=0.75, label="mu[e[1]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=17, y=0.75, label="(t[d[1]])", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=14.5, y=0.25, label="mu[e[2]](t[d[1]])", color="black", parse=TRUE)
  
  grf <- grf + annotate(geom="text", x=15.5+4, y=1, label="mu[e[2]]", color="blue", parse=TRUE)
  
  grf <- grf + geom_point(aes(16, 0.75), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(16, 0.25), colour = "red", size = 1)  
  
  grf <- grf + annotate(geom="text", x=1, y=0.95, label="(c)", color="black", parse=TRUE)

  return(grf)
}


grfd <- function(data) {
  model <- fit(harbinger(), data)
  detection <- detect(model, data)
  event <- rep(FALSE, length(data))
  
  grf <- har_plot(model, data, detection, event)
  grf <- grf + ylab(" ") + ylim(-0.1, 1.0)
  grf <- grf + font 
  grf <- grf + annotate(geom="text", x=15, y=-0.05, label="t[e[1]]", color="black", parse=TRUE)
  grf <- grf + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  grf <- grf + geom_segment(aes(x = 16, y = 0, xend = 16, yend = 0.70), col="red", linewidth = 0.125, linetype="dotdash")
  grf <- grf + geom_segment(aes(x = 17, y = 0, xend = 17, yend = 0.45), col="red", linewidth = 0.125, linetype="dotdash")
  grf <- grf + geom_segment(aes(x = 18, y = 0, xend = 18, yend = 0.20), col="red", linewidth = 0.125, linetype="dotdash")

  grf <- grf + geom_segment(aes(x = 11, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")
  grf <- grf + geom_segment(aes(x = 19, y = 0, xend = 15, yend = 1), col="black", linewidth = 0.125, linetype="solid")

  grf <- grf + annotate(geom="text", x=16, y=-0.05, label="t[d[1]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=17, y=-0.05, label="t[d[2]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=18, y=-0.05, label="t[d[3]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=22, y=-0.05, label="t[d[4]]", color="black", parse=TRUE)
  grf <- grf + annotate(geom="text", x=15.5, y=1, label="mu[e[1]]", color="black", parse=TRUE)

  grf <- grf + geom_point(aes(16, 0.75), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(17, 0.50), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(18, 0.25), colour = "red", size = 1)  
  grf <- grf + geom_point(aes(22, 0), colour = "red", size = 1)  
  
  grf <- grf + annotate(geom="text", x=1, y=0.95, label="(d)", color="black", parse=TRUE)

  return(grf)
}



grfA <- grfa(data_a())
plot(grfA)

grfB <- grfb(data_a())
plot(grfB)

grfC <- grfc(data_a())
plot(grfC)

grfD <- grfd(data_a())
plot(grfD)


mypng(file="figures/chap6_softed.png", width = 1600, height = 720) #144 #720*1.75
gridExtra::grid.arrange(grfA, grfB, grfC, grfD, layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 2))
dev.off()  

#save_png(grfA, "figures/chap6_softed.png", 1280, 720)



