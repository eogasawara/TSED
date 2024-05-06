source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly

data <- data[(nrow(data)-23+1):nrow(data),]

colorsg <- brewer.pal(9, 'Greys')
test_size <- 4

ats <- function(y) {
  yts <- ts(y, frequency=1, start = c(2000, 1))
  return(yts) 
}

y <- ats(data$serie)
x <- time(y)
obj <- smoothing_freq(n = 9) 
obj <- daltoolbox::fit(obj, y)
yl <- daltoolbox::transform(obj, y)
yl <- as.integer(names(yl))

d1 <- data.frame(
  y = y,
  x = x,
  ym = ats(rep(15.05, length(x))),
  yb = ats(rep(14.10, length(x))),
  colors = colorsg[yl],
  bcolors = rep("black", length(y))
)
d1$bcolors[(length(x)-test_size+1):length(x)] <- "darkgreen"

grf <- autoplot(d1$y) + theme_bw(base_size = 10) + geom_point(size=1)
grf <- grf + theme(plot.title = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("year")
grf <- grf + geom_vline(xintercept = x[1], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[length(x)-test_size], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[length(x)], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_point(aes(y=d1$ym),size = 0.5, col="darkgray") 
grf <- grf + geom_tile(aes(y=d1$yb), height=0.05, width=1, size = 1, fill = d1$colors, color=d1$bcolors, alpha=0.5) 
grf <- grf + annotate(geom="text", x=x[10], y=15.15, label="training", color="black")
grf <- grf + annotate(geom="text", x=x[21], y=15.15, label="test", color="black")
grf <- grf + annotate(geom="text", x=x[10], y=14.2, label="colored representation for the time series", color="black")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfA <- grf
#plot(grfA)

#####

d2 <- data.frame(
  y = rep(14.6, length(x)),
  x = x,
  ym = ats(rep(15.0, length(x))),
  yb = ats(rep(15, length(x))),
  yi = ats(rep(14.8, length(x))),
  colors = colorsg[yl],
  bcolors = rep("black", length(y))
)

d2$bcolorstt <- d2$bcolors
d2$bi <-d2$bcolors
d2$yi[(length(x)-test_size+2):length(x)] <- NA
d2$bi[(length(x)-test_size+1)] <- "darkred"
d2$bcolorstt[(length(x)-test_size+1):length(x)] <- "darkgreen"

for (i in 1:test_size) {
  d2$tyi <- d2$y - 0.1*(i-1)
  if (i != test_size)
    d2$tyi[(length(x)-test_size+1+i):length(x)] <- NA
  d2$tbi <- d2$bcolors
  d2$tbi[(length(x)-test_size+i)] <- "darkred"
  colnames(d2)[which(colnames(d2) == "tyi")] <- sprintf("y%d", i)
  colnames(d2)[which(colnames(d2) == "tbi")] <- sprintf("b%d", i)
}

grf <- autoplot(ats(d2$y4), color="white") + theme_bw(base_size = 10) + geom_point(color="white", size=0)
grf <- grf + theme(plot.title = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("year")
grf <- grf + geom_vline(xintercept = x[1], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[length(x)-test_size], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[length(x)], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_point(aes(y=d2$ym),size = 0.5, col="darkgray") 
grf <- grf + annotate(geom="text", x=x[10], y=15.1, label="training", color="black")
grf <- grf + annotate(geom="text", x=x[21], y=15.1, label="test", color="black")
grf <- grf + annotate(geom="text", x=x[10], y=14.9, label="one step-ahead prediction", color="black")
grf <- grf + geom_tile(aes(y=d2$yi), height=0.05, width=1, size = 1, fill = d2$colors, color=d2$bi, alpha=0.5) 
grf <- grf + annotate(geom="text", x=x[10], y=14.7, label="one step-ahead prediction with time series cross-validation", color="black")
grf <- grf + geom_tile(aes(y=d2$y1), height=0.05, width=1, size = 1, fill = d2$colors, color=d2$b1, alpha=0.5) 
grf <- grf + geom_tile(aes(y=d2$y2), height=0.05, width=1, size = 1, fill = d2$colors, color=d2$b2, alpha=0.5) 
grf <- grf + geom_tile(aes(y=d2$y3), height=0.05, width=1, size = 1, fill = d2$colors, color=d2$b3, alpha=0.5) 
grf <- grf + geom_tile(aes(y=d2$y4), height=0.05, width=1, size = 1, fill = d2$colors, color=d2$b4, alpha=0.5) 
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfB <- grf
#plot(grfB)

######

sacv <- 3

d3 <- data.frame(
  y = rep(14.6, length(x)),
  x = x,
  ym = ats(rep(14.95, length(x))),
  yi = ats(rep(14.8, length(x))),
  colors = colorsg[yl],
  bcolors = rep("black", length(y))
)

d3$bcolorstt <- d3$bcolors
if (1 < (test_size-sacv+1))
  d3$yi[(length(x)-test_size+1+sacv):length(x)] <- NA
d3$bi <- d3$bcolors
d3$bi[(length(x)-test_size+1):(length(x)-test_size+sacv)] <- "darkred"
d3$bcolorstt[(length(x)-test_size+1):length(x)] <- "darkgreen"

for (i in 1:(test_size-sacv+1)) {
  d3$tyi <- d3$y - 0.1*(i-1)
  if (i < (test_size-sacv+1))
    d3$tyi[(length(x)-test_size+i+sacv):length(x)] <- NA
  d3$tbi <- d3$bcolors
  d3$tbi[(length(x)-test_size+i):(length(x)-test_size+i+sacv-1)] <- "darkred"
  colnames(d3)[which(colnames(d3) == "tyi")] <- sprintf("y%d", i)
  colnames(d3)[which(colnames(d3) == "tbi")] <- sprintf("b%d", i)
}

grf <- autoplot(ats(d3$y2), color="white") + theme_bw(base_size = 10) + geom_point(color="white", size=0)
grf <- grf + theme(plot.title = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("year")
grf <- grf + geom_vline(xintercept = x[1], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[length(x)-test_size], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[length(x)], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=x[10], y=15.025, label="training", color="black")
grf <- grf + annotate(geom="text", x=x[21], y=15.025, label="test", color="black")
grf <- grf + geom_point(aes(y=d3$ym), size = 0.5, col="darkgray") 
grf <- grf + annotate(geom="text", x=x[10], y=14.875, label="three step-ahead prediction", color="black")
grf <- grf + geom_tile(aes(y=d3$yi), height=0.05, width=1, size = 1, fill = d3$colors, color=d3$bi, alpha=0.5) 
grf <- grf + annotate(geom="text", x=x[10], y=14.675, label="three step-ahead prediction with time series cross-validation", color="black")
grf <- grf + geom_tile(aes(y=d3$y1), height=0.05, width=1, size = 1, fill = d3$colors, color=d3$b1, alpha=0.5) 
grf <- grf + geom_tile(aes(y=d3$y2), height=0.05, width=1, size = 1, fill = d3$colors, color=d3$b2, alpha=0.5) 
grf <- grf + labs(caption = "(c)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfC <- grf
#plot(grfC)


mypng(file="figures/chap2_tt_sa.png", width = 1280, height = 1260) 
gridExtra::grid.arrange(grfA, grfB, grfC,
                        layout_matrix = matrix(c(1,1,1,1,1,2,2,2,2,3,3,3), byrow = TRUE, ncol = 1))
dev.off() 
