source("header.R")
options(scipen=999)
library(ggpmisc)
library(daltoolbox)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly

ats <- function(y) {
  yts <- ts(y, frequency=1, start = c(2000, 1))
  return(yts) 
}

data <- data[(nrow(data)-23+1):nrow(data),]

colorsg <- brewer.pal(9, 'Greys')
test_size <- 4

y <- ats(data$serie)
x <- time(y)
obj <- smoothing_freq(n = 9) 
obj <- daltoolbox::fit(obj, y)
yl <- daltoolbox::transform(obj, y)
yl <- as.integer(names(yl))
n <- length(y)

d4 <- data.frame(
  y = rep(14.6, n),
  x = x,
  ym = ats(rep(15.15, n)),
  yb = ats(rep(15.025, n)),
  yi = ats(rep(14.9, n)),
  colors = colorsg[yl],
  bcolors = rep("black", length(y))
)

d4$bcolorsreal <- d4$bcolors
d4$bcolorsreal[(n-test_size+1):n] <- "darkgreen"
d4$bcolorspred <- d4$bcolors
d4$bcolorspred[(n-test_size+1):n] <- "darkred"

d4$y1 <- c(rep(NA, 2), d4$yi[3:n])
d4$y2 <- c(rep(NA, 2), d4$yi[2:(n-1)])
d4$y3 <- c(rep(NA, 2), d4$yi[1:(n-2)])
d4$colors1 <- c(rep(NA, 2), d4$colors[3:n])
d4$colors2 <- c(rep(NA, 2), d4$colors[2:(n-1)])
d4$colors3 <- c(rep(NA, 2), d4$colors[1:(n-2)])

show_test <- function(x, i, test_size) {
  n <- length(x)
  if (test_size > i)
    x[(n-test_size+i+1):n] <- NA
  return(x)
}

grf <- autoplot(ats(d4$ym), color="white") + theme_bw(base_size = 10) + geom_point(color="white", size=0)
grf <- grf + theme(plot.title = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("")
grf <- grf + xlab("year")
grf <- grf + geom_vline(xintercept = x[1], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[n-test_size], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_vline(xintercept = x[n], col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_point(aes(y=d4$ym),size = 0.5, col="darkgray") 
grf <- grf + annotate(geom="text", x=x[10], y=15.1, label="training", color="black")
grf <- grf + annotate(geom="text", x=x[21], y=15.1, label="test", color="black")
grf <- grf + geom_tile(aes(y=d4$yb), height=0.05, width=1, size = 1, fill = d4$colors, color=d4$bcolorsreal, alpha=0.5) 

grf <- grf + annotate(geom="text", x=x[10], y=14.95, label="one step-ahead prediction", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y1[4], label="t", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y2[4]-0.05, label="t-1", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y3[4]-0.1, label="t-2", color="black")
grf <- grf + geom_tile(aes(y=show_test(d4$y1, 1, test_size)), height=0.05, width=1, size = 1, fill = d4$colors1, color=d4$bcolorspred, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y2-0.05, 0, test_size)), height=0.05, width=1, size = 1, fill = d4$colors2, color=d4$bcolorspred, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y3-0.1, 0, test_size)), height=0.05, width=1, size = 1, fill = d4$colors3, color=d4$bcolorspred, alpha=0.5) 

grf <- grf + annotate(geom="text", x=x[10], y=14.75, label="one step-ahead prediction with time series cross-validation", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y1[4]-0.30, label="t-2", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y2[4]-0.25, label="t-1", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y3[4]-0.20, label="t", color="black")
grf <- grf + geom_tile(aes(y=show_test(d4$y3, 1, test_size)-0.30), height=0.05, width=1, size = 1, fill = d4$colors3, color=d4$bcolors, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y2, 1, test_size)-0.25), height=0.05, width=1, size = 1, fill = d4$colors2, color=d4$bcolors, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y1, 2, test_size)-0.20), height=0.05, width=1, size = 1, fill = d4$colors1, color=d4$bcolorspred, alpha=0.5) 

grf <- grf + annotate(geom="text", x=2001, y=d4$y1[4]-0.50, label="t-2", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y2[4]-0.45, label="t-1", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y3[4]-0.40, label="t", color="black")
grf <- grf + geom_tile(aes(y=show_test(d4$y3, 2, test_size)-0.50), height=0.05, width=1, size = 1, fill = d4$colors3, color=d4$bcolors, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y2, 2, test_size)-0.45), height=0.05, width=1, size = 1, fill = d4$colors2, color=d4$bcolors, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y1, 3, test_size)-0.40), height=0.05, width=1, size = 1, fill = d4$colors1, color=d4$bcolorspred, alpha=0.5) 

grf <- grf + annotate(geom="text", x=2001, y=d4$y1[4]-0.70, label="t-2", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y2[4]-0.65, label="t-1", color="black")
grf <- grf + annotate(geom="text", x=2001, y=d4$y3[4]-0.60, label="t", color="black")
grf <- grf + geom_tile(aes(y=show_test(d4$y3, 3, test_size)-0.70), height=0.05, width=1, size = 1, fill = d4$colors3, color=d4$bcolors, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y2, 3, test_size)-0.65), height=0.05, width=1, size = 1, fill = d4$colors2, color=d4$bcolors, alpha=0.5) 
grf <- grf + geom_tile(aes(y=show_test(d4$y1, 4, test_size)-0.60), height=0.05, width=1, size = 1, fill = d4$colors1, color=d4$bcolorspred, alpha=0.5) 

grf <- grf  + font

save_png(grf, "figures/chap2_tt_sw.png", width=1280, height=720)


