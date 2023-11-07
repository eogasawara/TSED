source("header.R")
options(scipen=999)

load("data/noaa-global/temp_yearly.RData")
data <- temp_yearly
data$event <- FALSE

y <- data$temperature
yts <- ts(y, start = c(1850, 1))
xts <- time(yts)

fft_harmonics <- function(x, n = NULL) {
  minx <- min(x)
  x <- x - minx
  
  dff = fft(x)
  if (is.null(n)) {
    n <- length(dff)/2 - 1    
  }
  
  t = seq(from = 1, to = length(x))
  ndff = array(data = 0, dim = c(length(t), 1L))
  ndff[1] = dff[1] #Always, it's the DC component
  if(n != 0){
    ndff[2:(n+1)] = dff[2:(n+1)] #The positive frequencies always come first
    #The negative ones are trickier
    ndff[length(ndff):(length(ndff) - n + 1)] = dff[length(x):(length(x) - n + 1)]
  }
  indff = fft(ndff/length(x), inverse = TRUE)
  
  return(Mod(indff) + minx)
}

# Determine the number of harmonics to include based on the significant frequency components
periodogram <- spec.pgram(y, plot=FALSE)
harmonics <- length(periodogram$freq)
yhat <- fft_harmonics(y, harmonics)
print(sum(abs(y-yhat)))

grf <- autoplot(ts(yhat, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + labs(caption = sprintf("(a): %d harmonics", harmonics)) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfa <- grf
plot(grfa)

df <- data.frame(x = periodogram$freq, y = periodogram$spec)
harmonics <- as.integer(sqrt(length(periodogram$freq)))
spec <- df$y[harmonics]

for (i in harmonics:nrow(df)) {
  spec <- df$y[i]
  significant_freq <- which(df$y > spec)
  if(i >= max(significant_freq)) {
    harmonics <- i
    break
  }
}

# periodogram 
grf <- ggplot(df, aes(x = x, y = y)) + geom_line() + geom_point(size=0.5) + scale_y_log10()
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("frequency")
grf <- grf + xlab("spectrum")  
grf <- grf + geom_hline(yintercept = spec, col="darkgrey", size = 0.5, linetype="dotted")
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf
plot(grfb)

yhat <- fft_harmonics(y, harmonics)
print(c(harmonics, sum(abs(y-yhat))))
tolerance <- 30

grf <- autoplot(ts(yts, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + geom_line(aes(y=yhat), linetype = "dashed", col="darkblue") 
grf <- grf + geom_vline(xintercept = xts[tolerance], col="darkgrey", size = 0.5, linetype="dotted")
grf <- grf + geom_vline(xintercept = xts[length(xts)-tolerance], col="darkgrey", size = 0.5, linetype="dotted")
grf <- grf + labs(caption = sprintf("(c): %d harmonics", harmonics)) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfc <- grf
plot(grf)

grf <- autoplot(ts(y - yhat, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + geom_vline(xintercept = xts[tolerance], col="darkgrey", size = 0.5, linetype="dotted")
grf <- grf + geom_vline(xintercept = xts[length(xts)-tolerance], col="darkgrey", size = 0.5, linetype="dotted")
grf <- grf + labs(caption = sprintf("(d)")) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfd <- grf
plot(grfd)

mypng(file = "figures/chap2_fft.png", width = 1280, height=1440)
gridExtra::grid.arrange(grfa, grfb, grfc, grfd, 
                        layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, ncol = 1))
dev.off() 

