source("header.R")
library(harbinger)
options(scipen=999)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE

y <- data$serie
yts <- ts(y, start = c(1850, 1))
xts <- time(yts)

compute_cut_index <- function(freqs) {
  cutindex <- which.max(freqs)
  if (min(freqs) != max(freqs)) {
    threshold <- mean(freqs) + 2.698 * sd(freqs)
    freqs[freqs < threshold] <- min(freqs) + max(freqs)
    cutindex <- which.min(freqs)
  }
  return(cutindex)
}

fft_signal <- stats::fft(yts)

spectrum <- base::Mod(fft_signal) ^ 2
half_spectrum <- spectrum[1:(length(yts) / 2 + 1)]

cutindex <- compute_cut_index(half_spectrum)
print(cutindex)
n <- length(fft_signal)

fft_signal[1:cutindex] <- 0
fft_signal[(n - cutindex):n] <- 0


residual <- - base::Re(stats::fft(fft_signal, inverse = TRUE) / n)

yhat <- yts - residual


grf <- autoplot(ts(yts, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + geom_point(aes(y=yts),size = 0.5, col="black") 
grf <- grf + geom_line(aes(y=yhat), linetype = "dashed", col="darkblue") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfc <- grf
plot(grf)

grf <- autoplot(ts(residual, start = c(1850, 1)))
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("residual")
grf <- grf + xlab("time")
grf <- grf + geom_point(size = 0.5, col="black") 
grf <- grf + labs(caption = sprintf("(d)")) 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfd <- grf
plot(grfd)

mypng(file = "figures/chap2_fft_harbinger.png", width = 1280, height=1080)
gridExtra::grid.arrange(grfc, grfd, 
                        layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 1))
dev.off() 

