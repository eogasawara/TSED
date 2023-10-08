library(devtools) 
#devtools::install_github("cefet-rj-dal/daltoolbox", force=TRUE)
#devtools::install_github("cefet-rj-dal/harbinger", force=TRUE)

library(forecast)
library(ggplot2)
library(dplyr)
library(reshape)
library(RColorBrewer)
library(readr)
library(tseries)
library(TSPred)
library(daltoolbox)
library(harbinger)
library(gridExtra)
library(ragg)

data(har_examples)

mypng <- function(file, width, height) {
  return(agg_png(file, width = width, height = height, res = 144)) 
}

mypdf <- function(file, width, height) {
  return(pdf(file, width = width/100, height = height/100))
}

save_png <- function(grf, filename, width, height) {
  mypng(filename, width = width, height = height) 
  plot(grf)
  dev.off() 
}

save_pdf <- function(grf, filename, width, height) {
  mypdf(filename, width = width, height = height)
  plot(grf)
  dev.off() 
}



bp.test <- function(serie) {
  library(lmtest)
  data <- data.frame(x = 1:length(serie), y = serie)
  fit <- lm(y ~ x, data = data)
  return(bptest(fit))
}

nonstationary.test <- function(serie) {
  return(data.frame(adf = round(adf.test(serie)$p.value, 2),
    PP = round(PP.test(as.vector(serie))$p.value, 2),
    bp = round(bp.test(serie)$p.value, 2)))
}

colors <- brewer.pal(9, 'Set1')[c(1:5,7:9)]
font <- theme(text = element_text(size=16))
#fancy_font <- 'Lucida Console'
fancy_font <- 'Helvetica'

fontstyle <- theme(text = element_text(family = fancy_font))

prepare_grf <- function(grf, xlabel) {
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(plot.title = element_blank())
  grf <- grf + theme(panel.grid.major = element_blank())
  grf <- grf + theme(panel.grid.minor = element_blank())
  grf <- grf + ylab(xlabel)
  grf <- grf + xlab("time")  
  grf <- grf + fontstyle
  grf <- grf + font
  return(grf)
}

motifs_seqs <- function(detection) {
  iMotif <- NULL
  n <- nrow(detection)
  hasMotif <- !is.null(detection$seq)
  if (hasMotif) {
    iMotif <- rep(FALSE, n)
    posMotif <- !is.na(detection$seq)
    dMotif <- data.frame(start = (1:n)[posMotif], length = detection$seqlen[posMotif])
    repeat {
      if (nrow(dMotif) == 0)
        break
      iMotif[dMotif$start] <- TRUE
      dMotif$start <- dMotif$start + 1
      dMotif$length <- dMotif$length - 1
      dMotif <- dMotif[dMotif$length > 0,]
    }
  }
  return(iMotif)
}

har_plot <- function (obj, serie, detection, event = NULL, mark.cp = TRUE, ylim = NULL, idx = NULL, pointsize=0.5) 
{
  time <- 0
  if (is.null(idx)) 
    idx <- 1:length(serie)
  detection$event[is.na(detection$event)] <- FALSE
  data <- data.frame(time = idx, serie = serie, FP = detection$event, 
                     TP = FALSE, FN = FALSE, color = "black", size=pointsize)
  data$CP <- detection$type == "changepoint"
  if (!is.null(event)) {
    data$TP <- detection$event & (event == detection$event)
    data$FP <- detection$event & (event != detection$event)
    data$FN <- event & (event != detection$event)
  }
  motifs_seqs <- motifs_seqs(detection)
  if (!is.null(motifs_seqs)) {
    data$size[motifs_seqs] <- 1.5
    data$color[motifs_seqs] <- "purple"
  }
  data$color[data$FN] <- "blue"
  data$color[data$TP] <- "green"
  data$color[data$FP] <- "red"
  data$size[data$FN | data$TP | data$FP] <- 1.5
  
  min_data <- min(serie)
  max_data <- max(serie)
  if (!is.null(ylim)) {
    min_data <- ifelse(!is.na(ylim[1]), ylim[1], min(serie))
    max_data <- ifelse(!is.na(ylim[2]), ylim[2], max(serie))
  }
  top_1 <- max_data + (max_data - min_data) * 0.02
  top_2 <- max_data + (max_data - min_data) * 0.05
  bottom_1 <- min_data - (max_data - min_data) * 0.02
  bottom_2 <- min_data - (max_data - min_data) * 0.05
  plot <- ggplot(data, aes(x = time, y = serie)) + geom_point(colour = data$color, size=data$size) + 
    geom_line() + xlab("") + ylab("") + theme_bw()
  plot <- plot + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  
  if (!is.null(ylim)) 
    plot <- plot + ggplot2::ylim(bottom_2, top_2)
  if (mark.cp && (sum(data$CP, na.rm = TRUE) > 0)) {
    plot <- plot + geom_segment(aes(x = time, y = top_1, xend = time, yend = bottom_1), 
                                data = data[data$CP, ], col = "grey", size = 1, linetype = "dashed")
  }
  return(plot)
}

