library(tsmp)

source("header.R")
options(scipen=999)
library(ggpmisc)

load("data/noaa-global/temp_yearly.RData")

ts_data <- ts(temp_yearly$temperature, frequency=1, start = c(1850, 1))

mp <- tsmp(ts_data, window_size = 6, mode = "stamp", verbose = 0) 
#mp <- tsmp::stomp(ts_data, window_size = 4, verbose = 1)
#mp <- tsmp::stamp(ts_data, window_size = 4, verbose = 1)

mpm <- tsmp::find_motif(mp, n_motifs = 1)

model <- fit(harbinger(), ts_data)
detection <- detect(model, ts_data)
detection$seqlen <- NA

for (i in 1:length(mpm$motif$motif_idx)) {
  detection$event[mpm$motif$motif_idx[[i]]] <- TRUE
  detection$seqlen[mpm$motif$motif_idx[[i]]] <- 6
  detection$type[mpm$motif$motif_idx[[i]]] <- "motif"
}

idx <- as.Date(ts_data)
grf <- har_plot(model, ts_data, detection, idx=idx)
grf <- grf + font

save_png(grf, "figures/chap5_motifs_mp.png", 1280, 720)

