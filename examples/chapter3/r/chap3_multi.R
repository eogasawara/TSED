library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")



set.seed(123)
n <- 100; m <- 2
data <- as.data.frame(matrix(rnorm(n * m), nrow = n, ncol = m))
event <- rep(FALSE, n)
model <- fit(hmu_pca(), data)
pca_detection <- detect(model, data)
serie <- attr(pca_detection, "res")

model <- fit(hanr_arima(), data[, 1])

detection1 <- detect(model, data[, 1])

model <- fit(hanr_arima(), data[, 2])

detection2 <- detect(model, data[, 2])

detection <- detection1
detection$event <- (detection1$event | detection2$event)

grf <- har_plot(model, data[, 1], detection1, pca_detection$event)

grf <- grf + scale_x_continuous(breaks = seq(10, 100, by = 10), "(a) N(0,1)")
grf <- grf + ylab("v1") + font
grfA <- grf

grf <- har_plot(model, data[, 2], detection2, pca_detection$event)

grf <- grf + scale_x_continuous(breaks = seq(10, 100, by = 10), "(b) N(0,1)")
grf <- grf + ylab("v2") + font
grfB <- grf

grf <- har_plot(model, serie, pca_detection, detection$event)

grf <- grf + scale_x_continuous(breaks = seq(10, 100, by = 10), "(c) PCA")
grf <- grf + ylab("Residual") + font
grfC <- grf

#mypng(file = "figures/chap3_multi.png", width = 1600, height = 1080)
gridExtra::grid.arrange(grfA, grfB, grid::nullGrob(), grfC, grid::nullGrob(),
                        layout_matrix = matrix(c(1,1,2,2,3,4,4,5), byrow = TRUE, ncol = 4))
#dev.off()
