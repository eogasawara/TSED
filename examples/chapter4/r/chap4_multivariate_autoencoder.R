library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")

source_python('https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/seed.py')  # reproducible seeds for R/Python
set.seed(1)
seed_everything(1)
n <- 500                                 # number of time points
data <- data.frame(serie1 = rnorm(n),
                   serie2 = rnorm(n))
# Per-dimension means to define quadrants
m1 <- mean(data$serie1)
m2 <- mean(data$serie2)
# Mark points where both dims are on the same side of their means
data$drift <- ((data$serie1 > m1) & (data$serie2 > m2)) |
              ((data$serie1 < m1) & (data$serie2 < m2))

# Scatter of the two variables with light quadrant shading
grf <- ggplot(data, aes(x = serie1, y = serie2)) +
  geom_rect(xmin = m1, xmax = +Inf, ymin = m2, ymax = +Inf, fill = "lightgray", alpha = 0.2) +
  geom_rect(xmin = -Inf, xmax = m1, ymin = -Inf, ymax = m2, fill = "lightgray", alpha = 0.2) +
  geom_point(size = 2) +
  theme_classic()
plot(grf)

ts_before <- data[data$drift == FALSE, ]
ts_after  <- data[data$drift == TRUE, ]
data <- rbind(ts_before, ts_after)
data$i <- seq_len(nrow(data))
# Build one ground-truth event at the first drift index after reordering
data$event <- FALSE
data$event[min(which(data$drift))] <- TRUE
drift <- which(data$event)
# Fit Chow test on the first series
model <- fit(hcp_chow(), data$serie1)

detection <- detect(model, data$serie1)
print(detection$idx[detection$event])

grfA <- ggplot(data, aes(x = i, y = serie1)) +
  geom_line() +
  theme_classic()

model <- fit(hcp_chow(), data$serie2)
detection <- detect(model, data$serie2)
print(detection$idx[detection$event])

grfB <- ggplot(data, aes(x = i, y = serie2)) +
  geom_line() +
  theme_classic()

auto <- autoenc_e(2, 1)                       # 2 inputs -> 1 latent
auto <- fit(auto, data[, 1:2])                # train on both variables
autoencoder <- as.vector(transform(auto, data[, 1:2]))

model <- fit(hcp_chow(), autoencoder)
detection <- detect(model, autoencoder)
print(detection$idx[detection$event])

grfAE <- ggplot(data, aes(x = i, y = autoencoder)) +
  geom_line() +
  geom_vline(xintercept = drift, linetype = "dotted", color = "black", linewidth = 1) +
  theme_classic()

#mypng(file = "figures/chap4_multivariate_autoencoder.png", width = 1280, height = 1080)
gridExtra::grid.arrange(grfA, grfB, grfAE,
                        layout_matrix = matrix(c(1, 2, 3), byrow = TRUE, ncol = 1))
#dev.off()
