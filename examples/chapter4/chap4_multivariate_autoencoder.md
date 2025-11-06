---
title: "Chapter 4: Multivariate Autoencoder"
output: html_document
---

``` r
library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
## Theoretical Overview
An autoencoder maps multivariate inputs into a compact latent representation by learning to reconstruct the input. Change points can be detected on the latent embedding (or reconstruction error) where regime shifts become more apparent.
## Example Overview and Goals
We demonstrate a complete, reproducible workflow: setting up libraries, loading data, configuring a detector, fitting it, running detection, optionally evaluating, and visualizing the results.
### Knitr Options
Keep output clean and reproducible.

### Setup and Libraries
Load helpers and required packages.

``` r
library(ggplot2)
library(RColorBrewer)
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
### Data Generation
Create a simple 2D synthetic dataset and mark a drift region based on quadrant consistency relative to the means.

``` r
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
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
# Scatter of the two variables with light quadrant shading
grf <- ggplot(data, aes(x = serie1, y = serie2)) +
  geom_rect(xmin = m1, xmax = +Inf, ymin = m2, ymax = +Inf, fill = "lightgray", alpha = 0.2) +
  geom_rect(xmin = -Inf, xmax = m1, ymin = -Inf, ymax = m2, fill = "lightgray", alpha = 0.2) +
  geom_point(size = 2) +
  theme_classic()
plot(grf)
```

![plot of chunk plot-scatter](fig/chap4_multivariate_autoencoder/plot-scatter-1.png)
### Train/Test Framing
Reorder data so the non-drift portion comes first, add an index, and build a single-event ground truth.

``` r
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
```
### Event Detection (Series 1)
Run the detector on `serie1` and inspect detected indices.

``` r
detection <- detect(model, data$serie1)
print(detection$idx[detection$event])
```

```
## [1] 115
```
### Plot Series 1

``` r
grfA <- ggplot(data, aes(x = i, y = serie1)) +
  geom_line() +
  theme_classic()
```
### Event Detection (Series 2)
Fit and run Chow test on `serie2` and inspect indices.

``` r
model <- fit(hcp_chow(), data$serie2)
detection <- detect(model, data$serie2)
print(detection$idx[detection$event])
```

```
## [1] 310
```
### Plot Series 2

``` r
grfB <- ggplot(data, aes(x = i, y = serie2)) +
  geom_line() +
  theme_classic()
```
### Autoencoder Projection
Train a 2->1 autoencoder and obtain a 1D latent projection.

``` r
auto <- autoenc_e(2, 1)                       # 2 inputs -> 1 latent
auto <- fit(auto, data[, 1:2])                # train on both variables
autoencoder <- as.vector(transform(auto, data[, 1:2]))
```
### Event Detection (Autoencoder)
Run Chow test on the latent and list detections.

``` r
model <- fit(hcp_chow(), autoencoder)
detection <- detect(model, autoencoder)
print(detection$idx[detection$event])
```

```
## [1] 310
```
### Plot Autoencoder

``` r
grfAE <- ggplot(data, aes(x = i, y = autoencoder)) +
  geom_line() +
  geom_vline(xintercept = drift, linetype = "dotted", color = "black", linewidth = 1) +
  theme_classic()
```
### Save Figure

``` r
#mypng(file = "figures/chap4_multivariate_autoencoder.png", width = 1280, height = 1080)
gridExtra::grid.arrange(grfA, grfB, grfAE,
                        layout_matrix = matrix(c(1, 2, 3), byrow = TRUE, ncol = 1))
```

![plot of chunk save-figure](fig/chap4_multivariate_autoencoder/save-figure-1.png)

``` r
#dev.off()
```
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.
