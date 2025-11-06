---
title: "Chapter 2: Trend"
output: html_document
---

``` r
library(ggplot2)
library(RColorBrewer)
library(tseries)
library(lmtest)
source("https://raw.githubusercontent.com/eogasawara/TSED/refs/heads/main/code/header.R")
```
## Theoretical Overview
This example focuses on trend modeling and removal to aid analysis and detection.
## Example Overview and Goals
We will: set up libraries, load data, fit a linear trend and a moving-average smoother, and visualize original series with overlaid trends.
### What You Will Do
You will: prepare the environment, fit linear and moving-average trends, and compare overlays.
### Setup and Libraries
Short rationale for the libraries and any project-specific sources.

### Other Steps
Additional supporting steps that glue the workflow.
### Data Loading and Prep
Read the dataset and perform any minimal preparation required for modeling.

``` r
data(examples_harbinger)
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE
bp.test <- function(serie) {
  data <- data.frame(x = 1:length(serie), y = serie)
  fit <- lm(y ~ x, data = data)
  return(bptest(fit))
}
nonstationary.test <- function(serie) {
  return(data.frame(adf = round(adf.test(serie)$p.value, 2),
                    PP = round(PP.test(as.vector(serie))$p.value, 2),
                    bp = round(bp.test(serie)$p.value, 2)))
}
tsdata <- ts(data$serie, start = c(1850, 1))
model <- lm(tsdata ~ time(tsdata))
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
grf <- autoplot(tsdata, col="black")
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_point(aes(y=tsdata), size = 0.25, col="black") 
grf <- grf + geom_line(aes(y=ts(model$fitted.values, start = c(1850, 1))), linetype = "dashed", col="darkblue") 
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfa <- grf
#MAS
y_transformed <- TSPred::mas(tsdata, 10)
yhat <- TSPred::mas.rev(y_transformed,  attr(y_transformed, "xi"), 10)
nonstationary.test(y_transformed)
```

```
## Warning in adf.test(serie): p-value greater than printed p-value
```

```
##     adf   PP   bp
## BP 0.99 0.99 0.01
```

``` r
y_transformed <- ts(c(rep(NA, 9), y_transformed), start = c(1850, 1))
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
grf <- autoplot(tsdata, col="black")
```
### Other Steps
Additional supporting steps that glue the workflow.

``` r
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + geom_point(aes(y=tsdata), size = 0.25, col="black") 
grf <- grf + geom_line(aes(y=y_transformed), linetype = "dashed", col="darkblue") 
grf <- grf + ylab("temperature")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfb <- grf
```
### Visualization and Output
Plot the series with detected events and optionally save figures. Combine ggplot layers in one chunk for clarity.

``` r
#mypng(file = "figures/chap2_trend.png", width = 1600, height = 720) # 1280 * 1.5
gridExtra::grid.arrange(grfa, grfb, layout_matrix = matrix(c(1,2), byrow = TRUE, ncol = 2))
```

```
## Warning: Removed 9 rows containing missing values or values outside the scale range (`geom_line()`).
```

![plot of chunk viz_save](fig/chap2_trend/viz_save-1.png)

``` r
#dev.off()
```
## References
* General: Box, G. E. P. & Jenkins, G. (1970). Time Series Analysis.
* Ogasawara, E., Salles, R., Porto, F., Pacitti, E. Event Detection in Time Series. Springer, 2025. doi:10.1007/978-3-031-75941-3.
