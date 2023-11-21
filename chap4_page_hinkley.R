source("header.R")
library(daltoolbox)
library(harbinger)

set.seed(1)
n <- 100  # Number of time points
data <- c(sin((1:n)/pi), 2*sin((1:n)/pi), 10 + sin((1:n)/pi), 10-10/n*(1:n)+sin((1:n)/pi)/2, sin((1:n)/pi)/2)
event <- rep(FALSE, n)

#there is a bug when the data frame has only one serie. This should be corrected soon.
data <- data.frame(serie1 = data, serie2 = data)

model <- fit(hcd_page_hinkley(threshold=3), data)
detection <- detect(model, data)
print(detection[(detection$event),])

grf <- har_plot(model, data$serie1, detection)
grf <- grf + ylab("value")
grf <- grf + font


save_png(grf, "figures/chap4_page_hinkley.png", 1280, 720)
