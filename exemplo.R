source("header.R")
library(daltoolbox)
library(harbinger)
library(heimdall)


data(st_drift_examples)

data <- st_drift_examples$dataset1
data$event <- FALSE
data$event[min(which(data$drift))] <- TRUE


ggplot(data, aes(x=serie1, y=serie2)) +
  geom_rect(xmin = 0, xmax = +Inf,   ymin = 0, ymax = +Inf,   fill = "red") +
  geom_rect(xmin = -Inf, xmax = 0,   ymin = -Inf, ymax = 0,   fill = "red") +
  geom_point(size=2)+
  theme_classic()

drift <- which(data$event)

ggplot(data, aes(x=i, y=serie1)) +
  geom_line() +
  geom_vline(xintercept = drift, linetype="dotted", color = "red", size=1.5) +
  theme_classic()

ggplot(data, aes(x=i, y=serie2)) +
  geom_line() +
  geom_vline(xintercept = drift, linetype="dotted", color = "red", size=1.5) +
  theme_classic()

auto <- autoenc_encode(2, 1)
auto <- fit(auto, data[,1:2])
result <- transform(auto, data[,1:2])

ggplot(data, aes(x=i, y=result)) +
  geom_line() +
  geom_vline(xintercept = drift, linetype="dotted", color = "red", size=1.5) +
  theme_classic()

