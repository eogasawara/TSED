source("header.R")
library(daltoolbox)
library(harbinger)

# loading example
data(examples_changepoints)
data <- examples_changepoints$complex

model <- fit(harbinger(), data$serie)
detection <- detect(model, data$serie)

grf <- har_plot(model, data$serie, detection)
grf <- grf + scale_x_continuous(breaks = seq(0, length(data$serie), by = length(data$serie)/5), "example")
grf <- grf + annotate(geom="text", x=50, y=12.5, label="A", color="black")
grf <- grf + geom_vline(xintercept = 100, col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=111, y=11.8, label="cp[AB]", color="black", parse=TRUE)
grf <- grf + annotate(geom="text", x=150, y=12.5, label="B", color="black")
grf <- grf + geom_vline(xintercept = 200, col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=211, y=11.8, label="cp[BC]", color="black", parse=TRUE)
grf <- grf + annotate(geom="text", x=250, y=12.5, label="C", color="black")
grf <- grf + geom_vline(xintercept = 300, col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=311, y=11.8, label="cp[CD]", color="black", parse=TRUE)
grf <- grf + annotate(geom="text", x=350, y=12.5, label="D", color="black")
grf <- grf + geom_vline(xintercept = 400, col="darkgray", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=411, y=11.8, label="cp[DE]", color="black", parse=TRUE)
grf <- grf + annotate(geom="text", x=450, y=12.5, label="E", color="black")
grf <- grf + ylab("value")
grf <- grf + font

save_png(grf, "figures/chap4_example.png", 1280, 720)
