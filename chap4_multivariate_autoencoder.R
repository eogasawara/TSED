source("header.R")
library(daltoolbox)
library(harbinger)
library(heimdall)
library(reticulate)

source_python('seed.py')
set.seed(1)
seed_everything(1)

n <- 500  # Number of time points
example_type='multivariate'
# Multivariate Example
data <- data.frame(serie1=rnorm(n), serie2=rnorm(n))

m1 <- mean(data$serie1)
m2 <- mean(data$serie2)

data$drift <- ((data$serie1 > m1) & (data$serie2 > m2)) | ( (data$serie1 < m1) & (data$serie2 < m2))

grf <- ggplot(data, aes(x=serie1, y=serie2)) +
  geom_rect(xmin = m1, xmax = +Inf,   ymin = m2, ymax = +Inf,   fill = "lightgray", alpha=0.2) +
  geom_rect(xmin = -Inf, xmax = m1,   ymin = -Inf, ymax = m2,   fill = "lightgray", alpha=0.2) +
  geom_point(size=2)+
  theme_classic()
plot(grf)

tsantes <- data[data$drift==FALSE,]
posdrift <- nrow(tsantes) + 1
tsdepois <- data[data$drift==TRUE,]

data <- rbind(tsantes, tsdepois)
data$i <- 1:nrow(data)

data$event <- FALSE
data$event[min(which(data$drift))] <- TRUE

drift <- which(data$event)

model <- fit(hcp_chow(), data$serie1)
detection <- detect(model, data$serie1)
print(detection$idx[detection$event])

grfA <- ggplot(data, aes(x=i, y=serie1)) +
  geom_line() +
  theme_classic()


model <- fit(hcp_chow(), data$serie2)
detection <- detect(model, data$serie2)
print(detection$idx[detection$event])

grfB <- ggplot(data, aes(x=i, y=serie2)) +
  geom_line() +
  theme_classic()

auto <- autoenc_encode(2, 1)
auto <- fit(auto, data[,1:2])
autoencoder <- as.vector(transform(auto, data[,1:2]))

model <- fit(hcp_chow(), autoencoder)
detection <- detect(model, autoencoder)
print(detection$idx[detection$event])

grfAE <-ggplot(data, aes(x=i, y=autoencoder)) +
  geom_line() +
  geom_vline(xintercept = drift, linetype="dotted", color = "black", size=1) +
  #geom_vline(xintercept = detection$idx[detection$event], linetype="dotted", color = "red", size=1) +  
  theme_classic()


mypng(file="figures/chap4_multivariate_autoencoder.png", width=1280, height=1080) 
gridExtra::grid.arrange(grfA, grfB, grfAE,
                        layout_matrix = matrix(c(1,2,3), byrow = TRUE, ncol = 1))
dev.off() 

