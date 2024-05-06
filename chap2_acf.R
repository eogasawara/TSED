source("header.R")
library(forecast)
library(harbinger)

data(examples_harbinger)
data <- examples_harbinger$global_temperature_yearly
data$event <- FALSE
serie <- data$serie

serie_a <- examples_harbinger$nonstationarity$serie[1:200]
serie_b <- examples_harbinger$nonstationarity$serie[201:400]
serie_c <- examples_harbinger$nonstationarity$serie[401:600]
serie_d <- examples_harbinger$nonstationarity$serie[601:800]
serie_e <- examples_harbinger$nonstationarity$serie[801:1000]

grf <- ggAcf(serie_a)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("acf")
grf <- grf + xlab("lag")
grf <- grf + labs(caption = "(a) - stationary") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfaa<- grf

grf <- ggAcf(serie_b)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("acf")
grf <- grf + xlab("lag")
grf <- grf + labs(caption = "(b) - trend stationary") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfab <- grf

grf <- ggAcf(serie_c)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("acf")
grf <- grf + xlab("lag")
grf <- grf + labs(caption = "(c) - level stationary") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfac <- grf


grf <- ggAcf(serie_d)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("acf")
grf <- grf + xlab("lag")
grf <- grf + labs(caption = "(d) - heteroscedastic") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfad <- grf

grf <- ggAcf(serie_e)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("acf")
grf <- grf + xlab("lag")
grf <- grf + labs(caption = "(e) - random walk") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfae <- grf

grf <- ggAcf(serie)
grf <- grf + theme_bw(base_size = 10)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("acf")
grf <- grf + xlab("lag")
grf <- grf + labs(caption = "(f) - YGT") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfaf <- grf

mypng(file="figures/chap2_acf.png", width = 1600, height = 1260) #144 #720*1.75
gridExtra::grid.arrange(grfaa, grfab, grfac, grfad, grfae, grfaf,
                        layout_matrix = matrix(c(1,1,2,2,3,3,4,4,5,5,6,6), byrow = TRUE, ncol = 4))
dev.off()  

