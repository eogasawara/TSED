source("header.R")
library(harbinger)
library(tseries)
data(har_examples)

bp.test <- function(serie) {
  library(lmtest)
  data <- data.frame(x = 1:length(serie), y = serie)
  fit <- lm(y ~ x, data = data)
  return(bptest(fit))
}

load("data/noaa-global/temp_monthly.RData")

serie_a <- har_examples[[9]]$serie[1:200]
serie_b <- har_examples[[9]]$serie[201:400]
serie_c <- har_examples[[9]]$serie[401:600]
serie_d <- har_examples[[9]]$serie[601:800]
serie_e <- har_examples[[9]]$serie[801:1000]

x <- 1:200
ts_data <- ts(serie_a)
grf <- autoplot(ts_data)
grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("value")
grf <- grf + xlab("time")
grf <- grf + geom_hline(yintercept = 0, col="black", size = 0.5)
grf <- grf + geom_hline(yintercept = +var(ts_data), col="black", linetype = 'dashed', size = 0.5)
grf <- grf + geom_hline(yintercept = -var(ts_data), col="black", linetype = 'dashed', size = 0.5)
grf <- grf + labs(caption = "(a) stationary") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfsa <- grf


ts_data <- ts(serie_b)
model <- lm(ts_data ~ x)
grf <- autoplot(ts_data)
grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("value")
grf <- grf + xlab("time")
grf <- grf + geom_line(aes(y=model$fitted.values),linetype="dashed") 
grf <- grf + labs(caption = "(b) trend stationary") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfsb <- grf

ts_data <- ts(serie_c)
grf <- autoplot(ts_data)
grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("value")
grf <- grf + xlab("time")
grf <- grf + geom_segment(aes(x=1,xend=100,y=mean(serie_c[1:100]),yend=mean(serie_c[1:100])))
grf <- grf + geom_segment(aes(x=101,xend=200,y=mean(serie_c[101:200]),yend=mean(serie_c[101:200])))
grf <- grf + labs(caption = "(c) level stationary") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfsc <- grf

y <- ts_data <- ts(serie_d)
grf <- autoplot(ts_data)
grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("value")
grf <- grf + xlab("time")
grf <- grf + geom_segment(aes(x=1,xend=100,y=mean(y)+var(y[1:100]),yend=mean(y)+var(y[1:100])), linetype="dashed")
grf <- grf + geom_segment(aes(x=1,xend=100,y=mean(y)-var(y[1:100]),yend=mean(y)-var(y[1:100])), linetype="dashed")
grf <- grf + geom_segment(aes(x=101,xend=200,y=mean(y)+var(y[101:200]),yend=mean(y)+var(y[101:200])), linetype="dashed")
grf <- grf + geom_segment(aes(x=101,xend=200,y=mean(y)-var(y[101:200]),yend=mean(y)-var(y[101:200])), linetype="dashed")
grf <- grf + labs(caption = "(d) heteroscedastic") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfsd <- grf

ts_data <- ts(serie_e)
grf <- autoplot(ts_data)
grf <- grf + theme_bw(base_size = 10) + geom_point(size = 0.25)
grf <- grf + theme(plot.title = element_blank())
grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
grf <- grf + ylab("value")
grf <- grf + xlab("time")
grf <- grf + labs(caption = "(e) random walk") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grf <- grf  + font
grfse <- grf

test <- data.frame(
           serie = c("(a)", "(b)", "(c)", "(d)", "(e)", "gt"), 
           adf = c(adf.test(serie_a)$p.value, 
                   adf.test(serie_b)$p.value, 
                   adf.test(serie_c)$p.value,
                   adf.test(serie_d)$p.value,
                   adf.test(serie_e)$p.value,
                   adf.test(temp_monthly$temperature)$p.value),
           pp = c(PP.test(serie_a)$p.value, 
                  PP.test(serie_b)$p.value, 
                  PP.test(serie_c)$p.value,
                  PP.test(serie_d)$p.value,
                  PP.test(serie_e)$p.value,
                  PP.test(temp_monthly$temperature)$p.value),
           bp = c(bp.test(serie_a)$p.value, 
                  bp.test(serie_b)$p.value, 
                  bp.test(serie_c)$p.value,
                  bp.test(serie_d)$p.value,
                  bp.test(serie_e)$p.value,
                  bp.test(temp_monthly$temperature)$p.value)
)

test$adf <- round(test$adf, 2)
test$pp <- round(test$adf, 2)
test$bp <- round(test$bp, 2)
print(head(test))

mypng(file="figures/chap2_stationary.png", width = 1600, height = 1260) #144 #720*1.75
gridExtra::grid.arrange(grfsa, grfsb, grfsc, grfsd, grid::nullGrob(), grfse, grid::nullGrob(),
                        layout_matrix = matrix(c(1,1,2,2,3,3,4,4,5,6,6,7), byrow = TRUE, ncol = 4))
dev.off()  

