source("header.R")

# Load necessary libraries
library(ggplot2)
library(ROCR)

# Sample data
true_labels <- c(0, 1, 1, 1, 1, 1, 0, 0, 0, 0)
prob_positive <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

# Create a prediction object
prediction <- prediction(prob_positive, true_labels)

# Calculate performance metrics
roc_perf <- performance(prediction, "tpr", "fpr")

# Create a data frame for the ROC curve
roc_df <- data.frame(FPR = roc_perf@x.values[[1]], TPR = roc_perf@y.values[[1]])

roc_df

# Create the ROC curve using ggplot2
grf <-ggplot(roc_df, aes(x = FPR, y = TPR))
grf <- grf + geom_line(color = "blue") 
grf <- grf + annotate(geom="text", x=0.5, y=0.75, label="ROC curve", color="blue")
grf <- grf + annotate(geom="text", x=0.15, y=0.9, label="perfect performance", color="darkgreen")
grf <- grf + geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), col="darkgreen", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), col="darkgreen", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=0.55, y=0.4, label="random performance", color="red")
grf <- grf + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") 
grf <- grf + labs(x = "False Positive Rate", y = "True Positive Rate") 
grf <- grf + theme_minimal()

save_png(grf, "figures/chap6_roc.png", 1280, 720)
