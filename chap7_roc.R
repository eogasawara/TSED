source("header.R")
# Load necessary libraries
library(caret)
library(PRROC)
library(dplyr)
library(patchwork)

actual_labels <- c(TRUE, TRUE, FALSE,  TRUE, TRUE, FALSE,  FALSE, FALSE,  TRUE, FALSE)
 evt <-          c(0.9,  0.8,   0.7,   0.6,  0.55, 0.54,   0.53, 0.51,   0.5,   0.4)
 
 actual_labels <- !actual_labels
 evt <- 1 - evt
 
 data <- data.frame(detect = evt >= 0.5, evt, actual_labels)
 
 print(data)
 
actual_labels <- as.integer(actual_labels) 
 
predictions <- data.frame(evt)
predicted_probs <- evt

# Generate precision-recall curve
pr <- pr.curve(scores.class0 = predicted_probs, weights.class0 = actual_labels, sorted = FALSE, curve = TRUE)

# Plot precision-recall curve
# plot(pr)

pr_df <- data.frame(recall = pr$curve[,1], precision = pr$curve[,2])
pr_df <- pr_df |> group_by(recall) |> summarise(precision = max(precision))

grf <-ggplot(pr_df, aes(x = recall, y = precision))
grf <- grf + theme_minimal()
grf <- grf + geom_line(color = "blue") 
grf <- grf + annotate(geom="text", x=0.5, y=1.025, label=sprintf("PR curve = %.2f", pr$auc.integral), color="blue")
grf <- grf + annotate(geom="text", x=0.5, y=0.975, label="skilled method", color="darkgreen")
grf <- grf + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), col="darkgreen", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=0.5, y=0.525, label="unskilled method", color="red")
grf <- grf + geom_segment(aes(x = 0, y = 0.5, xend = 1, yend = 0.5), col="red", linewidth = 0.5, linetype="dashed")
grf <- grf + labs(caption = "(b)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grfPR <- grf
plot(grfPR)

proc <- roc.curve(scores.class0 = predicted_probs, weights.class0 = actual_labels, sorted = FALSE, curve = TRUE)

# Plot precision-recall curve
# plot(proc)

roc_df <- data.frame(FPR = proc$curve[,1], TPR = proc$curve[,2])
roc_df <- roc_df |> group_by(FPR) |> summarise(TPR = max(TPR))

grf <-ggplot(roc_df, aes(x = FPR, y = TPR))
grf <- grf + theme_minimal()
grf <- grf + geom_line(color = "blue") 
grf <- grf + annotate(geom="text", x=0.25, y=0.95, label="perfect performance", color="darkgreen")
grf <- grf + geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), col="darkgreen", linewidth = 0.5, linetype="dashed")
grf <- grf + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), col="darkgreen", linewidth = 0.5, linetype="dashed")
grf <- grf + annotate(geom="text", x=0.65, y=0.4, label="random performance", color="red")
grf <- grf + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") 
grf <- grf + annotate(geom="text", x=0.5, y=1.05, label=sprintf("ROC curve = %.2f", proc$auc), color="blue")
grf <- grf + labs(caption = "(a)") 
grf <- grf + theme(plot.caption = element_text(hjust = 0.5))
grfROC <- grf
plot(grfROC)

# To print precision and recall values
print(proc$curve)

grf <- wrap_plots(grfROC, grfPR, ncol = 2, widths = c(1, 1, 1), heights = c(1))
save_png(grf, "figures/chap7_roc.png", width = 1280, height = 720)

