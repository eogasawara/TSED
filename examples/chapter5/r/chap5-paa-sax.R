library(ggplot2)

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(ggplot2)

# 1) Generate a simple synthetic series with mild noise
set.seed(42)
n <- 60
time <- seq_len(n)
series <- sin(0.2 * time) + rnorm(n, mean = 0, sd = 0.2)
# 2) Z-score normalization for scale invariance
series_norm <- scale(series)
# 3) PAA with block size = 5 (average every 5 points)
block <- 5
blocks <- seq(1, n, by = block)
paa <- sapply(blocks, function(i) mean(series_norm[i:(i + block - 1)]))
paa_rep <- rep(paa, each = block)  # expand to original length for plotting
# 4) SAX with 3 symbols using N(0,1) breakpoints [-0.43, 0.43]
breakpoints <- c(-Inf, -0.43, 0.43, Inf)
alphabet <- c("a", "b", "c")
sax <- cut(paa, breaks = breakpoints, labels = alphabet, include.lowest = TRUE)
sax_labels <- rep(as.character(sax), each = block)  # align with original timeline
# 5) Build plotting data frame
df <- data.frame(
  Time = time,
  Series = as.numeric(series_norm),
  PAA = paa_rep,
  SAX = sax_labels
)

p <- ggplot(df, aes(x = Time)) +
  geom_line(aes(y = Series), color = "gray70", linewidth = 1.2) +
  geom_line(aes(y = PAA), color = "orange", linetype = "dashed", linewidth = 1) +
  geom_text(aes(y = 2, label = SAX), size = 5) +
  labs(title = "PAA and SAX Representation in Time Series",
       x = "Time", y = "Value (normalized)") +
  ylim(-2.5, 2.5) +
  theme_minimal()
#ggplot2::ggsave(filename = "figures/chap5_paa_sax.png", plot = p, width = 12.8, height = 7.2, units = "in", dpi = 100)
p
