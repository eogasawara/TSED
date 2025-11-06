## Implicit Periodicity Detection (English, commented)
# Parameters for a synthetic sinusoidal time series
n <- 1024            # number of observations
period <- 9          # seasonality period (e.g., 9 time steps)
amplitude <- 5       # sinusoid amplitude
# Generate the series: sinusoid + Gaussian noise
t <- 1:n
seasonality <- amplitude * sin(2 * pi * t / period)
noise <- rnorm(n, mean = 0, sd = 2)
ts_data <- ts(10 + seasonality + noise)

plot(
  ts_data,
  main = paste("Series with implicit seasonality (period ~", period, ")"),
  ylab = "Value",
  xlab = "Time"
)

# ACF configuration
lag.max <- 36
# Compute ACF without plotting
acf_obj <- acf(ts_data, lag.max = lag.max, plot = FALSE)
# Consider lags from 1 to lag.max (remove lag 0)
lags <- 1:lag.max
acf_vals <- acf_obj$acf[-1]
# 95% significance threshold for the ACF values
sig_limit <- qnorm((1 + 0.95)/2) / sqrt(length(ts_data))
# Keep significant lags (not used below but informative)
significant_lags <- lags[abs(acf_vals) > sig_limit]
# Heuristic function to estimate periodicity from ACF local maxima
find_periodicity <- function(acf_vals, lags, span = 3) {
  # Detect local maxima in a sliding window of size 'span'
  n <- length(acf_vals)
  peak_indices <- c()
  for (i in (span + 1):(n - span)) {
    window <- acf_vals[(i - span):(i + span)]
    center <- acf_vals[i]
    if (center == max(window) && center > 0) {
      peak_indices <- c(peak_indices, i)
    }
  }
  # Case 1: multiple peaks — use the most frequent spacing between peaks
  if (length(peak_indices) >= 2) {
    if (max(peak_indices) <= length(lags)) {
      lag_peaks <- lags[peak_indices]
      spacing <- diff(lag_peaks)
      period_mode <- as.numeric(names(sort(table(spacing), decreasing = TRUE)[1]))
      return(period_mode)
    }
  }
  # Case 2: single peak — return the corresponding lag
  if (length(peak_indices) == 1) {
    return(lags[peak_indices])
  }
  # No clear periodicity
  return(NULL)
}
period_est <- find_periodicity(acf_vals, lags)
if (!is.null(period_est)) {
  message("Estimated implicit periodicity: ~", period_est, " lags.")
} else {
  message("No clear periodicity detected.")
}
# ACF visualization
acf(ts_data, lag.max = lag.max, main = "ACF - Implicit Seasonality Detection")
