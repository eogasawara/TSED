# Required packages
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
library(united)

safe_get <- function(lst, i) {
  if (i > 0 && i <= length(lst)) {
    lst[[i]]
  } else {
    NULL
  }
}

## ------------------------------------------------------------
## 1) Methods setup (models) ----
## ------------------------------------------------------------
methods <- list(
  hanr_fbiad(),  # FBIAD
  hanr_arima()   # ARIMA
)
names(methods) <- c("fbiad", "arima")

## ------------------------------------------------------------
## 2) Data preparation ----
## ------------------------------------------------------------
dataset_name <- "gecco"
data(gecco)  # load 'gecco' dataset into the environment

# Slice each series to the same interval [16500:18000]
# Note: adjust this slice if series lengths vary.
series_ts <- vector("list", length(gecco) - 1)

# Initially use only one series to test hyperparameter optimization
series_ts <- vector("list", 1)
for (i in seq_along(series_ts)) {
  series_name <- names(gecco)[i]
  # Bounds check to avoid errors if the series is shorter
  n <- nrow(gecco[[i]])
  start_row <- 16500L
  end_row   <- 18000L
  if (is.null(n)) {
    stop(sprintf("Object %s is not an expected data.frame/ts.", series_name))
  }
  if (end_row > n) {
    stop(sprintf("Series %s has only %d rows; adjust the slice (%d:%d).",
                 series_name, n, start_row, end_row))
  }
  series_ts[[i]] <- gecco[[i]][start_row:end_row, ]
  names(series_ts)[i] <- series_name
}

## Ensure bench/results directory exists
dir.create("bench/results", showWarnings = FALSE, recursive = TRUE)

## ------------------------------------------------------------
## 3) Detailed detection (with per-method cache) ----
## ------------------------------------------------------------
all_details <- list()

for (j in seq_along(methods)) {                 # iterate methods
  current_model   <- methods[[j]]
  model_name      <- names(methods)[j]
  model_details   <- list()                     # bench/results per series for this method
  
  # Cache file path for this method
  cache_file <- file.path("bench/results", sprintf("exp_detail_%s.RData", model_name))
  
  # If a precomputed result exists, load it to continue
  if (file.exists(cache_file)) {
    load(file = cache_file)  # may load 'model_details' or legacy 'detalhes_modelo'
    if (exists("detalhes_modelo")) {
      model_details <- detalhes_modelo
    }
  }
  
  for (i in seq_along(series_ts)) {             # iterate series
    series_data <- series_ts[[i]]
    series_name <- names(series_ts)[i]
    
    result <- safe_get(model_details, i)
    
    if (is.null(result)) {
      
      # If there is no result for this series yet, process
      model_details[[i]] <- tryCatch({
        ## 3.1 Fit
        start_time <- Sys.time()
        fitted_model <- fit(current_model, series_data$value)
        fit_time <- as.double(Sys.time() - start_time, units = "secs")
        
        ## 3.2 Detect
        start_time <- Sys.time()
        detection_result <- detect(fitted_model, series_data$value)
        detect_time <- as.double(Sys.time() - start_time, units = "secs")
        
        ## 3.3 Package this series result
        result <- list(
          md          = fitted_model,
          rs          = detection_result,
          data_index  = i,                 # series index
          modelname   = model_name,
          datasetname = dataset_name,
          seriesname  = series_name,
          time_fit    = fit_time,
          time_detect = detect_time
        )
        names(result)[i] <- sprintf("%s_%s", dataset_name, series_name)
        
        ## if successful, return result
        result
      }, error = function(e) {
        message(sprintf("Error in %s - %s: %s", model_name, series_name, e$message))
        ## return NULL on failure
        NULL
      })
    }
    ## 3.4 Save incremental cache (save new and legacy names for compatibility)
    detalhes_modelo <- model_details
    save(model_details, detalhes_modelo, file = cache_file, compress = "xz")
  }
  
  ## Accumulate this method's details into the overall list
  all_details <- c(all_details, model_details)
}

## ------------------------------------------------------------
## 4) Performance summary (time and metrics) ----
## ------------------------------------------------------------
summary_rows <- vector("list", length(all_details))
for (k in seq_along(all_details)) {
  exp_k         <- all_details[[k]]
  series_index  <- if (!is.null(exp_k$data_index)) exp_k$data_index else exp_k$dataref
  series_k_data <- series_ts[[series_index]]
  
  # Soft evaluation with sliding window (adjust sw_size as needed)
  soft_eval <- evaluate(har_eval_soft(sw_size = 10),
                        exp_k$rs$event, series_k_data$event)
  
  # Summary row for this series and method
  summary_rows[[k]] <- data.frame(
    method      = exp_k$modelname,
    dataset     = exp_k$datasetname,
    series      = exp_k$seriesname,
    time_fit    = exp_k$time_fit,
    time_detect = exp_k$time_detect,
    precision   = soft_eval$precision,
    recall      = soft_eval$recall,
    f1          = soft_eval$F1,
    stringsAsFactors = FALSE
  )
}

experiments_summary <- do.call(rbind, summary_rows)
resumo_experimentos <- experiments_summary  # legacy name for compatibility

## ------------------------------------------------------------
## 5) Persist summary ----
## ------------------------------------------------------------
save(experiments_summary, resumo_experimentos,
     file = file.path("bench/results", "exp_summary.RData"),
     compress = "xz")
