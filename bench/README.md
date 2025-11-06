# TSED Benchmarks (Simple and Tuned)

This folder contains two scripts to benchmark event/anomaly detection methods on the `gecco` dataset. Use the short script for quick smoke tests and the full script for a comprehensive run with hyperparameter tuning.

## Scripts

- `benchmark-short.R` — fast run with two representative methods (FBIAD and ARIMA).
- `benchmark-full.R` — full suite including MLP/ELM/RF/SVM/LSTM/Conv1D and tuned variants.

Both scripts:
- Load `gecco` via `data(gecco)` from installed packages.
- Slice each series to rows `[16500:18000]` for speed/reproducibility.
- Cache per-method, per-series results under `results/`.
- Produce an overall summary `results/exp_summary.RData`.

## Requirements

- R (≥ 4.0 recommended)
- Packages: `daltoolbox`, `daltoolboxdp`, `tspredit`, `harbinger`, `united`
- Optional (for rendering R Markdown variants in `Rmd/bench/`): `rmarkdown`

Install missing packages in R:

```r
install.packages(c("harbinger", "united"))
# Other packages may be installed from their usual sources as needed
```

## How to Run

From the project root (so results are written to `./results/`):

- Quick benchmark (FBIAD + ARIMA):

```sh
Rscript bench/benchmark-short.R
# or inside R: source("bench/benchmark-short.R")
```

- Full benchmark (all methods + tuned variants):

```sh
Rscript bench/benchmark-full.R
# or inside R: source("bench/benchmark-full.R")
```

R Markdown equivalents with narrative are in `Rmd/bench/`:

```r
rmarkdown::render("Rmd/bench/benchmark-short.Rmd")
rmarkdown::render("Rmd/bench/benchmark-full.Rmd")
```

## Results and Caching

- Per-method details: `results/exp_detail_<method>.RData`
- Summary: `results/exp_summary.RData` containing `experiments_summary` (and a legacy alias `resumo_experimentos`)

Incremental caching lets you resume runs without recomputing finished series. To force a fresh run, delete files in `results/` before executing again.

## Customization

- Number of series: by default the scripts run on a single series for speed. To run all available series, change the line that sets `series_ts <- vector("list", 1)` to `series_ts <- vector("list", length(gecco) - 1)`.
- Slice interval: adjust `start_row` and `end_row` if your series lengths differ from the default.
- Evaluation window: change `har_eval_soft(sw_size = 10)` to tune soft evaluation tolerance.

## Reference

- Ogasawara, Event Detection in Time Series.
