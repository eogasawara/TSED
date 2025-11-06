library(readr)
library(dplyr)

# NOAA Global Temperature ETL
# - Reads monthly CSVs for all months (1..12) over 1850-2023
# - Builds a monthly series with yyyymm as an integer index (x)
# - Builds a yearly series by averaging monthly temperatures per year
# - Saves both objects as RData files under data/noaa-global/
#
# Expected working directory: project root (so that data/noaa-global exists)

base_dir <- file.path("data", "noaa-global")

# Read and assemble monthly CSV files
raw <- NULL
for (m in 1:12) {
  csv_path <- file.path(base_dir, sprintf("1850-2023-%d.csv", m))
  month_df <- read_csv(csv_path, show_col_types = FALSE)
  # Build yyyymm integer index
  month_df$ym <- month_df$Year * 100 + m
  month_df$Year <- NULL
  raw <- rbind(raw, month_df)
}

# Monthly time series: keep to 2022-12 (exclude partial 2023)
temp_monthly <- raw |>
  arrange(ym) |>
  filter(ym < 202300) |>
  select(x = ym, temperature = Value)

# Add a Date column for convenience (YYYY-MM-01)
temp_monthly$date <- as.Date(sprintf("%d-%02d-01",
                                     as.integer(temp_monthly$x / 100),
                                     temp_monthly$x %% 100))

# Yearly series: average temperature per year
temp_yearly <- temp_monthly |>
  mutate(year = as.integer(x / 100)) |>
  group_by(year) |>
  summarise(temperature = mean(temperature), .groups = "drop") |>
  mutate(date = as.Date(sprintf("%d-01-01", year))) |>
  select(x = year, temperature, date)

# Persist results
save(temp_monthly, file = file.path(base_dir, "temp_monthly.RData"), compress = TRUE)
save(temp_yearly,  file = file.path(base_dir, "temp_yearly.RData"),  compress = TRUE)

