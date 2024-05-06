library(readr)
library(dplyr)

data <- NULL
for (i in 1:12)  {
  data_month <- read_csv(sprintf("noaa-global/1850-2023-%d.csv", i))
  data_month$Date <- data_month$Year*100+i
  data_month$Year <- NULL
  data <- rbind(data, data_month)
}

temp_monthly <- data |> 
  arrange(Date) |> 
  filter(Date < 202300) |>
  select(x = Date, temperature = Value)

examples_harbinger$global_temperature_monthly <- examples_harbinger$global_temperature_monthly + 13.9

temp_yearly <- temp_monthly

temp_monthly$i <-  as.Date(sprintf("%d-%02d-01", as.integer(temp_monthly$i / 100), temp_monthly$i %% 100))
temp_yearly$i <-  as.Date(sprintf("%d-01-01", as.integer(temp_yearly$i / 100)))

temp_yearly <- temp_yearly |> group_by(x) |> summarise(temperature = mean(temperature)) 

save(temp_monthly, file="noaa-global/temp_monthly.RData", compress=TRUE)
save(temp_yearly, file="noaa-global/temp_yearly.RData", compress=TRUE)

