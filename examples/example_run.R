library(readr)
library(dplyr)

source("R/calculate_temporal_indices.R")

# Example input data
daily_cases <- tibble::tibble(
  area = c("Sanmin", "Sanmin", "Sanmin", "Fengshan", "Fengshan", "Fengshan"),
  date = c("2023-01-01", "2023-01-02", "2023-01-10",
           "2023-01-01", "2023-01-08", "2023-01-15"),
  cases = c(0, 2, 3, 1, 0, 4),
  population = c(100000, 100000, 100000, 120000, 120000, 120000)
)

temporal_indices <- calculate_temporal_indices(
  data = daily_cases,
  area_col = "area",
  date_col = "date",
  cases_col = "cases",
  population_col = "population",
  epidemic_start = "2023-01-01",
  epidemic_end = "2023-12-31",
  week_start = 1,
  intensity_per = 10000,
  min_wave_length = 3
)

print(temporal_indices)

# Optional save
# write_csv(temporal_indices, "output/temporal_indices.csv")