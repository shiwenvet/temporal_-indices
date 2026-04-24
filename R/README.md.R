# Temporal Indices Calculation for Dengue

This repository contains R functions to calculate the temporal indices described in Wen et al. (2010):
  
  - Occurrence index (a)
- Duration index (b)
- Intensity index (g)

The code accepts daily case-count data and first aggregates them into weekly case counts to match the original paper's methodology.

## Input format

The input data frame should contain:

- `area`: spatial unit
- `date`: date of observation
- `cases`: daily case count
- `population`: optional population for incidence-based intensity

## Example

```r
source("R/calculate_temporal_indices.R")

result <- calculate_temporal_indices(
  data = daily_cases,
  area_col = "area",
  date_col = "date",
  cases_col = "cases",
  population_col = "population",
  epidemic_start = "2023-01-01",
  epidemic_end = "2023-12-31"
)