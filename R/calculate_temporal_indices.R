# calculate_temporal_indices.R
# =========================================================
# Title: Temporal Indices Calculation for Dengue Weekly Data
# Author: Shi Wen Goh ORCID:0009-0001-5656-2390
# Description:
#   Functions to calculate temporal indices described in
#   Wen et al. (2010), adapted for daily case-count data by
#   first aggregating to weekly counts.
#
#   Indices:
#     1. Occurrence index (a):
#        proportion of weeks with >= 1 case during the full epidemic period
#     2. Duration index (b):
#        mean number of weeks per epidemic wave
#     3. Intensity index (g):
#        mean incidence of cumulative cases per epidemic wave
#        lasting more than 2 weeks
#
# Dependencies:
#   dplyr, tidyr, lubridate, purrr, rlang
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(rlang)
})

#' Validate required columns in input data
#'
#' @param data A data frame.
#' @param required_cols Character vector of required column names.
#'
#' @return Invisibly returns TRUE if all required columns exist.
#' @noRd
.validate_required_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required column(s): %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Identify epidemic waves from weekly case counts
#'
#' Consecutive weeks with weekly_cases > 0 are treated as one epidemic wave.
#'
#' @param weekly_cases Numeric vector of weekly case counts.
#'
#' @return Integer vector of wave IDs, with NA for zero-case weeks.
#' @export
identify_weekly_waves <- function(weekly_cases) {
  if (!is.numeric(weekly_cases)) {
    stop("`weekly_cases` must be numeric.", call. = FALSE)
  }
  
  present <- weekly_cases > 0
  runs <- rle(present)
  run_ids <- rep(seq_along(runs$lengths), runs$lengths)
  wave_id <- ifelse(present, run_ids, NA_integer_)
  
  return(wave_id)
}

#' Aggregate daily case counts into weekly counts
#'
#' @param data A data frame containing daily case counts.
#' @param area_col Name of the area column.
#' @param date_col Name of the date column.
#' @param cases_col Name of the daily case count column.
#' @param population_col Optional name of the population column.
#' @param week_start Integer passed to lubridate::floor_date().
#'   1 = Monday, 7 = Sunday.
#'
#' @return A tibble with one row per area-week.
#' @export
daily_to_weekly <- function(data,
                            area_col = "area",
                            date_col = "date",
                            cases_col = "cases",
                            population_col = NULL,
                            week_start = 1) {
  required_cols <- c(area_col, date_col, cases_col)
  .validate_required_columns(data, required_cols)
  
  if (!is.null(population_col)) {
    .validate_required_columns(data, population_col)
  }
  
  data %>%
    mutate(
      !!date_col := as.Date(.data[[date_col]]),
      week_start_date = floor_date(.data[[date_col]], unit = "week", week_start = week_start)
    ) %>%
    group_by(.data[[area_col]], week_start_date) %>%
    summarise(
      weekly_cases = sum(.data[[cases_col]], na.rm = TRUE),
      population = if (!is.null(population_col)) first(.data[[population_col]]) else NA_real_,
      .groups = "drop"
    ) %>%
    arrange(.data[[area_col]], week_start_date)
}

#' Create complete epidemic week sequence
#'
#' @param epidemic_start Start date of epidemic period.
#' @param epidemic_end End date of epidemic period.
#' @param week_start Integer passed to lubridate::floor_date().
#'
#' @return A Date vector of week start dates.
#' @export
make_epidemic_weeks <- function(epidemic_start,
                                epidemic_end,
                                week_start = 1) {
  epidemic_start <- as.Date(epidemic_start)
  epidemic_end <- as.Date(epidemic_end)
  
  if (is.na(epidemic_start) || is.na(epidemic_end)) {
    stop("`epidemic_start` and `epidemic_end` must be valid dates.", call. = FALSE)
  }
  
  if (epidemic_end < epidemic_start) {
    stop("`epidemic_end` must be on or after `epidemic_start`.", call. = FALSE)
  }
  
  seq(
    from = floor_date(epidemic_start, unit = "week", week_start = week_start),
    to   = floor_date(epidemic_end, unit = "week", week_start = week_start),
    by   = "week"
  )
}

#' Calculate temporal indices for one area using weekly data
#'
#' @param weekly_data A data frame with columns:
#'   `week_start_date` and `weekly_cases`.
#' @param area_name Area identifier.
#' @param epidemic_weeks Full vector of epidemic week start dates.
#' @param population Optional population size for incidence-based intensity.
#' @param intensity_per Scaling factor for incidence, default 10000.
#' @param min_wave_length Minimum epidemic wave length in weeks
#'   for inclusion in intensity calculation. Default is 3,
#'   corresponding to "persisted for more than 2 weeks".
#'
#' @return A tibble with one row and the calculated temporal indices.
#' @export
calculate_temporal_indices_one_area <- function(weekly_data,
                                                area_name,
                                                epidemic_weeks,
                                                population = NULL,
                                                intensity_per = 10000,
                                                min_wave_length = 3) {
  .validate_required_columns(weekly_data, c("week_start_date", "weekly_cases"))
  
  weekly_data <- weekly_data %>%
    mutate(week_start_date = as.Date(week_start_date)) %>%
    arrange(week_start_date)
  
  full_df <- tibble(week_start_date = as.Date(epidemic_weeks)) %>%
    left_join(weekly_data, by = "week_start_date") %>%
    mutate(
      weekly_cases = replace_na(weekly_cases, 0)
    ) %>%
    arrange(week_start_date)
  
  total_weeks <- nrow(full_df)
  weeks_with_cases <- sum(full_df$weekly_cases > 0, na.rm = TRUE)
  
  # Occurrence index (a)
  occurrence_index <- weeks_with_cases / total_weeks
  
  # Identify epidemic waves
  full_df <- full_df %>%
    mutate(wave_id = identify_weekly_waves(weekly_cases))
  
  wave_summary <- full_df %>%
    filter(!is.na(wave_id)) %>%
    group_by(wave_id) %>%
    summarise(
      start_week = min(week_start_date),
      end_week = max(week_start_date),
      duration_weeks = n(),
      total_cases = sum(weekly_cases, na.rm = TRUE),
      .groups = "drop"
    )
  
  n_waves <- nrow(wave_summary)
  
  # Duration index (b)
  duration_index <- if (n_waves > 0) {
    mean(wave_summary$duration_weeks, na.rm = TRUE)
  } else {
    0
  }
  
  # Intensity index (g)
  eligible_waves <- wave_summary %>%
    filter(duration_weeks >= min_wave_length)
  
  intensity_index <- if (nrow(eligible_waves) > 0) {
    if (!is.null(population) && !is.na(population)) {
      mean((eligible_waves$total_cases / population) * intensity_per, na.rm = TRUE)
    } else {
      mean(eligible_waves$total_cases, na.rm = TRUE)
    }
  } else {
    NA_real_
  }
  
  tibble(
    area = area_name,
    total_weeks = total_weeks,
    weeks_with_cases = weeks_with_cases,
    n_waves = n_waves,
    occurrence_index = occurrence_index,
    duration_index = duration_index,
    intensity_index = intensity_index
  )
}

#' Calculate temporal indices for multiple areas from daily case data
#'
#' This function first aggregates daily case counts into weekly counts,
#' then calculates the three temporal indices defined by Wen et al. (2010).
#'
#' @param data A data frame containing daily case counts.
#' @param area_col Name of the area column.
#' @param date_col Name of the date column.
#' @param cases_col Name of the daily case count column.
#' @param population_col Optional name of the population column.
#' @param epidemic_start Start date of full epidemic period.
#'   If NULL, uses minimum date in data.
#' @param epidemic_end End date of full epidemic period.
#'   If NULL, uses maximum date in data.
#' @param week_start Integer passed to lubridate::floor_date().
#'   1 = Monday, 7 = Sunday.
#' @param intensity_per Scaling factor for incidence, default 10000.
#' @param min_wave_length Minimum wave length in weeks for inclusion
#'   in intensity calculation. Default 3.
#'
#' @return A tibble with one row per area.
#' @export
calculate_temporal_indices <- function(data,
                                       area_col = "area",
                                       date_col = "date",
                                       cases_col = "cases",
                                       population_col = NULL,
                                       epidemic_start = NULL,
                                       epidemic_end = NULL,
                                       week_start = 1,
                                       intensity_per = 10000,
                                       min_wave_length = 3) {
  required_cols <- c(area_col, date_col, cases_col)
  .validate_required_columns(data, required_cols)
  
  if (!is.null(population_col)) {
    .validate_required_columns(data, population_col)
  }
  
  data <- data %>%
    mutate(
      !!date_col := as.Date(.data[[date_col]])
    )
  
  if (is.null(epidemic_start)) {
    epidemic_start <- min(data[[date_col]], na.rm = TRUE)
  }
  
  if (is.null(epidemic_end)) {
    epidemic_end <- max(data[[date_col]], na.rm = TRUE)
  }
  
  epidemic_weeks <- make_epidemic_weeks(
    epidemic_start = epidemic_start,
    epidemic_end = epidemic_end,
    week_start = week_start
  )
  
  weekly_data <- daily_to_weekly(
    data = data,
    area_col = area_col,
    date_col = date_col,
    cases_col = cases_col,
    population_col = population_col,
    week_start = week_start
  )
  
  result <- weekly_data %>%
    group_split(.data[[area_col]]) %>%
    map_dfr(function(sub_df) {
      area_name <- unique(sub_df[[area_col]])[1]
      
      population_value <- NULL
      if (!is.null(population_col)) {
        population_candidates <- unique(sub_df$population)
        population_candidates <- population_candidates[!is.na(population_candidates)]
        population_value <- if (length(population_candidates) > 0) population_candidates[1] else NA_real_
      }
      
      calculate_temporal_indices_one_area(
        weekly_data = sub_df %>% select(week_start_date, weekly_cases),
        area_name = area_name,
        epidemic_weeks = epidemic_weeks,
        population = population_value,
        intensity_per = intensity_per,
        min_wave_length = min_wave_length
      )
    })
  
  return(result)
}