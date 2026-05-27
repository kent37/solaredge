# Misc data analysis and munging

#' Get total energy generation and consumption for all available dates
#' @export
energy_summary = function() {
  meter = read_meter() |> 
    mutate(start_date=lag(Date), end_date=Date)
  
  meter$generation = pmap_dbl(meter, energy_generation)
  meter |> 
    mutate(use = generation + meter_usage,
           daily_use = use/as.integer(end_date-start_date),
           daily_generation = generation/as.integer(end_date-start_date)
           )
}


#' Get total energy generation (kWh) between two dates
#' @param start_date The first date to count
#' @param end_date The date _after_ the last date (open interval)
energy_generation = function(start_date, end_date, ...) {
  start_date = make_date(start_date)
  end_date = make_date(end_date)
  
  energy = energy_table() |> 
    filter(date >= start_date, date < end_date) |> 
    collect() |> 
    mutate(kwh = value / 1000)

  sum(energy$kwh)
}

#' Get total energy generation by quarter
#' @description Quarters run from the 12th of March/June/September/December
#'   to the 11th of the month three months later.
#' @returns A data frame with columns `start_date`, `end_date`, and `kwh`
#' @export
quarterly_energy = function() {
  energy_table() |>
    mutate(
      adj_month = month(date - 11L),
      adj_year  = year(date - 11L),
      q_month = case_when(
        adj_month >= 3L & adj_month <= 5L  ~ 3L,
        adj_month >= 6L & adj_month <= 8L  ~ 6L,
        adj_month >= 9L & adj_month <= 11L ~ 9L,
        TRUE                               ~ 12L
      ),
      q_year = if_else(adj_month <= 2L, adj_year - 1L, adj_year)
    ) |>
    group_by(q_year, q_month) |>
    summarize(kwh = sum(value, na.rm = TRUE) / 1000, .groups = "drop") |>
    collect() |>
    mutate(
      start_date = lubridate::make_date(q_year, q_month, 12),
      end_date   = lubridate::make_date(q_year, q_month, 12) + months(3) - days(1)
    ) |>
    select(start_date, end_date, kwh) |>
    arrange(start_date)
}

#' Read and munge historical meter values
read_meter = function() {
  meter_path = here::here('misc/Meter_readings.csv')
  meter = read_csv(meter_path, show_col_types=FALSE, comment='#') |> 
    mutate(Date=mdy(Date),
    meter_true = if_else(Meter<90000, Meter, Meter-100000),
    meter_usage = meter_true - lag(meter_true),
    meter_usage = coalesce(meter_usage, Usage))
  meter
}
