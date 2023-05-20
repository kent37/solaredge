# Misc data analysis and munging

#' Get total energy generation and consumption for all available dates
#' @export
energy_summary = function() {
  meter = read_meter() |> 
    mutate(start_date=lag(Date), end_date=Date)
  
  meter$generation = pmap_dbl(meter, energy_generation)
  meter |> 
    mutate(use = generation + meter_usage,
           daily_use = use/as.integer(end_date-start_date)
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

#' Read and munge historical meter values
read_meter = function() {
  meter_path = here::here('misc/Meter_readings.csv')
  meter = read_csv(meter_path, show_col_types=FALSE) |> 
    mutate(Date=mdy(Date),
    meter_true = if_else(Meter<90000, Meter, Meter-100000),
    meter_usage = meter_true - lag(meter_true),
    meter_usage = coalesce(meter_usage, Usage))
  meter
}
