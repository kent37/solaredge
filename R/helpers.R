# Misc helper functions

#' Convert input date from string if needed
#' @param d A Date or string
#' @returns A Date
make_date = function(d) {
  if (is.character(d))
    lubridate::ymd(d)
  else if (lubridate::is.Date(d))
    d
  else stop('Invalid date format: ', d)
}

#' Update the database and plot power
#' @export
update_and_plot = function() {
  update_energy()
  update_power()
  power_chart()
}

#' Read the PV2 predictions of monthly energy in kWh
#' @returns A data.frame with columns Month, kWh
#' @export
monthly_predictions = function() {
  read_csv(here::here('misc/Monthly_estimates.csv'), 
           comment='#', show_col_types=FALSE)
}

#' Given a data frame with a `date` column, remove any partial month at the
#' beginning or end.
clip_partial_month = function(df) {
  # Find the first month's dates
  first_month <- df |> 
    filter(year(date) == min(year(date))) |> 
    filter(month(date) == min(month(date)))
  
  first_date = min(first_month$date)
  
  first_is_first = first_date == floor_date(first_date, unit='month')
  
  # If not all dates are present, delete rows for the first month
  if (!first_is_first) {
    df <- df |> 
      filter(!(year(date) == year(first_date) & month(date) == month(first_date)))
  }
  
  # Find the last month's dates
  last_month <- df |> 
    filter(year(date) == max(year(date))) |> 
    filter(month(date) == max(month(date)))
  
  last_date = max(last_month$date)
  
  last_is_last = last_date == ceiling_date(last_date, unit='month') - 1
  
  # If not all dates are present, delete rows for the last month
  if (!last_is_last) {
    df <- df |> 
      filter(!(year(date) == year(last_date) & month(date) == month(last_date)))
  }
  
  df
  }
  