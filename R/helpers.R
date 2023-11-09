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