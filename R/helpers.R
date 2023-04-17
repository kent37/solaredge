# Misc helper functions

make_date = function(d) {
  if (is.character(d))
    lubridate::ymd(d)
  else if (lubridate::is.Date(d))
    d
  else stop('Invalid date format: ', d)
}