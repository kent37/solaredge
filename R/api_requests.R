# Access the SolarEdge API

#' Get detailed energy data
#' @param start_time string or DateTime to start
#' @param end_time string or DateTime to end, must be <= now()
#' @param time_unit Granularity to report
#' @returns A data frame with columns `meter`, `date`, `value`, `unit`,
#' `year`, `month`, `day`, `time`
#' @importFrom rlang .data
#' @export
get_energy_details = function(start_time, end_time,
  time_unit=c('QUARTER_OF_AN_HOUR', 'HOUR', 'DAY', 'WEEK', 'MONTH', 'YEAR ')) 
{
  get_details(start_time, end_time, time_unit, 'energyDetails')
}

#' Get detailed power data
#' @param start_time string or DateTime to start
#' @param end_time string or DateTime to end, must be <= now()
#' @param time_unit Granularity to report
#' @returns A data frame with columns `meter`, `date`, `value`, `unit`,
#' `year`, `month`, `day`, `time`
#' @importFrom rlang .data
#' @export
get_power_details = function(start_time, end_time,
  time_unit=c('QUARTER_OF_AN_HOUR', 'HOUR', 'DAY', 'WEEK', 'MONTH', 'YEAR ')) 
{
  get_details(start_time, end_time, time_unit, 'powerDetails')
}

# Implementation of both energyDetails and powerDetails
get_details = function(start_time, end_time,
  time_unit=c('QUARTER_OF_AN_HOUR', 'HOUR', 'DAY', 'WEEK', 'MONTH', 'YEAR '),
  which_details) 
{
  time_unit = match.arg(time_unit)
  if (is.character(start_time))
    start_time = lubridate::ymd_hms(start_time)
  if (is.character(end_time))
    end_time = lubridate::ymd_hms(end_time)
  
  if (end_time <= start_time)
    stop('end_time must be after start_time.')
  if (end_time > lubridate::now())
    stop('end_time must be in the past.')
  
  secrets = solaredge_secrets()
  url = stringr::str_glue('https://monitoringapi.solaredge.com/site/{secrets$site_id}/{which_details}')
  
  req = httr2::request(url) |> 
    httr2::req_url_query(startTime=format(start_time, "%Y-%m-%d %H:%M:%S"),
                  endTime=format(end_time, "%Y-%m-%d %H:%M:%S"),
                  timeUnit=time_unit,
                  meters='Production',
                  api_key=secrets$key)
  resp = req |> 
    httr2::req_perform()

  raw = resp |> httr2::resp_body_string() |> jsonlite::fromJSON()
  unit = raw[[which_details]][['unit']]
  cooked = raw[[which_details]][["meters"]] |>
    tidyr::unnest(.data$values) |> 
    dplyr::mutate(date=lubridate::as_datetime(date), unit=unit) |> 
    dplyr::rename(meter=.data$type)

  cooked |> expand_date()
}

#' Get the SolarEdge key and site id from the environment
#' @returns A list with `site_id` and `key` members
solaredge_secrets = function() {
  site_id = Sys.getenv('SOLAREDGE_SITE_ID')
  if (site_id == '')
    stop('Please define the environment variable SOLAREDGE_SITE_ID.')
  
  key = Sys.getenv('SOLAREDGE_API_KEY')
  if (key == '')
    stop('Please define the environment variable SOLAREDGE_API_KEY.')
  
  list(site_id=site_id, key=key)
}

# Add helpful datetime columns to a data frame:
# year, month, day, time_of_day
expand_date = function(df) {
  stopifnot('date' %in% names(df))
  
  df |> 
    dplyr::mutate(
      year=lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date),
      time = hms::as_hms(date)
    )
}