# Access the SolarEdge API

#' Get daily energy data
#' @param start_date string or DateTime to start
#' @param end_date string or Date to end, must be <= now()
#' @returns A data frame with columns `date`, `value`, `unit`,
#' `year`, `month`, `day`, `time`
#' @importFrom rlang .data
#' @export
get_daily_energy = function(start_date, end_date) 
{
  if (is.character(start_date))
    start_date = lubridate::ymd(start_date)
  if (is.character(end_date))
    end_date = lubridate::ymd(end_date)
  
  if (end_date < start_date)
    stop('end_date must be after start_date.')
  if (end_date > lubridate::today())
    stop('end_date must be in the past.')
  
  secrets = solaredge_secrets()
  url = stringr::str_glue('https://monitoringapi.solaredge.com/site/{secrets$site_id}/energy')
  
  req = httr2::request(url) |> 
    httr2::req_url_query(startDate=format(start_date, "%Y-%m-%d"),
                  endDate=format(end_date, "%Y-%m-%d"),
                  timeUnit='DAY',
                  api_key=secrets$key)
  resp = req |> 
    httr2::req_perform()

  raw = resp |> httr2::resp_body_string() |> jsonlite::fromJSON()
  unit = raw[['energy']][['unit']]
  cooked = raw[['energy']][["values"]] |>
    dplyr::mutate(date=lubridate::as_datetime(date), unit=unit) |> 
    dplyr::filter(!is.na(.data$value)) |> 
    expand_date()
  
  cooked
}

#' Get detailed power data
#' @param start_time string or DateTime to start
#' @param end_time string or DateTime to end, must be <= now()
#' @returns A data frame with columns `date`, `value`, `unit`,
#' `year`, `month`, `day`, `time`
#' @importFrom rlang .data
#' @export
get_power = function(start_time, end_time) 
{
  if (is.character(start_time))
    start_time = lubridate::ymd_hms(start_time)
  if (is.character(end_time))
    end_time = lubridate::ymd_hms(end_time)
  
  if (end_time < start_time)
    stop('end_time must be after start_time.')
  if (end_time > lubridate::now())
    stop('end_time must be in the past.')
  
  secrets = solaredge_secrets()
  url = stringr::str_glue('https://monitoringapi.solaredge.com/site/{secrets$site_id}/powerDetails')
  
  req = httr2::request(url) |> 
    httr2::req_url_query(startTime=format(start_time, "%Y-%m-%d %H:%M:%S"),
                  endTime=format(end_time, "%Y-%m-%d %H:%M:%S"),
                  meters='Production',
                  time_unit='QUARTER_OF_AN_HOUR',
                  api_key=secrets$key)
  resp = req |> 
    httr2::req_perform()

  raw = resp |> httr2::resp_body_string() |> jsonlite::fromJSON()
  unit = raw[['powerDetails']][['unit']]
  cooked = raw[['powerDetails']][["meters"]] |>
    tidyr::unnest(.data$values) |> 
    dplyr::mutate(date=lubridate::as_datetime(date), unit=unit)
  
  # The API does not always return new data even if end_time > start_time
  if (!'value' %in% names(cooked)) cooked$value = NA
  
  cooked = cooked |> 
    dplyr::filter(!is.na(.data$value)) |> 
    dplyr::select(-'type') |> 
    expand_date()
  
  cooked
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
      year=as.integer(lubridate::year(date)),
      month = as.integer(lubridate::month(date)),
      day = as.integer(lubridate::day(date)),
      time = hms::as_hms(date)
    )
}