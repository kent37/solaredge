# Database functions

#' One-time database initialization
#' @param start_date Earliest date to get data from the API
init_db = function(start_date) {
  con = local_db()
  cat('Creating Dates table\n')
  if (!DBI::dbExistsTable(con, 'Dates')) {
    start_date = make_date(start_date)
    initial_dates = tibble::tribble(
      ~name, ~date,
      'start', start_date,
      'last_energy', start_date,
      'last_power', start_date
    )
    
    dplyr::copy_to(con, initial_dates, name='Dates', 
                   temporary=FALSE, overwrite=TRUE)
  }
  
  # Energy table is daily watt-hour production, one entry per day
  if (!DBI::dbExistsTable(con, 'Energy')) {
    cat('Creating Energy table\n')
    DBI::dbCreateTable(con, 'Energy',
                       fields = c(date='DATE', value='INTEGER', unit='VARCHAR',
                                  year='INTEGER', month='INTEGER', day='INTEGER',
                                  time='TIME'))
  }
  
  # Power table is 'instantaneous' watts, one entry per 15 minutes
  if (!DBI::dbExistsTable(con, 'Power')) {
    cat('Creating Power table\n')
    DBI::dbCreateTable(con, 'Power',
                       fields = c(date='TIMESTAMP', value='INTEGER', 
                                  unit='VARCHAR',
                                  year='INTEGER', month='INTEGER', day='INTEGER',
                                  time='TIME'))
  }
}

#' Get a `dplyr` table backed by the Energy table
#' @returns A table
energy_table = function(envir=parent.frame()) {
  con = local_db(envir)
  dplyr::tbl(con, 'Energy')
}

#' Get a `dplyr` table backed by the Power table
#' @returns A table
power_table = function(envir=parent.frame()) {
  con = local_db(envir)
  dplyr::tbl(con, 'Power')
}

#' Update the energy table with the latest
#' @returns A data frame with columns `date`, `value`, `unit`,
#' `year`, `month`, `day`, `time`
update_energy = function() {
  con = local_db()
  last_date = DBI::dbGetQuery(con, 
                "select date from Dates where name='last_energy'")[1, 1]
  
  last_entry = DBI::dbGetQuery(con, 
                "select max(date) from Energy")[1, 1]
  
  end_date = lubridate::today()-1
  if (end_date <= last_date) {
    cat('No new energy dates to fetch.\n')
    return()
  } else {
    cat('Fetching energy data for', format(last_date), 
        'to', format(end_date), '\n')
  }
  
  new_energy = get_daily_energy(last_date, end_date)
  
  # If there are already entries on the last_date, filter them
  if (!is.na(last_entry)) {
    new_energy = new_energy |> 
      dplyr::filter(date > last_entry)
  }
  
  if (nrow(new_energy)==0) {
    cat('No new data.\n')
    return()
  } else {
    cat('Received', nrow(new_energy), 'new entries.\n')
  }
  
  DBI::dbWriteTable(con, name='Energy', value=new_energy,
                   append=TRUE, overwrite=FALSE)
  
  # Update last_date
  last_date = as.Date(max(new_energy$date))
  DBI::dbExecute(con, 
                 "update Dates set date=? where name='last_energy'", 
                 list(last_date))
}

#' Update the power table with the latest at 15-minute intervals
#' @returns A data frame with columns `date`, `value`, `unit`,
#' `year`, `month`, `day`, `time`
update_power = function() {
  con = local_db()
  last_date = DBI::dbGetQuery(con, 
                "select date from Dates where name='last_power'")[1, 1]
  
  last_entry = DBI::dbGetQuery(con, 
                "select max(date) from Power")[1, 1]
  start_time = dplyr::if_else(is.na(last_entry), last_date, last_entry)
  
  end_time = lubridate::floor_date(lubridate::now(), 
                                   unit = "15 mins")
  
  if (end_time <= start_time) {
    cat('No new power entries to fetch.\n')
    return()
  } else {
    cat('Fetching power data for', format(start_time), 
        'to', format(end_time), '\n')
  }
  
  new_power = get_power(start_time, end_time)
  
  # If there are already entries on the last_date, filter them
  if (!is.na(last_entry)) {
    new_power = new_power |> 
      dplyr::filter(date > last_entry)
  }
  
  if (nrow(new_power)==0) {
    cat('No new data.\n')
    return()
  } else {
    cat('Received', nrow(new_power), 'new entries.\n')
  }
  
  DBI::dbWriteTable(con, name='Power', value=new_power,
                   append=TRUE, overwrite=FALSE)
  
  # Update last_date
  last_date = as.Date(max(new_power$date))
  DBI::dbExecute(con, "update Dates set date=? where name='last_power'", 
                 list(last_date))
}

#' Get a database connection that will be correctly shut down
#' when it goes out of scope in the provided environment
#' @param envir Environment to monitor for scope
local_db = function(envir = parent.frame()) {
  con = get_connection()
  withr::defer(DBI::dbDisconnect(con, shutdown=TRUE), envir=envir)
  con
}

get_connection = function() {
  # dbdir = 'misc/solaredge.sqlite'
  # DBI::dbConnect(RSQLite::SQLite(), dbdir)
  dbdir = here::here('misc/solaredge.duckdb')
  DBI::dbConnect(duckdb::duckdb(), dbdir, read_only=FALSE)
}