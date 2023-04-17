# SQLite does not have a native date type!
# DuckDB doesn't seem to release the database lock when a connection
# is closed with dbDisconnect(con)

# Some tests
# This leaves a lock
  withr::with_db_connection(
    list(con = get_connection()),
    { })
    
# This does not leave a lock
con = get_connection()
DBI::dbDisconnect(con, shutdown=TRUE)

local_db = function(envir = parent.frame()) {
  con = get_connection()
  defer(DBI::dbDisconnect(con, shutdown=TRUE), envir=envir)
  con
}

# Also good
test_local_db = function() {
  con = local_db()
}

power = power_table()
power |> summarize(.by=c(year, month, day), total=sum(value)) |> collect()

withr::deferred_run()
