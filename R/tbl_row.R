# S3 method to retrieve the number of rows

tbl_row = function(...) {
  UseMethod('tbl_row')
}

# SQL ---------------------------------------------------------------------
tbl_row.SQLiteConnection = function(con,
                                    schema,
                                    tbl,
                                    ...) {
  q = paste0("SELECT count(1) AS n_total
              FROM ", paste0(c(schema, tbl), collapse = '.'), ";")
  unlist(DBI::dbGetQuery(con, q))
}

tbl_row.MySQLConnection = tbl_row.SQLiteConnection
tbl_row.PqConnection = tbl_row.SQLiteConnection
tbl_row.PostgreSQLConnection = tbl_row.SQLiteConnection

# R object ----------------------------------------------------------------
tbl_row.data.table = function(con,
                              ...) {
  norw(con)
}

tbl_row.data.frame = tbl_row.data.table
tbl_row.tibble = tbl_row.data.table

