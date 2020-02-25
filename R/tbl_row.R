#' S3 method to retrieve the number of rows
#'
#' @param con database connection obj or R table object
#' @param schema database schema
#' @param tbl database table
#'
#'
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
  as.numeric(DBI::dbGetQuery(con, q)$n_total)
}

tbl_row.MySQLConnection = tbl_row.SQLiteConnection
tbl_row.PqConnection = tbl_row.SQLiteConnection
tbl_row.PostgreSQLConnection = tbl_row.SQLiteConnection

# R object ----------------------------------------------------------------
tbl_row.data.table = function(con,
                              ...) {
  nrow(con)
}

tbl_row.data.frame = tbl_row.data.table
tbl_row.tibble = tbl_row.data.table

