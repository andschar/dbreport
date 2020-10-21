#' S3 method to retrieve name of table
#'
#' @param con database connection or R table object
#' @param schema database schema
#' @param tbl database table
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
tbl_name = function(...) {
  UseMethod('tbl_name')
}

# SQLite ------------------------------------------------------------------
tbl_name.PostgreSQLConnection = function(con,
                                         schema,
                                         tbl,
                                         ...) {
  paste0(schema, '.', tbl)
}

tbl_name.PqConnection = tbl_name.PostgreSQLConnection
tbl_name.SQLiteConnection = tbl_name.PostgreSQLConnection
tbl_name.MySQLConnection = tbl_name.PostgreSQLConnection

# R object ----------------------------------------------------------------
tbl_name.data.table = function(con,
                               ...) {
  paste0(con_deparse) # NOTE con_deparse is `<<-` in dbreport()
}

tbl_name.data.frame = tbl_name.data.table
tbl_name.tibble = tbl_name.data.table
