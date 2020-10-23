#' S3 method to check whether provided table exists
#'
#' @param con Database connection obj or R table object.
#' @param schema Database schema.
#' @param tbl Database table.
#' @param ... Currently not used.
#'  
tbl_exists = function(...) {
  UseMethod('tbl_exists')
}

tbl_exists.SQLiteConnection = function(con,
                                       schema = NULL,
                                       tbl = NULL) {
  tbl = paste0(c(schema, tbl), collapse = '.')
  row = try(DBI::dbGetQuery(con, paste0("SELECT * FROM ", tbl, " LIMIT 1")))
  if (is.null(row) || nrow(row) != 1) {
    stop('Table does not exist: ', tbl)
  }
}

tbl_exists.MySQLConnection = tbl_exists.SQLiteConnection
tbl_exists.PqConnection = tbl_exists.SQLiteConnection
tbl_exists.PostgreSQLConnection = tbl_exists.SQLiteConnection

tbl_exists.data.table = function(con,
                                 schema = NULL,
                                 tbl = NULL) {
  if (!is.data.table(con)) {
    stop('Table does not exist: ', con)
  }
}
tbl_exists.data.frame = function(con,
                                 schema = NULL,
                                 tbl = NULL) {
  if (!is.data.frame(con)) {
    stop('Table does not exist: ', con)
  }
}
tbl_exists.tibble = function(con,
                             schema = NULL,
                             tbl = NULL) {
  if (!is_tibble(con)) {
    stop('Table does not exist: ', con)
  }
}
tbl_exists.list = function(con,
                           schema = NULL,
                           tbl = NULL) {
  stop('dbreport does not work on lists.')
}







