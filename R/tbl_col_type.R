#' S3 method to retrieve the column types of a table
#'
#' @param con database connection obj or R table object
#' @param schema database schema
#' @param tbl database table
#'
tbl_col_type = function(...) {
  UseMethod('tbl_col_type')
}

# SQL ---------------------------------------------------------------------
tbl_col_type.SQLiteConnection = function(con,
                                         schema,
                                         tbl,
                                         ...) {
  q1 = paste0("PRAGMA table_info(", tbl, ")")
  type = DBI::dbGetQuery(con, q1)
  setDT(type)
  setnames(type, 'name', 'cols')
  type[ , .SD, .SDcols = c('cols', 'type') ]
}

tbl_col_type.PqConnection = function(con,
                                     schema,
                                     tbl,
                                     ...) {
  q1 = paste0("SELECT column_name AS cols, data_type AS type
               FROM information_schema.columns
               WHERE TABLE_NAME = '", tbl, "' AND TABLE_SCHEMA = '", schema, "';")
  data.table(DBI::dbGetQuery(con, q1))
}

tbl_col_type.PostgreSQLConnection = tbl_col_type.PqConnection

tbl_col_type.MySQLConnection = function(con,
                                        schema,
                                        tbl,
                                        ...) {
  q1 = paste0("SELECT column_name, data_type
               FROM information_schema.columns
               WHERE TABLE_NAME = '", tbl, "' AND TABLE_SCHEMA = '", schema, "';")
  type = data.table(DBI::dbGetQuery(con, q1))
  setnames(type, c('cols', 'type')) # NOTE "AS column, DATA_TYPE AS type" throws an error
  type
}

# R object ----------------------------------------------------------------
tbl_col_type.data.table = function(con,
                                   schema,
                                   tbl,
                                   ...) {
  setDT(con)
  l = lapply(lapply(con, class), paste0, collapse = ' ') # NOTE paste0 for entries such as "ordered factor"
  type = as.data.table(transpose(l, keep.names = 'column'))
  setnames(type, c('cols', 'type'))
  type
}

tbl_col_type.data.frame = tbl_col_type.data.table
tbl_col_type.tibble = tbl_col_type.data.table

