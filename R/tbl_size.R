#' S3 method to retrieve size of table
#'
#' @param con database connection or R table object
#' @param schema database schema
#' @param tbl database table
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
tbl_size = function(...) {
  UseMethod('tbl_size')
}

# SQLite ------------------------------------------------------------------
tbl_size.SQLiteConnection = function(con,
                                     schema,
                                     tbl,
                                     ...) {
  NULL
}

# MySQL -------------------------------------------------------------------
tbl_size.MySQLConnection = function(con,
                                    schema,
                                    tbl,
                                    ...) {
  q = paste0(
    "SELECT data_length + index_length AS table_size
              FROM information_schema.tables
              WHERE table_schema = '",
    schema,
    "'
              AND table_name = '",
    tbl,
    "';"
  )
  size = DBI::dbGetQuery(con, q)
  out = conv_byte(size)
  out
}

# Postgres ----------------------------------------------------------------
tbl_size.PostgreSQLConnection = function(con,
                                         schema,
                                         tbl,
                                         ...) {
  q = paste0("SELECT pg_total_relation_size('", schema, ".", tbl, "');")
  size = DBI::dbGetQuery(con, q)
  out = conv_byte(size)
  out
}
tbl_size.PqConnection = tbl_size.PostgreSQLConnection

# R object ----------------------------------------------------------------
tbl_size.data.table = function(con,
                               ...) {
  out = conv_byte(data.table::data.table(as.numeric(object.size(con))))
  out
}

tbl_size.data.frame = tbl_size.data.table
tbl_size.tibble = tbl_size.data.table
