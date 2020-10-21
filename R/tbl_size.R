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
  # TODO warning?
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
  size = DBI::dbGetQuery(con, q)
  size = structure(as.numeric(size[1,]), class = 'object_size')
  conv_byte(size)
}

# Postgres ----------------------------------------------------------------
tbl_size.PostgreSQLConnection = function(con,
                                         schema,
                                         tbl,
                                         ...) {
  q = paste0("SELECT pg_total_relation_size('", schema, ".", tbl, "');")
  size = DBI::dbGetQuery(con, q)
  size = structure(as.numeric(size[1,]), class = 'object_size')
  conv_byte(size)
}
tbl_size.PqConnection = tbl_size.PostgreSQLConnection

# R object ----------------------------------------------------------------
tbl_size.data.table = function(con,
                               ...) {
  conv_byte(object.size(con))
}

tbl_size.data.frame = tbl_size.data.table
tbl_size.tibble = tbl_size.data.table
