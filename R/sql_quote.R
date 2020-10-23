#' S3 method to escape SQL variables accordingly
#'
#' @param con Database connection or R table object.
#' @param x A string to quote.
#' @param ... Currently not used.
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' 
sql_quote = function(...) {
  UseMethod('sql_quote')
}

sql_quote.SQLiteConnection = function(con, x) {
  paste0('"', x, '"')
}

sql_quote.MySQLConnection = function(con, x) {
  paste0('`', x, '`')
}

sql_quote.PqConnection = function(con, x) {
  paste0('"', x, '"')
}

sql_quote.PostgreSQLConnection = sql_quote.PqConnection
