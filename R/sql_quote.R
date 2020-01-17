# S3 method to retrieve the number of rows

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