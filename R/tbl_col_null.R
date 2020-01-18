#' S3 method to get the amount of NULLs or NAs in each column
#'
#' @param con database connection obj or R table object
#' @param schema database schema
#' @param tbl database table
#' @param col specific table column
#'  
tbl_col_null = function(...) {
  UseMethod('tbl_col_null')
}

tbl_col_null.SQLiteConnection = function(con,
                                         schema = NULL,
                                         tbl = NULL,
                                         col = NULL) {
  
  foo = function(con, schema, tbl, col) {
    # checking
    if (is.null(tbl)) {
      stop('No data base table supplied.')
    }
    # build query
    if (is.null(schema)) {
      from = paste0("\nFROM ", tbl)
    } else {
      from = paste0("\nFROM ", schema, ".", tbl)
    }
    # sql
    q = paste0("SELECT count(\"", col, "\") AS n_null",
               from,
               "\nWHERE \"", col, "\" IS NULL;")
    # query
    lapply(q, DBI::dbGetQuery, con = con)[[1]]
  }
  
  # preparation
  out = mapply(FUN = foo,
               col = col,
               MoreArgs = list(con = con,
                               schema = schema,
                               tbl = tbl))
  out = transpose(as.data.table(out))
  out[ , cols := col ]
  setnames(out, 'V1', 'n_null')
  setcolorder(out, 'cols')
  
  return(copy(out))
}

tbl_col_null.MySQLConnection = tbl_col_null.SQLiteConnection
tbl_col_null.PostgreSQLConnection = tbl_col_null.SQLiteConnection
tbl_col_null.PqConnection = tbl_col_null.SQLiteConnection

tbl_col_null.data.table = function(con,
                                   ...) {
  setDT(con)
  l = lapply(con, function(x) length(which(is.na(x))))
  null = as.data.table(transpose(l, keep.names = 'column'))
  setnames(null, c('cols', 'n_null'))
  null
}

tbl_col_null.data.frame = tbl_col_null.data.table
tbl_col_null.tibble = tbl_col_null.data.table






