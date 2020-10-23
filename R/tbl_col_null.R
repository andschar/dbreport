#' S3 method to get the amount of NULLs or NAs in each column
#'
#' @param con Database connection or R table object.
#' @param schema Database schema.
#' @param tbl Database table.
#' @param column Specific table column.
#' @param ... Currently not used.
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
tbl_col_null = function(...) {
  UseMethod('tbl_col_null')
}

tbl_col_null.SQLiteConnection = function(con,
                                         schema = NULL,
                                         tbl = NULL,
                                         column = NULL) {
  foo = function(con, schema, tbl, column) {
    # checking
    if (is.null(tbl)) {
      stop('No database table supplied.')
    }
    # build query
    if (is.null(schema)) {
      from = paste0("\nFROM ", tbl)
    } else {
      from = paste0("\nFROM ", schema, ".", tbl)
    }
    # sql
    q = paste0("SELECT count(\"",
               column,
               "\") AS n_null",
               from,
               "\nWHERE \"",
               column,
               "\" IS NULL;")
    # query
    lapply(q, DBI::dbGetQuery, con = con)[[1]]
  }
  
  # preparation
  out = mapply(
    FUN = foo,
    column = column,
    MoreArgs = list(
      con = con,
      schema = schema,
      tbl = tbl
    )
  )
  out = data.table::transpose(data.table::as.data.table(out))
  out[, cols := column]
  data.table::setnames(out, 'V1', 'n_null')
  data.table::setcolorder(out, 'cols')
  data.table::copy(out)
}

tbl_col_null.MySQLConnection = tbl_col_null.SQLiteConnection
tbl_col_null.PostgreSQLConnection = tbl_col_null.SQLiteConnection
tbl_col_null.PqConnection = tbl_col_null.SQLiteConnection

tbl_col_null.data.table = function(con,
                                   ...) {
  data.table::setDT(con)
  l = lapply(con, function(x)
    length(which(is.na(x))))
  null = data.table::as.data.table(data.table::transpose(l, keep.names = 'column'))
  data.table::setnames(null, c('cols', 'n_null'))
  null
}

tbl_col_null.data.frame = tbl_col_null.data.table
tbl_col_null.tibble = tbl_col_null.data.table
