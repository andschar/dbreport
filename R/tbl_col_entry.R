#' S3 method to get count of specific entries
#'
#' @param con Database connection or R table object
#' @param schema Database schema
#' @param tbl Database table
#' @param column Column
#' @param entry Entry to be looked for
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
tbl_col_entry = function(...) {
  UseMethod('tbl_col_entry')
}

tbl_col_entry.SQLiteConnection = function(con,
                                          schema = NULL,
                                          tbl = NULL,
                                          column = NULL,
                                          entry = NULL) {
  foo = function(con, schema, tbl, column, entry) {
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
    q = paste0(
      "SELECT COUNT(",
      sql_quote(con, column),
      ") AS n",
      from,
      "\nWHERE LOWER(CAST(",
      sql_quote(con, column),
      " AS CHAR(100))) = LOWER('",
      entry,
      "');"
    )
    # query
    lapply(q, DBI::dbGetQuery, con = con)[[1]]
  }
  
  # preparation
  entry_l = list()
  for (i in seq_along(entry)) {
    v = as.character(entry[i])
    dt = mapply(
      FUN = foo,
      column = column,
      entry = v,
      MoreArgs = list(
        con = con,
        schema = schema,
        tbl = tbl
      )
    )
    dt = data.table::transpose(data.table::as.data.table(dt))
    dt[, cols := column]
    data.table::setnames(dt, 'V1', paste0('n_', v))
    
    entry_l[[i]] = dt
    names(entry_l)[i] = paste0('n_', v)
  }
  
  Reduce(merge, entry_l)
}

tbl_col_entry.MySQLConnection = tbl_col_entry.SQLiteConnection
tbl_col_entry.PqConnection = tbl_col_entry.SQLiteConnection
tbl_col_entry.PostgreSQLConnection = tbl_col_entry.SQLiteConnection

tbl_col_entry.data.table = function(con,
                                    column,
                                    entry,
                                    ...) {
  data.table::setDT(con)
  if (!is.null(entry)) {
    l = list()
    for (i in seq_along(entry)) {
      v = entry[[i]]
      n = sapply(con, function(x)
        length(which(x == v)))
      dt = data.table::data.table(cols = names(n),
                                  V1 = n)
      data.table::setnames(dt, 'V1', paste0('n_', v))
      l[[i]] = dt
      names(l)[i] = paste0('n_', v)
    }
    out = Reduce(merge, l)
  }
  
  out
}

tbl_col_entry.data.frame = tbl_col_entry.data.table
tbl_col_entry.tibble = tbl_col_entry.data.table
