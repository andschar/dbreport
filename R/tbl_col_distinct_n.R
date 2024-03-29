#' S3 method to get counts of distinct entries for every column
#'
#' @param con Database connection or R table object.
#' @param schema Database schema.
#' @param tbl Database table.
#' @param column Specific table column.
#' @param ... Currently not used.
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
tbl_col_distinct_n = function(...) {
  UseMethod('tbl_col_distinct_n')
}

tbl_col_distinct_n.SQLiteConnection = function(con,
                                               schema = NULL,
                                               tbl = NULL,
                                               column = NULL) {
  # helper function
  f_distinct_n = function(con,
                          schema,
                          tbl,
                          column) {
    # NOTE Assume that this works for all SQL software, otherwise make own S3 method
    q_l = list()
    for (i in seq_along(column)) {
      cl = column[i]
      # query
      select = paste0("SELECT '", cl, "' AS cols, COUNT(*) AS distinct ")
      if (is.null(schema)) {
        from = paste0("FROM (SELECT DISTINCT ",
                      sql_quote(con, cl),
                      " FROM ",
                      tbl,
                      ") AS tmp;")
      } else {
        from = paste0(
          "FROM (SELECT DISTINCT ",
          sql_quote(con, cl),
          " FROM ",
          paste0(schema, ".", tbl),
          ") AS tmp;"
        )
      }
      q = trimws(paste(select,
                       from,
                       sep = '\n'))
      q_l[[i]] = q
      names(q_l)[i] = cl
    }
    
    q_l
  }
  # query list
  l_q_distinct_n = f_distinct_n(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column
  )
  # query
  l_distinct_n = list()
  for (i in seq_along(l_q_distinct_n)) {
    q = l_q_distinct_n[[i]]
    nam = names(l_q_distinct_n)[i]
    l_distinct_n[[i]] = DBI::dbGetQuery(con, q)
    data.table::setDT(l_distinct_n[[i]])
    # l_distinct_n[[i]][ , distinct := as.numeric(distinct) ]
    names(l_distinct_n)[i] = nam
  }
  
  data.table::rbindlist(l_distinct_n)
}

tbl_col_distinct_n.MySQLConnection = tbl_col_distinct_n.SQLiteConnection
tbl_col_distinct_n.PqConnection = tbl_col_distinct_n.SQLiteConnection
tbl_col_distinct_n.PostgreSQLConnection = tbl_col_distinct_n.SQLiteConnection

tbl_col_distinct_n.data.table = function(con,
                                         ...) {
  data.table::setDT(con)
  dt = data.table::as.data.table(sapply(con, data.table::uniqueN),
                                 keep.rownames = TRUE)
  data.table::setnames(dt, c('cols', 'distinct'))
  
  dt
}

tbl_col_distinct_n.data.frame = tbl_col_distinct_n.data.table
tbl_col_distinct_n.tibble = tbl_col_distinct_n.data.table
