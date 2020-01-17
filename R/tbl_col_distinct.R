# S3 method to get distinct entries for every column

tbl_col_distinct = function(...) {
  UseMethod('tbl_col_distinct')
}

tbl_col_distinct.SQLiteConnection = function(con,
                                             schema = NULL,
                                             tbl = NULL,
                                             cols = NULL,
                                             limit = NULL,
                                             verbose = FALSE) {
  
  foo = function(con, schema, tbl, cols, limit) {
    q_l = list()
    for (i in cols) {
      select = paste0("SELECT ", sql_quote(con, i), ", count(", sql_quote(con, i), ") AS n")
      if (is.null(schema)) {
        from = paste0("FROM ", tbl)
      } else {
        from = paste0("FROM ", schema, ".", tbl)
      }
      groupby = paste0("GROUP BY ", sql_quote(con, i))
      orderby = paste0("ORDER BY n DESC")
      if (!is.null(limit) & is.numeric(limit)) {
        limit = paste0("LIMIT ", as.integer(limit))
      }
      q = trimws(paste(select,
                       from,
                       groupby,
                       orderby,
                       limit,
                       sep = '\n'))
      q_l[[i]] = q
    }
    
    return(q_l)
  }
  q_l = foo(con = con,
            schema = schema,
            tbl = tbl,
            cols = cols,
            limit = limit)
  l = list()
  for (i in seq_along(q_l)) {
    q = q_l[[i]]
    nam = names(q_l)[i]
    if (verbose)
      message('Fetching: ', nam)
    l[[i]] = DBI::dbGetQuery(con, q)
    names(l)[i] = nam
  }
  # count distinct entries
  distinct_n = lapply(l, nrow)
  distinct_n = transpose(as.data.table(distinct_n))
  distinct_n[ , cols := names(l) ]
  setnames(distinct_n, 'V1', 'distinct')
  setcolorder(distinct_n, 'cols')
  # out
  out_l = list(distinct = l,
               distinct_n = distinct_n)
  
  return(out_l)
}

tbl_col_distinct.MySQLConnection = tbl_col_distinct.SQLiteConnection
tbl_col_distinct.PqConnection = tbl_col_distinct.SQLiteConnection
tbl_col_distinct.PostgreSQLConnection = tbl_col_distinct.SQLiteConnection

tbl_col_distinct.data.table = function(con,
                                       ...) {
  setDT(con)
  l = list()
  for (i in names(con)) {
    dt = con[ , .(n = .N), i ][ order(-n) ]
    l[[i]] = dt
  }
  distinct_n = lapply(con, uniqueN)
  distinct_n = transpose(as.data.table(distinct_n), keep.names = 'cols')
  setnames(distinct_n, c('cols', 'distinct'))
  setcolorder(distinct_n, 'cols')
  # out
  out_l = list(distinct = l,
               distinct_n = distinct_n)
  
  return(out_l)
}

tbl_col_distinct.data.frame = tbl_col_distinct.data.table
tbl_col_distinct.tibble = tbl_col_distinct.data.table





