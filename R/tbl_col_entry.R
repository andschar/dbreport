# S3 method to get count of specific entries

tbl_col_entry = function(...) {
  UseMethod('tbl_col_entry')
}

tbl_col_entry.SQLiteConnection = function(con,
                                          schema = NULL,
                                          tbl = NULL,
                                          col = NULL,
                                          var = NULL) {
  
  foo = function(con, schema, tbl, col, var) {
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
    q = paste0("SELECT COUNT(", sql_quote(con, col), ") AS n",
               from,
               "\nWHERE LOWER(CAST(", sql_quote(con, col), " AS CHAR(100))) = LOWER('", var, "');")
    # query
    lapply(q, DBI::dbGetQuery, con = con)[[1]]
  }
  
  # preparation
  var_l = list()
  for (i in seq_along(var)) {
    v = as.character(var[i])
    dt = mapply(FUN = foo,
                col = col,
                var = v,
                MoreArgs = list(con = con,
                                schema = schema,
                                tbl = tbl))
    dt = transpose(as.data.table(dt))
    dt[ , cols := col ]
    setnames(dt, 'V1', paste0('n_', v))
    
    var_l[[i]] = dt
    names(var_l)[i] = paste0('n_', v)
  }
  out = Reduce(merge, var_l)
  
  return(out)
}

tbl_col_entry.MySQLConnection = tbl_col_entry.SQLiteConnection
tbl_col_entry.PqConnection = tbl_col_entry.SQLiteConnection
tbl_col_entry.PostgreSQLConnection = tbl_col_entry.SQLiteConnection

tbl_col_entry.data.table = function(con,
                                    col,
                                    var,
                                    ...) {
  setDT(con)
  
  if (!is.null(var)) {
    l = list()
    for (i in seq_along(var)) {
      v = var[[i]]
      n = sapply(con, function(x) length(which(x == v)))
      dt = data.table(cols = names(n),
                      V1 = n)
      setnames(dt, 'V1', paste0('n_', v))
      l[[i]] = dt
      names(l)[i] = paste0('n_', v)
    }
    out = Reduce(merge, l)
  }
  
  return(out)
}

tbl_col_entry.data.frame = tbl_col_entry.data.table
tbl_col_entry.tibble = tbl_col_entry.data.table






