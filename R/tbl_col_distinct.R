#' S3 method to get distinct entries for every column
#'
#' @param con database connection or R table object
#' @param schema database schema
#' @param tbl database table
#' @param column specific table column
#' @param col_numeric logical; whether a column is numeric or not
#' @param verbose verbose output
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
tbl_col_distinct = function(...) {
  UseMethod('tbl_col_distinct')
}

tbl_col_distinct.SQLiteConnection = function(con,
                                             schema = NULL,
                                             tbl = NULL,
                                             column = NULL,
                                             col_numeric = NULL,
                                             verbose = FALSE) {
  # helper functions
  # TODO put functions outside as own S3 class? 300ns to create..not slow
  f_distinct = function(con,
                        schema,
                        tbl,
                        column,
                        col_numeric) {
    q_l = list()
    for (i in seq_along(column)) {
      cl = column[i]
      num = col_numeric[i]
      # query
      if (!num) {
        select = paste0("SELECT ", sql_quote(con, cl), ", count(*) AS n_distinct") # DROPS NULLs: count(", sql_quote(con, i), ")
        if (is.null(schema)) {
          from = paste0("FROM ", tbl)
        } else {
          from = paste0("FROM ", schema, ".", tbl)
        }
        groupby = paste0("GROUP BY ", sql_quote(con, cl))
        orderby = "ORDER BY 2 DESC" # NOTE ORDER BY INDEX b/c when column is named n "ORDER BY n" errors
        limit = "LIMIT 1e4" # NOTE hard-coded leveling off to avoid size explosions!
        q = trimws(paste(select,
                         from,
                         groupby,
                         orderby,
                         limit,
                         sep = '\n'))
        class(q) = c('character', 'categorical')
      } else if (num) {
        select = paste0(# TODO make this an S3 method for other DBs and R objects
          "SELECT ", sql_quote(con, cl))
        
        # NOTE summary statistics (not easy to calculate density curve then)
        # select = paste0(
        #   "SELECT ",
        #   "min(",
        #   sql_quote(con, cl),
        #   ") AS min, ",
        #   "percentile_cont(0.25) within group (order by ",
        #   sql_quote(con, cl),
        #   ") AS p25, ",
        #   "avg(",
        #   sql_quote(con, cl),
        #   ") AS men, ",
        #   "percentile_cont(0.5) within group (order by ",
        #   sql_quote(con, cl),
        #   ") AS med, ",
        #   "percentile_cont(0.75) within group (order by ",
        #   sql_quote(con, cl),
        #   ") AS p75, ",
        #   "max(",
        #   sql_quote(con, cl),
        #   ") AS max"
        # )
        if (is.null(schema)) {
          from = paste0("FROM ", tbl)
        } else {
          from = paste0("FROM ", schema, ".", tbl)
        }
        extra = paste0("TABLESAMPLE BERNOULLI (10) REPEATABLE (1234);")
        q = trimws(paste(select,
                         from,
                         extra,
                         sep = '\n'))
        class(q) = c('character', 'continuous')
      } else {
        stop('Numeric column is not logical.')
      }
      q_l[[i]] = q
      names(q_l)[i] = cl
    }
    
    q_l
  }
  # query lists
  l_q_distinct = f_distinct(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column,
    col_numeric = col_numeric
  )
  # TODO probably faster in a bulk query OR maybe BEGIN...COMMIT
  # TODO distinct_n is probably fast enough
  l_distinct = list()
  for (i in seq_along(l_q_distinct)) {
    q = l_q_distinct[[i]]
    nam = names(l_q_distinct)[i]
    if (verbose)
      message('Fetching: ', nam)
    l_distinct[[i]] = DBI::dbGetQuery(con, q)
    data.table::setDT(l_distinct[[i]])
    class(l_distinct[[i]]) = c('data.table', 'data.frame', class(q)[class(q) != 'character']) # NOTE not very elegant ?
    names(l_distinct)[i] = nam
  }
  
  l_distinct
}

tbl_col_distinct.MySQLConnection = tbl_col_distinct.SQLiteConnection
tbl_col_distinct.PqConnection = tbl_col_distinct.SQLiteConnection
tbl_col_distinct.PostgreSQLConnection = tbl_col_distinct.SQLiteConnection

tbl_col_distinct.data.table = function(con,
                                       ...) {
  data.table::setDT(con)
  l_distinct = list()
  todo = names(con)
  names(todo) = sapply(con, class)
  for (i in seq_along(todo)) {
    cl = todo[i]
    if (names(cl) %in% c('numeric', 'integer')) {
      dt = con[ , .SD, .SDcols = cl ][ , .(n_distinct = .N), cl ][ order(-n_distinct) ]
      class(dt) = c('continuous', 'data.table', 'data.frame')
    } else {
      dt = con[ , .SD, .SDcols = cl ][ , .(n_distinct = .N), cl ][ order(-n_distinct) ]
      class(dt) = c('categorical', 'data.table', 'data.frame')
    }
    l_distinct[[i]] = dt
    names(l_distinct)[i] = cl
  }
  
  l_distinct
}

tbl_col_distinct.data.frame = tbl_col_distinct.data.table
tbl_col_distinct.tibble = tbl_col_distinct.data.table
