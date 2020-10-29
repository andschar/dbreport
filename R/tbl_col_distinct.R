#' S3 method to get distinct entries for every column
#'
#' @param con Database connection or R table object.
#' @param schema Database schema.
#' @param tbl Database table.
#' @param column Specific table column.
#' @param col_type Type of column.
#' @param ... Currently not used.
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
                                             col_type = NULL) {
  # helper functions
  # TODO put functions outside as own S3 class? 300ns to create..not slow
  f_distinct = function(con,
                        schema,
                        tbl,
                        column,
                        col_type) {
    q_l = list()
    for (i in seq_along(column)) {
      cl = column[i]
      typ = col_type[i]
      # query
      if (typ %in% c('char', 'date')) {
        select = paste0("SELECT ", sql_quote(con, cl), ", CAST(count(*) AS integer) AS n_distinct")
        # DROPS NULLs: count(", sql_quote(con, i), ")
        if (is.null(schema)) {
          from = paste0("FROM ", tbl)
        } else {
          from = paste0("FROM ", schema, ".", tbl)
        }
        groupby = paste0("GROUP BY ", sql_quote(con, cl))
        orderby = "ORDER BY 2 DESC" # NOTE ORDER BY INDEX b/c when column is named n "ORDER BY n" errors
        limit = "LIMIT 1e3" # NOTE hard-coded leveling off to avoid size explosions!
        q = trimws(paste(select,
                         from,
                         groupby,
                         orderby,
                         limit,
                         sep = '\n'))
        if (typ == 'char') {
          class(q) = c('character', 'char')
        }
        if (typ == 'date') {
          class(q) = c('character', 'date')
        }
      } else if (typ %in% c('nume', 'geom')) {
        select = paste0(# TODO make this an S3 method for other DBs and R objects
          "SELECT ", sql_quote(con, cl))
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
        if (typ == 'nume') {
          class(q) = c('character', 'nume')
        }
        if (typ == 'geom') {
          class(q) = c('character', 'geom')
        }
      } else {
        stop('Data type not supported. Please raise an issue here: https://github.com/andschar/dbreport')
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
    col_type = col_type
  )
  # TODO probably faster in a bulk query OR maybe BEGIN...COMMIT
  # TODO distinct_n is probably fast enough
  l_distinct = list()
  for (i in seq_along(l_q_distinct)) {
    q = l_q_distinct[[i]]
    nam = names(l_q_distinct)[i]
    l_distinct[[i]] = DBI::dbGetQuery(con, q)
    data.table::setDT(l_distinct[[i]])
    class(l_distinct[[i]]) = c('data.table', 'data.frame',
                               class(q)[class(q) != 'character']) # NOTE not very elegant ?
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
    dt = con[, .SD, .SDcols = cl][, .(n_distinct = .N), cl][order(-n_distinct)]
    if (names(cl) %in% c('numeric', 'integer')) {
      class(dt) = c('nume', 'data.table', 'data.frame')
    } else if (names(cl) == 'date') {
      class(dt) = c('date', 'data.table', 'data.frame')
    } else {
      class(dt) = c('char', 'data.table', 'data.frame')
    }
    l_distinct[[i]] = dt
    names(l_distinct)[i] = cl
  }
  
  l_distinct
}

tbl_col_distinct.data.frame = tbl_col_distinct.data.table
tbl_col_distinct.tibble = tbl_col_distinct.data.table
