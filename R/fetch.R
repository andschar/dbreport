#' function to fetch data from data base
fetch = function(con = NULL,
                 schema = NULL, 
                 tbl = NULL,
                 cols = NULL,
                 var = NULL,
                 limit = NULL,
                 verbose = FALSE,
                 ...) {
  # checking
  if (is.null(con))
    stop('No data base connection or R-object supplied.')
  # summary stats
  type = tbl_col_type(con = con, schema = schema, tbl = tbl)
  if (is.null(cols))
    cols = type$cols
  dist_l = tbl_col_distinct(con = con, schema = schema, tbl = tbl, cols = cols, limit = limit, verbose = verbose)
  dist_n = dist_l$distinct_n
  null = tbl_col_null(con = con, schema = schema, tbl = tbl, col = cols)
  if (!is.null(var))
    var = tbl_col_entry(con = con, schema = schema, tbl = tbl, col = cols, var = var)
  example = examples_n(dist_l$distinct, n = 3)
  l = list(type = type,
           dist_n = dist_n,
           null = null,
           variable = var,
           example = example)
  l = l[ !sapply(l, is.null) ]
  summary = Reduce(function(...) merge(..., by = 'cols'), l)
  # final object
  out_l = list(summary = summary,
               distinct = dist_l$distinct)
}
