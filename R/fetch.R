#' function to retrieve data from databases or R-objects
#' 
#' @param con Database connection obj or R table object
#' @param schema Database schema
#' @param tbl Database table
#' @param col Specific table column
#' @param entry Entry to be looked for
#' @param verbose Verbose output
#' 
fetch = function(con = NULL,
                 schema = NULL, 
                 tbl = NULL,
                 col = NULL,
                 entry = NULL,
                 verbose = FALSE,
                 ...) {
  # checking
  if (is.null(con))
    stop('No data base connection or R-object supplied.')
  # summary stats
  type = tbl_col_type(con = con, schema = schema, tbl = tbl)
  if (is.null(col))
    col = type$col
  dist_l = tbl_col_distinct(con = con, schema = schema, tbl = tbl, col = col, verbose = verbose)
  dist_n = dist_l$distinct_n
  null = tbl_col_null(con = con, schema = schema, tbl = tbl, col = col)
  if (!is.null(entry))
    entry = tbl_col_entry(con = con, schema = schema, tbl = tbl, col = col, entry = entry)
  example = examples_n(dist_l$distinct, n = 3)
  l = list(type = type,
           dist_n = dist_n,
           null = null,
           entry = entry,
           example = example)
  l = l[ !sapply(l, is.null) ]
  summary = Reduce(function(...) merge(..., by = 'cols'), l)
  # final object
  out_l = list(summary = summary,
               distinct = dist_l$distinct)
}
