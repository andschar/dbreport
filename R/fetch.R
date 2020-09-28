#' Function to retrieve data from databases or R-objects
#'
#' @param con Database connection or R table object
#' @param schema Database schema
#' @param tbl Database table
#' @param column Specific table column
#' @param entry Entry to be looked for
#' @param verbose Verbose output?
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
fetch = function(con = NULL,
                 schema = NULL,
                 tbl = NULL,
                 column = NULL,
                 entry = NULL,
                 verbose = FALSE,
                 ...) {
  # checking
  if (is.null(con)) {
    stop('No data base connection or R-object supplied.')
  }
  # summary stats
  type = tbl_col_type(con = con,
                      schema = schema,
                      tbl = tbl)
  if (is.null(column)) {
    column = type$cols
  }
  # TODO add missing types
  cha = c('text', 'varchar')
  num = c('integer', 'bigint', 'numeric', 'double precision')
  type[, numeric := data.table::fcase(type %in% cha, FALSE,
                                      type %in% num, TRUE)]
  distinct_l = tbl_col_distinct(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column,
    col_numeric = type$numeric,
    verbose = verbose
  )
  distinct_n = tbl_col_distinct_n(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column
  )
  null = tbl_col_null(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column
  )
  if (!is.null(entry)) {
    entry = tbl_col_entry(
      con = con,
      schema = schema,
      tbl = tbl,
      column = column,
      entry = entry
    )
  }
  example = examples_n(distinct_l, n = 3)
  l = list(
    type = type,
    distinct_n = distinct_n,
    null = null,
    entry = entry,
    example = example
  )
  l = l[!sapply(l, is.null)]
  summary = Reduce(function(...)
    merge(..., by = 'cols'), l)
  # final object
  out_l = list(summary = summary,
               distinct = distinct_l)
}
