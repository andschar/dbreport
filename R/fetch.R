#' Function to retrieve data from databases or R-objects
#'
#' @param con Database connection or R table object.
#' @param schema Database schema.
#' @param tbl Database table.
#' @param column Specific table column.
#' @param entry Entry to be looked for.
#' @param verbose Verbose output?
#' @param ... Currently not used.
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
    stop('No database connection or R-object supplied.')
  }
  # summary stats
  verbose_message(verbose, '..Fetching: Column type.')
  type = tbl_col_type(
    con = con,
    schema = schema,
    tbl = tbl
  )
  if (is.null(column)) {
    column = type$cols
  }
  # data type variables
  # TODO add missing types
  char = c('boolean', 'text', 'varchar')
  nume = c('integer', 'bigint', 'numeric', 'double precision')
  date = 'date'
  geom = c('geometry', 'geography')
  # type[, numeric := data.table::fcase(type %in% char, FALSE,
  #                                     type %in% nume, TRUE,
  #                                     default = FALSE)]
  type[, type_conv := data.table::fcase(type %in% char, 'char',
                                        type %in% nume, 'nume',
                                        type %in% date, 'date',
                                        type %in% geom, 'geom',
                                        default = NA_character_)]
  verbose_message(verbose, '..Fetching: Distinct columns.')
  distinct_l = tbl_col_distinct(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column,
    col_type = type$type_conv
  )
  verbose_message(verbose, '..Fetching: Distinct column counts.')
  distinct_n = tbl_col_distinct_n(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column
  )
  verbose_message(verbose, '..Fetching: NULLs.')
  null = tbl_col_null(
    con = con,
    schema = schema,
    tbl = tbl,
    column = column
  )
  if (!is.null(entry)) {
    verbose_message(verbose, '..Fetching: Specific entries.')
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
    type = type[ , .SD, .SDcols =! 'type_conv' ],
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
