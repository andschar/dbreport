#' Function to retrieve meta data from databases or R-objects
#'
#' @param con Database connection or R table object
#' @param schema Database schema
#' @param tbl Database table
#' @param plot_type Plot type
#' @param plot_limit Plot limit
#' @param plot_limit_text Plot text limit
#' @param output_dir Output directory
#' @param output_file Output file
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
meta = function(...) {
  UseMethod("meta")
}

meta.SQLiteConnection = function(con = NULL,
                                 schema = NULL,
                                 tbl = NULL,
                                 plot_type = NULL,
                                 plot_limit = NULL,
                                 plot_limit_text = NULL,
                                 output_dir = NULL,
                                 output_file = NULL,
                                 ...) {
  connection = paste0(as.character(class(con)), collapse = ' ')
  db_info = DBI::dbGetInfo(con)
  # table
  out = data.table::data.table(
    connection = paste0(as.character(class(con)), collapse = ', '),
    host = db_info$host,
    user = db_info$user,
    database = db_info$dbname,
    schema = schema,
    table = tbl,
    `table rows` = tbl_row(
      con = con,
      schema = schema,
      tbl = tbl
    ),
    `table size` = paste0(tbl_size(
      con = con,
      schema = schema,
      tbl = tbl
    ), collapse = ' '),
    plot_type = plot_type,
    plot_limit = plot_limit,
    plot_limit_text = plot_limit_text,
    output_dir = output_dir,
    output_file = output_file
  )
  
  out = data.table::transpose(out, keep.names = 'variable')
  data.table::setnames(out, c('variable', 'value'))
  out
}

meta.MySQLConnection = meta.SQLiteConnection
meta.PqConnection = meta.SQLiteConnection
meta.PostgreSQLConnection = meta.SQLiteConnection

meta.data.table = function(con = NULL,
                           plot_type = NULL,
                           plot_limit = NULL,
                           plot_limit_text = NULL,
                           output_dir = NULL,
                           output_file = NULL,
                           ...) {
  out = data.table::data.table(
    table = tbl_name(con = con, schema = schem, tbl = tbl),
    `table type` = paste0(as.character(class(con)), collapse = ', '),
    `table rows` = tbl_row(con),
    `table size` = paste0(tbl_size(con, schema, tbl), collapse = ' '),
    plot_type = plot_type,
    plot_limit = plot_limit,
    plot_limit_text = plot_limit_text,
    output_dir = output_dir,
    output_file = output_file
  )
  out = data.table::transpose(out, keep.names = 'variable')
  data.table::setnames(out, c('variable', 'value'))
  out
}

meta.data.frame = meta.data.table
meta.tibble = meta.data.table
