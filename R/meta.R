#' function to retrieve meta data from databases R-objects or files
#' @export
#'
meta = function(...) {
  UseMethod("meta")
}

# databases ---------------------------------------------------------------
meta.SQLiteConnection = function(con = NULL,
                                 schema = NULL,
                                 tbl = NULL,
                                 output_dir = NULL,
                                 output_file = NULL,
                                 ...) {
  
  connection = paste0(as.character(class(con)), collapse = ' ')
  db_info = DBI::dbGetInfo(con)
  # table
  out = data.table(connection = paste0(as.character(class(con)), collapse = ', '),
                   host = db_info$host,
                   user = db_info$user,
                   database = db_info$dbname,
                   schema = schema,
                   table = tbl,
                   `table rows` = tbl_row(con = con, schema = schema, tbl = tbl),
                   `table size` = paste0(tbl_size(con = con, schema = schema, tbl = tbl), collapse = ' '),
                   output_dir = output_dir,
                   output_file = output_file)
  
  out = transpose(out, keep.names = 'variable')
  setnames(out, c('variable', 'value'))
  out
}

meta.MySQLConnection = meta.SQLiteConnection
meta.PqConnection = meta.SQLiteConnection
meta.PostgreSQLConnection = meta.SQLiteConnection

# R objects ---------------------------------------------------------------
meta.data.table = function(con = NULL,
                           output_dir = NULL,
                           output_file = NULL,
                           ...) {
  out = data.table(object = deparse(substitute(con)),
                   `object type` = paste0(as.character(class(con)), collapse = ', '),
                   `object rows` = nrow(con),
                   `object size` = paste0(tbl_size(con, schema, tbl), collapse = ' '),
                   output_dir = output_dir,
                   output_file = output_file)
  out = transpose(out, keep.names = 'variable')
  setnames(out, c('variable', 'value'))
  out
}

meta.data.frame = meta.data.table
meta.tibble = meta.data.table

# disk files --------------------------------------------------------------
meta.character = function(con = NULL,
                          output_dir = NULL,
                          output_file = NULL,
                          ...) {
  out = data.table(file = file.path(con),
                   `file size` = paste0(tbl_size(con, schema, tbl), collapse = ' '),
                   output_dir = output_dir,
                   output_file = output_file)
  out = transpose(out, keep.names = 'variable')
  setnames(out, c('variable', 'value'))
  out
}
