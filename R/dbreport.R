#' Create reports from database tables, R table objects (data.table, data.frame, tibble)
#'
#' @title Create reports of a table object
#' 
#' @details Columns with more than 10,000 individual entries are leveled off to
#' avoid a too large RAM and storage footprint.
#' 
#' @import data.table rmarkdown knitr ggplot2
#' 
#' @param con Connection to database or an R-object (data.table, data.frame, tibble). Required.
#' @param schema character; Database schema (if applicable).
#' @param tbl character; Table to be used for summary report (if applicable).
#' @param column character; Table columns that should be summarised in report.
#' @param entry any; Specific table entries which should be comsidered in the summary table.
#' @param report_title character; Title of summary report.
#' @param report_text character or connection; User defined text input to add some description to the report. Default NULL - no text. Can be a character vector or a file.
#' @param plot_distinct logical; Include plots of ordered distinct counts of each column entry in the report?
#' @param plot_type character; Should a lollipop, barplot or treemap plot be produced?
#' @param plot_limit integer; Limit distinct count axis in the plots. Default: 40L.
#' @param plot_limit_text integer; Limit axis text to a certain length. Default: NULL (i.e. no limit is applied).
#' @param output_dir character; To which directory should the output be saved?
#' @param output_file character; File name of the report.
#' @param output_format object; directly passed on to rmarkdown::render(). Default: html_document(theme = 'united', highlight = 'tango').
#' @param file logical; Should one or multiple files for each distinct column counts be created?
#' @param file_format character; To which format should the file(s) be put out (csv, json). Multiple possible.
#' @param file_type character; Output each column distinct counts set to a single or to multiple files.
#' @param exit logical; Should database connections be closed after running dbreport(). Default: TRUE.
#' @param verbose logical; Should a verbose output be printed to console?
#' 
#' @return The function creates a markdown report and exports files 
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' @export
#' @examples
#' \donttest{
#' # creates a .html, .pdf, .doc via Rmarkdown
#' dbreport(
#'   con = iris,
#'   report_title = 'My iris report',
#'   report_text = 'Here goes my text as an introduction to the report.',
#'   plot_distinct = TRUE,
#'   plot_type = 'lollipop',
#'   plot_limit = 10, # Limit your plot to the first 15 entries
#'   output_dir = file.path(tempdir(), 'iris'),
#'   output_file = 'test',
#'   file = TRUE, # Output single distinct counts to a file
#'   file_type = 'multiple',
#'   file_format = 'csv'
#' )
#' }
dbreport = function(con = NULL,
                    schema = NULL,
                    tbl = NULL,
                    column = NULL,
                    entry = NULL,
                    report_title = NULL,
                    report_text = NULL,
                    plot_distinct = TRUE,
                    plot_type = c('lollipop', 'bar_horiz', 'treemap'),
                    plot_limit = 40L,
                    plot_limit_text = 20L,
                    output_dir = NULL,
                    output_file = NULL,
                    output_format = rmarkdown::html_document(
                      theme = 'united',
                      highlight = 'tango',
                      toc = TRUE,
                      toc_float = TRUE,
                      toc_collapsed = FALSE,
                      toc_depth = 3,
                      number_sections = FALSE
                    ),
                    file = FALSE,
                    file_format = c('csv', 'json'),
                    file_type = c('single', 'multiple'),
                    exit = TRUE,
                    verbose = TRUE) {
  # verbose
  verbose_message(verbose, 'Creating report..')
  # time
  time_report <<- Sys.time() # TODO save to disk
  # checking
  if (is.null(con)) {
    stop('Provide a database connnection.')
  }
  if (length(schema) > 1) {
    stop('Only one schema can be supplied.')
  }
  if (length(tbl) > 1) {
    stop('Only one table can be used to create a report. Use mapply for multiple table reports.')
  }
  if (!is.logical(plot_distinct)) {
    stop('plot_distinct must be logical.')
  }
  plot_type = match.arg(plot_type)
  if (is.null(plot_limit) || !is.numeric(plot_limit)) {
    stop('The argument plot_limit must be integer.')
  }
  if (is.null(output_dir)) {
    stop('Provide a directory for the output: output_dir = NULL')
  }
  file_format = match.arg(file_format)
  file_type = match.arg(file_type)
  # check db
  tbl_exists(con = con, schema = schema, tbl = tbl)
  # object deparse
  # defaults
  con_deparse <<- deparse(match.call()$con) # TODO store it on  disk
  tbl_name_defualt = tbl_name(con = con,
                              schema = schema,
                              tbl = tbl)
  if (is.null(report_title)) {
    report_title = paste0('Table: ', tbl_name_defualt)
  }
  if (is.null(output_file)) {
    output_file = gsub('\\.', '_', tbl_name_defualt)
  }
  # render
  fl = system.file('rmd', 'rmarkdown_template.Rmd', package = 'dbreport')
  rmarkdown::render(fl,
                    output_format = output_format,
                    output_dir = output_dir,
                    output_file = output_file,
                    params = list(
                      con = con,
                      schema = schema,
                      tbl = tbl,
                      column = column,
                      entry = entry,
                      title = report_title,
                      report_text = report_text,
                      plot_distinct = plot_distinct,
                      plot_type = plot_type,
                      plot_limit = plot_limit,
                      plot_limit_text = plot_limit_text,
                      output_dir = output_dir,
                      file = file,
                      file_format = file_format,
                      file_type = file_type,
                      verbose = verbose
                    ),
                    quiet = TRUE)
  verbose_message(verbose, 
                  paste0('Report finished in ',
                         format(round(Sys.time() - time_report, 1), unit = 'auto'),
                         '.'))
  verbose_message(verbose,
                  paste0('Saved under: ', output_dir))
  if (exit) {
    if (inherits(con, c('PqConnection',
                        'PostgreSQLDriver',
                        'MySQLDriver',
                        'SQLiteDriver'))) {
      DBI::dbDisconnect(con)
    }
  }
}



