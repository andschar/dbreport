#' Create reports from data base tables, R table objects (data.table, data.frame, tibble)
#'
#' @title Create reports of a table
#' 
#' @import data.table rmarkdown knitr ggplot2 treemap
#'
#' @param con Connection to data base, R-object (data.table, data.frame, tibble). Required.
#' @param schema Data base schema (if applicable).
#' @param tbl Table to be used for summary report (if applicable).
#' @param col Table columns that should be summarised in report.
#' @param entry Table entries which should be comsidered in summary report.
#' @param title Title of summary report.
#' @param text User defined text input to add some description to the report. Default NULL - no text. Can be a character vector or a file.
#' @param plot_distinct Include plots of ordered distinct counts of each column in summary report.
#' @param plot_type Should a ggplot(), a barplot() or a treemap::treemap() be produced?
#' @param plot_limit Limit distinct count axis in the plots. Default: 100L. Should be set to a reasonable size.
#' @param output_dir To which directory should the output be saved?
#' @param output_file File name
#' @param output_format Which markdown output_format should be used for rendering the summary report? E.g. html_document, html_vignette, pdf_document, ...
#' @param file Should one or multiple files for each distinct column counts br created?
#' @param file_format To which format should the file(s) be put out (csv, json). Multiple possible.
#' @param file_type Output each column distinct counts set to a single or to multiple files.
#' @param exit logical. Should database connections be closed after runing dbreport(). Default: TRUE.
#' @param verbose TODO
#' 
#' @return The function creates a markdown report and exports files 
#' 
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' @export
#' @examples
#' \donttest{
#' # creates a .html, .pdf, .doc via Rmarkdown
#' dbreport(con = iris,
#'          plot_distinct = TRUE,
#'          plot_type = 'tree', # Instead of a ggplot() you can also create a barplot() or a treemap() output
#'          plot_limit = 10, # Limit your plot to the first 15 entries
#'          output_dir = file.path(tempdir(), 'iris'),
#'          output_file = 'test',
#'          output_format = 'pdf_document', # you can also create a .pdf
#'          title = 'My iris report',
#'          file = TRUE, # Output single distinct counts to a file
#'          file_type = 'multiple',
#'          file_format = 'csv')
#' }
dbreport = function(con = NULL,
                    schema = NULL,
                    tbl = NULL,
                    col = NULL,
                    entry = NULL,
                    title = 'My report title',
                    text = NULL,
                    plot_distinct = TRUE,
                    plot_type = c('gg', 'bp', 'tree'),
                    plot_limit = 100L,
                    output_dir = NULL,
                    output_file = 'report',
                    output_format = c('html_document', 'html_vignette', 'pdf_document', 'word_document'),
                    file = TRUE,
                    file_format = c('csv', 'json'),
                    file_type = c('single', 'multiple'),
                    exit = TRUE,
                    verbose = FALSE) {
  # checking
  if (is.null(con))
    stop('Provide a data base connnection.')
  if (length(schema) > 1)
    stop('Only one schema can be supplied.')
  if (length(tbl) > 1)
    stop('Only one table can be used to create a report. Use mapply for multiple table reports.')
  if (!is.logical(plot_distinct))
    stop('plot_distinct must be logical.')
  plot_type = match.arg(plot_type)
  if (is.null(plot_limit) || !is.numeric(plot_limit))
    stop('The argument plot_limit must be integer.')
  if (is.null(output_dir))
    stop('Provide a directory for the output: output_dir = NULL')
  output_format = match.arg(output_format, several.ok = TRUE)
  file_format = match.arg(file_format)
  file_type = match.arg(file_type)
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
                      col = col,
                      entry = entry,
                      title = title,
                      text = text,
                      plot_distinct = plot_distinct,
                      plot_type = plot_type,
                      plot_limit = plot_limit,
                      output_dir = output_dir,
                      file = file,
                      file_format = file_format,
                      file_type = file_type,
                      verbose = verbose
                    ))
  if (exit) {
    if (inherits(con, c('PqConnection', 'PostgreSQLDriver', 'MySQLDriver', 'SQLiteDriver'))) {
      DBI::dbDisconnect(con)
    }
  }
}



