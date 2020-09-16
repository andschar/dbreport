#' Helper S3 method to prepare data for plots
#'
#' @param dat data.table object
#' @param col column
#' @param plot_limit Plot limit
#' @param plot_limit_text Plot text limit
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
pl_prepare = function(...) {
  UseMethod('pl_prepare')
}
pl_prepare.categorical = function(dat,
                                  col,
                                  plot_limit,
                                  plot_limit_text,
                                  ...) {
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(col), plot_limit_text))]
  dat[, n_distinct := as.numeric(n_distinct)] # NB to get rid of integer64, that might be returned - can be done more gracefully - as.integer doesn't work!
  if (!is.null(plot_limit) & plot_limit < nrow(dat)) {
    dat = dat[1:plot_limit]
  }
  
  dat
}
pl_prepare.continuous = function(dat,
                                 col,
                                 plot_limit,
                                 plot_limit_text,
                                 ...) {
  data.table::setDT(dat)
  # datt = data.table::transpose(dat, keep.names = 'variable')
  # data.table::setnames(datt, 'V1', 'value')
  
  dat
}
