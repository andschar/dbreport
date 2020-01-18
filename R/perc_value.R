#' function to convert numeric values to perc strings
#'
perc_value = function(x, total) {
  paste0(x, ' (', round(x / total * 100, 1), '%)')
}