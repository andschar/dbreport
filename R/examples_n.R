# function to retrieve examples

examples_n = function (l, n = 3) {
  l = lapply(lapply(l, `[[`, 1), head, 3)
  example = as.data.table(data.table::transpose(l), col.names = names(l[[1]]))
  example[ , cols := names(l) ]
  setnames(example, c(paste0('example', 1:(length(names(example))-1)), 'cols'))
  example
}
