#' function to bind data.frame s in a list to a single data.frame
#'
cbind_fill = function(..., dflist = NULL){
  # https://stackoverflow.com/questions/7962267/cbind-a-dataframe-with-an-empty-dataframe-cbind-fill
  nm = c(list(...), dflist)
  nm = lapply(nm, as.matrix)
  n = max(sapply(nm, nrow)) 
  out = do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
  out = data.table(out)

  return(out)
}
