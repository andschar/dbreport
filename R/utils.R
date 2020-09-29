# Utility functions for the package

#' function to bind data.frame s in a list to a single data.frame
#'
#' taken from: https://stackoverflow.com/questions/7962267/cbind-a-dataframe-with-an-empty-dataframe-cbind-fill
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
cbind_fill = function(...,
                      dflist = NULL) {
  nm = c(list(...), dflist)
  nm = lapply(nm, as.matrix)
  n = max(sapply(nm, nrow))
  out = do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(
      , n - nrow(x), ncol(x)
    ))))
  
  data.table::data.table(out)
}

#' function to convert numeric (byte) value into pretty byte representation
#'
#' taken from: https://stackoverflow.com/questions/29787452/how-do-i-quickly-convert-the-size-element-of-file-info-from-bytes-to-kb-mb-g
#' taken from
#' @param size size data.table
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
conv_byte = function(size) {
  data.table::setDT(size)
  size[, size := utils:::format.object_size(size[, 1], standard = 'IEC', units = 'auto')] # TODO ::: not allowed on CRAN
  out = size[, data.table::tstrsplit(size, ' ')]
  data.table::setnames(out, c('size', 'unit'))
  
  out
}

#' function to transpose example columns
#'
#' @param l a list
#' @param n number of examples to be selected
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
examples_n = function (l, n = 3) {
  l = lapply(lapply(l, `[[`, 1), head, 3)
  example = data.table::as.data.table(data.table::transpose(l), col.names = names(l[[1]]))
  example[, cols := names(l)]
  data.table::setnames(example, c(paste0('example', 1:(length(
    names(example)
  ) - 1)), 'cols'))
  example
}

#' function to convert numeric values to perc strings
#'
#' @param x numeric vector to convert
#' @param total numeric vector to divide with
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
perc_value = function(x, total) {
  paste0(x, ' (', round(x / total * 100, 1), '%)')
}

#' Create sub-chunks for plots
#'
#' taken from: https://stackoverflow.com/questions/15365829/dynamic-height-and-width-for-knitr-plots
#'
#' @param pl a plot object
#' @param fig.height figure height
#' @param fig.width figure width
#' @param chunk_name name of the chunk
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
subchunkify = function(pl,
                       fig.height = 7,
                       fig.width = 5,
                       chunk_name = 'plot') {
  pl_deparsed = paste0(deparse(function() {
    pl
  }), collapse = '')
  
  sub_chunk = paste0(
    "```{r ",
    chunk_name,
    ", fig.height=",
    fig.height,
    ", fig.width=",
    fig.width,
    ", dpi=72",
    ", echo=FALSE, message=FALSE, warning=FALSE}",
    "\n(",
    pl_deparsed,
    ")()",
    "\n```"
  )
  
  cat(knitr::knit(
    text = knitr::knit_expand(text = sub_chunk),
    quiet = TRUE
  ))
}

#' Shorten string
#'
#' @param x string
#' @param limit integer; number of characters to limit string to
#'
#' @return a string
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
str_limit = function(x, limit = 20L) {
  # TODO add to andmisc
  if (is.null(limit)) {
    return(x)
  }
  if (limit <= 10) {
    stop('Limit has to be greater than 10.')
  }
  ifelse(nchar(as.character(x)) > limit,
         paste0(substr(x, 1, limit - 5),
                '..',
                substr(x, nchar(x) - 2, nchar(x))),
         x)
}
