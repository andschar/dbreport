# Utility functions for the package

#' function to bind data.frame s in a list to a single data.frame
#'
#' taken from: https://stackoverflow.com/questions/7962267/cbind-a-dataframe-with-an-empty-dataframe-cbind-fill
#' @param ... For list().
#' @param dflist List of data.fames.
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
#' @param size Numeric size value.
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
#'
conv_byte = function(size) {
  size_str = strsplit(format(size, standard = 'IEC', units = 'auto'), '\\s')[[1]]
  
  data.table::data.table(size = size_str[1],
                         unit = size_str[2])
}

#' function to transpose example columns
#'
#' @param l A list.
#' @param n Number of examples to be selected.
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'
examples_n = function (l, n = 3) {
  cols = NULL
  l = lapply(lapply(l, `[[`, 1), utils::head, 3)
  example = data.table::as.data.table(data.table::transpose(l), col.names = names(l[[1]]))
  example[, cols := names(l)]
  data.table::setnames(example, c(paste0('example', 1:(length(
    names(example)
  ) - 1)), 'cols'))
  example
}

#' function to convert numeric values to perc strings
#'
#' @param x Numeric vector to convert.
#' @param total Numeric vector to divide with.
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

#' Verbose ouput messages
#' 
#' @param verbose Should a verbose output be printed to console?
#' @param msg Message to be printed to console
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
verbose_message = function(verbose,
                           msg) {
  if (verbose) {
    message(msg)
  }
}

#' Unified plot theme
#' 
theme_minimal2 = function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14)
    )
}









