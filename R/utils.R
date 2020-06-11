# taken from: https://stackoverflow.com/questions/15365829/dynamic-height-and-width-for-knitr-plots?noredirect=1&lq=1
#' Create sub-chunks for plots
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
  if (is.null(limit)) {
    return(x)
  }
  if (limit <= 10) {
    stop('Limit has to be greater than 10.')
  }
  ifelse(
    nchar(x) > limit, 
    paste0(substr(x, 1, limit - 5),
           '..',
           substr(x, nchar(x) - 2, nchar(x))),
    x
  )
}
