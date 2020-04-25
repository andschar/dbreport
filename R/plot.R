#' Plots a ggplot()
#' 
#' @param dat data.table object
#' @param col column
#' 
pl_gg = function(dat = NULL, col = NULL) {
  ggplot2::ggplot(dat, ggplot2::aes(x = reorder(var20, -n), y = n)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = n),
                       position = ggplot2::position_dodge(width = 0.9),
                       vjust = 0, hjust = 0, angle = 45) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::labs(title = col, subtitle = col, x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
#' Plots a barplot()
#' 
#' @param dat data.table object 
#'
# https://stackoverflow.com/questions/29583849/save-a-plot-in-an-object
plotit = function(dat, col){
  barplot(dat$n ~ reorder(dat$var20, -n),
          data = dat,
          ylab = 'n',
          xlab = NA,
          main = col)
}
#' plots a barplot()
#' 
#' @param dat data.table object 
#'
pl_bp = function(dat = dat) { plotit(dat) }
#' Plots a treemap()
#' 
#' @param dat data.table object
#' @param index used in treemap()
#' @param vsize used in treemap()
#' 
pl_tree = function(dat = NULL, index = NULL, vsize = NULL) {
  treemap::treemap(
    dtf = dat,
    # data
    index = index,
    vSize = vsize,
    type = "index",
    # Main
    title = "",
    palette = "Dark2",
    # Borders
    border.col = c("black"),
    border.lwds = 1,
    # Labels
    fontsize.labels = 0.5,
    fontcolor.labels = "white",
    fontface.labels = 1,
    bg.labels = c("transparent"),
    align.labels = c("left", "top"),
    overlap.labels = 0.5,
    inflate.labels = TRUE
  )
}

