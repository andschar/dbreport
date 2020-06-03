#' Plots a vertical lollipop ggplot()
#' 
#' @param dat data.table object
#' @param x category
#' @param y count
#' 
pl_lollipop = function(dat = NULL,
                       x = NULL,
                       y = NULL) {
  # DEBUG dat = tbl_l$distinct$species[1:100L]; x = 'species'; y = 'n_distinct'; dat[ , n_distinct := as.integer(n_distinct) ]
  ggplot2::ggplot(dat, ggplot2::aes(x = reorder(get(x), get(y)), y = get(y))) +
    # ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_segment(ggplot2::aes(xend = get(x), y = 0, yend = get(y)), 
                          col = 'skyblue') +
    ggplot2::geom_point(col = 'blue', size = 4, alpha = 0.6) +  
    ggplot2::geom_text(ggplot2::aes(label = get(y)),
                       position = ggplot2::position_dodge(width = 0.9),
                       vjust = 0.5, hjust = -1) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.02, 0.025))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.025, 0.12))) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = NULL, x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14))
}

#' Plots a horizontal bar-ggplot()
#' 
#' @param dat data.table object
#' @param x category
#' @param y count
#' 
pl_bar_horiz = function(dat = NULL,
                        x = NULL,
                        y = NULL) {
  ggplot2::ggplot(dat, ggplot2::aes(x = reorder(get(x), -get(y)), y = get(y))) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(ggplot2::aes(label = get(y)),
                       position = ggplot2::position_dodge(width = 0.9),
                       vjust = 0, hjust = 0, angle = 45) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::labs(# title = col,
                  # subtitle = col,
                  x = NULL,
                  y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 14))
}
#' Plots a ggplot() treemap
#' 
#' @param dat data.table object
#' @param x category
#' @param y count
#'   
pl_treemap = function(dat = NULL,
                      x = NULL,
                      y = NULL) {
  ggplot2::ggplot(dat, ggplot2::aes_string(area = y, fill = x, label = x)) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(fontface = 'italic',
                                  colour = 'white',
                                  place = 'centre',
                                  grow = FALSE,
                                  reflow = TRUE) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme(legend.position = 'none')
}
  
