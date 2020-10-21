#' Plots a vertical lollipop ggplot()
#'
#' @param dat data.table object
#' @param column Column
#' @param plot_limit Limit number of plotted lines
#' @param plot_limit_text Limit axis plot axis text
#' @param ... not used
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
pl_lollipop = function(...) {
  UseMethod('pl_lollipop')
}
pl_lollipop.categorical = function(dat = NULL,
                                   column = NULL,
                                   plot_limit = NULL,
                                   plot_limit_text = NULL,
                                   ...) {
  # dat = tbl_l$distinct$class; x = 'species'; y = 'n_distinct'; dat[ , n_distinct := as.integer(n_distinct) ] # DEBUG
  # prepare
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(column), plot_limit_text))]
  dat[, n_distinct := as.numeric(n_distinct)] # NB to get rid of integer64, that might be returned - can be done more gracefully - as.integer doesn't work!
  if (!is.null(plot_limit) & plot_limit < nrow(dat)) {
    dat = dat[1:plot_limit]
  }
  # plot
  ggplot2::ggplot(dat, ggplot2::aes(x = reorder(var20, n_distinct), y = n_distinct)) +
    ggplot2::geom_segment(ggplot2::aes(xend = var20,
                                       y = 0,
                                       yend = n_distinct),
                          color = 'skyblue') +
    ggplot2::geom_point(color = 'blue',
                        size = 4,
                        alpha = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_distinct),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = 0.5,
      hjust = -1
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.02, 0.025))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.025, 0.12))) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = NULL, x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14)
    )
}
pl_lollipop.continuous = function(dat = NULL,
                                  column = NULL,
                                  ...) {
  # dat = data.table::transpose(dat)
  ggplot2::ggplot(dat, ggplot2::aes(x = get(column))) +
    ggplot2::geom_density() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  # ggplot2::geom_segment(ggplot2::aes(
  #   xend = get(x),
  #   y = 0,
  #   yend = get(y)
  # ),
  # color = 'skyblue') +
  # ggplot2::geom_point(color = 'blue',
  #                     size = 4,
  #                     alpha = 0.6)
  
}

#' Plots a horizontal bar-ggplot()
#'
#' @param dat data.table object
#' @param column Column
#' @param plot_limit Limit number of plotted lines
#' @param plot_limit_text Limit axis plot axis text
#' @param ... not used
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
pl_bar_horiz = function(...) {
  UseMethod('pl_bar_horiz')
}
pl_bar_horiz.categorical = function(dat = NULL,
                                    column = NULL,
                                    plot_limit = NULL,
                                    plot_limit_text = NULL,
                                    ...) {
  # prepare
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(column), plot_limit_text))]
  dat[, n_distinct := as.numeric(n_distinct)] # NB to get rid of integer64, that might be returned - can be done more gracefully - as.integer doesn't work!
  if (!is.null(plot_limit) & plot_limit < nrow(dat)) {
    dat = dat[1:plot_limit]
  }
  # plot
  ggplot2::ggplot(dat, ggplot2::aes(x = var20, y = n_distinct)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::geom_text(
      ggplot2::aes(label = n_distinct),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = 0,
      hjust = 0,
      angle = 45
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::labs(# title = column,
      # subtitle = column,
      x = NULL,
      y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 14,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(size = 14)
    )
}
pl_bar_horiz.continuous = function(dat = NULL,
                                   column = NULL,
                                   ...) {
  ggplot2::ggplot(dat, ggplot2::aes(x = get(column))) +
    ggplot2::geom_density() +
    ggplot2::theme_minimal()
}

#' Plots a ggplot() treemap
#'
#' @param dat data.table object
#' @param column Column
#' @param plot_limit Limit number of plotted lines
#' @param plot_limit_text Limit axis plot axis text
#' @param ... not used
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
pl_treemap = function(...) {
  UseMethod('pl_treemap')
}

pl_treemap.categorical = function(dat = NULL,
                                  column = NULL,
                                  plot_limit = NULL,
                                  plot_limit_text = NULL,
                                  ...) {
  # prepare
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(column), plot_limit_text))]
  dat[, n_distinct := as.numeric(n_distinct)] # NB to get rid of integer64, that might be returned - can be done more gracefully - as.integer doesn't work!
  if (!is.null(plot_limit) & plot_limit < nrow(dat)) {
    dat = dat[1:plot_limit]
  }
  # plot
  ggplot2::ggplot(dat, ggplot2::aes(
    area = n_distinct,
    fill = var20,
    label = var20
  )) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      fontface = 'italic',
      colour = 'white',
      place = 'centre',
      grow = FALSE,
      reflow = TRUE
    ) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'none')
}

pl_treemap.continuous = function(dat = NULL,
                                 column = NULL,
                                 ...) {
  ggplot2::ggplot(dat, ggplot2::aes(x = get(column))) +
    ggplot2::geom_density() +
    ggplot2::theme_minimal()
  # ggplot2::ggplot(dat, ggplot2::aes_string(
  #   area = y,
  #   fill = x,
  #   label = x
  # )) +
  #   treemapify::geom_treemap() +
  #   treemapify::geom_treemap_text(
  #     fontface = 'italic',
  #     colour = 'white',
  #     place = 'centre',
  #     grow = FALSE,
  #     reflow = TRUE
  #   ) +
  #   ggplot2::scale_fill_viridis_d() +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(legend.position = 'none')
}
