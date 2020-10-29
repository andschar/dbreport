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
pl_lollipop.char = function(dat = NULL,
                            column = NULL,
                            plot_limit = NULL,
                            plot_limit_text = NULL,
                            ...) {
  # dat = tbl_l$distinct$class; x = 'species'; y = 'n_distinct'; # DEBUG
  # prepare
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(column), plot_limit_text))]
  if (!is.null(plot_limit) & plot_limit < nrow(dat)) {
    dat = dat[1:plot_limit]
  }
  # plot
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(var20, n_distinct),
                                    y = n_distinct)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = var20,
        y = 0,
        yend = n_distinct
      ),
      color = 'skyblue'
    ) +
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
    theme_minimal2()
}
pl_lollipop.nume = function(dat = NULL,
                            column = NULL,
                            ...) {
  dat[ , var := as.numeric(get(column)) ]
  ggplot2::ggplot(dat, ggplot2::aes(x = var)) +
    ggplot2::geom_density(na.rm = TRUE) +
    ggplot2::labs(x = NULL) +
    theme_minimal2()
}
pl_lollipop.date = function(dat = NULL,
                            column = NULL,
                            ...) {
  # dat = tbl_l$distinct$date # debuging
  ggplot2::ggplot(dat, ggplot2::aes(y = get(column),
                                    x = n_distinct)) +
    ggplot2::geom_point() +
    # TODO
    # ggplot2::geom_segment(
    #   ggplot2::aes(
    #     xend = get(column),
    #     y = 0,
    #     yend = n_distinct
    #   ),
    #   color = 'skyblue'
    # ) +
    ggplot2::scale_y_date() +
    ggplot2::labs(y = NULL, x = NULL)# +
    theme_minimal2()
}

pl_lollipop.geom = function(dat = NULL,
                            column = NULL,
                            ...) {
  # dat = tbl_l$distinct$date # debuging
  # TODO
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
pl_bar_horiz.char = function(dat = NULL,
                             column = NULL,
                             plot_limit = NULL,
                             plot_limit_text = NULL,
                             ...) {
  # prepare
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(column), plot_limit_text))]
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
    ggplot2::labs(
      x = NULL,
      y = NULL) +
    theme_minimal2() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
pl_bar_horiz.nume = function(dat = NULL,
                             column = NULL,
                             ...) {
  dat[, var := as.numeric(get(column))] # NB to get rid of integer64
  ggplot2::ggplot(dat, ggplot2::aes(x = var)) +
    ggplot2::geom_density(na.rm = TRUE) +
    ggplot2::labs(x = NULL) +
    theme_minimal2()
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

pl_treemap.char = function(dat = NULL,
                           column = NULL,
                           plot_limit = NULL,
                           plot_limit_text = NULL,
                           ...) {
  # prepare
  data.table::setDT(dat)
  dat[, var20 := as.character(str_limit(get(column), plot_limit_text))]
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
    theme_minimal2()
    ggplot2::theme(legend.position = 'none')
}

pl_treemap.nume = function(dat = NULL,
                           column = NULL,
                           ...) {
  dat[ , var := as.numeric(get(column)) ]
  ggplot2::ggplot(dat, ggplot2::aes(x = var)) +
    ggplot2::geom_density(na.rm = TRUE) +
    ggplot2::labs(x = NULL) +
    theme_minimal2()
}
