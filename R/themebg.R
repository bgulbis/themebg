
#' Custom ggplot2 theme
#'
#' This is a custom ggplot2 theme loosely based on recommendations of Edward
#' Tufte, Stephen Few, and others. By default, the panel background is blank,
#' gridlines are removed, and axis colors are grey to minimize distraction from
#' the data.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @export
#'
theme_bg <- function(base_size = 11, base_family = "") {
    ggplot2::theme_bw(base_family = base_family, base_size = base_size) +
        ggplot2::theme(legend.background = ggplot2::element_blank(),
              legend.key = ggplot2::element_blank(),
              legend.text = ggplot2::element_text(color = "grey35"),
              panel.background = ggplot2::element_blank(),
              panel.border = ggplot2::element_blank(),
              strip.background = ggplot2::element_blank(),
              strip.text = ggplot2::element_text(color = "grey35"),
              plot.background = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(color = "grey85"),
              axis.text = ggplot2::element_text(color = "grey35"),
              axis.title = ggplot2::element_text(color = "grey35"),
              axis.ticks = ggplot2::element_line(color = "grey35"))
}

