
#' Custom ggplot2 theme
#'
#' This is a custom ggplot2 theme loosely based on recommendations of Edward
#' Tufte, Stephen Few, and others. By default, the panel background is blank,
#' gridlines are removed, and axis colors are grey to minimize distraction from
#' the data.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param xticks logical indicating whether tick marks should be displayed on
#'   the x-axis
#' @param yticks logical indicating whether tick marks should be displayed on
#'   the y-axis
#'
#' @export
#'
theme_bg <- function(base_size = 11, base_family = "", xticks = TRUE, yticks = TRUE) {
    p <- ggplot2::theme_bw(
        base_family = base_family,
        base_size = base_size
    ) +
        ggplot2::theme(
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(color = "grey35"),
            legend.title = ggplot2::element_text(color = "grey35"),
            panel.background = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = "grey35"),
            plot.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color = "grey85"),
            axis.text = ggplot2::element_text(color = "grey35"),
            axis.title = ggplot2::element_text(color = "grey35"),
            axis.ticks = ggplot2::element_line(color = "grey50"),
            plot.caption = ggplot2::element_text(color = "gray35"),
            plot.title = ggplot2::element_text(color = "gray35")
        )

    if (!xticks) p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    if (!yticks) p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    p
}

#' Custom ggplot2 theme for black and white printing
#'
#' This is a custom ggplot2 theme loosely based on recommendations of Edward
#' Tufte, Stephen Few, and others. The panel background is blank and gridlines
#' are removed. In contrast to theme_bg, this theme uses dark axis colors to
#' improve black and white printing or copying.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param xticks logical indicating whether tick marks should be displayed on
#'   the x-axis
#' @param yticks logical indicating whether tick marks should be displayed on
#'   the y-axis
#'
#' @export
#'
theme_bg_print <- function(base_size = 11, base_family = "", xticks = TRUE, yticks = TRUE) {
    p <- ggplot2::theme_bw(
        base_family = base_family,
        base_size = base_size
    ) +
        ggplot2::theme(
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(color = "black"),
            legend.title = ggplot2::element_text(color = "black"),
            panel.background = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = "black"),
            plot.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color = "black"),
            axis.text = ggplot2::element_text(color = "black"),
            axis.title = ggplot2::element_text(color = "black"),
            axis.ticks = ggplot2::element_line(color = "black"),
            plot.caption = ggplot2::element_text(color = "black")
        )

    if (!xticks) p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    if (!yticks) p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    p
}

#' Custom ggplot2 theme
#'
#' This is a custom ggplot2 theme loosely based on recommendations of Edward
#' Tufte, Stephen Few, and others. By default, the panel background is blank,
#' gridlines are removed, and axis colors are grey to minimize distraction from
#' the data. This theme uses white lines and text for display on a dark
#' background.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param xticks logical indicating whether tick marks should be displayed on
#'   the x-axis
#' @param yticks logical indicating whether tick marks should be displayed on
#'   the y-axis
#'
#' @export
#'
theme_bg_invert <- function(base_size = 11, base_family = "", xticks = TRUE, yticks = TRUE) {
    p <- ggplot2::theme_bw(
        base_family = base_family,
        base_size = base_size
    ) +
        ggplot2::theme(
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(color = "grey35"),
            legend.title = ggplot2::element_text(color = "grey35"),
            panel.background = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = "grey35"),
            plot.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color = "grey85"),
            axis.text = ggplot2::element_text(color = "grey35"),
            axis.title = ggplot2::element_text(color = "grey35"),
            axis.ticks = ggplot2::element_line(color = "grey50"),
            plot.caption = ggplot2::element_text(color = "gray35"),
            plot.title = ggplot2::element_text(color = "gray35")
        )

    if (!xticks) p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    if (!yticks) p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    p
}

#' Custom ggplot2 theme for PowerPoint
#'
#' This is a custom ggplot2 theme loosely based on recommendations of Edward
#' Tufte, Stephen Few, and others. By default, the panel background is blank,
#' gridlines are removed, and axis colors are grey to minimize distraction from
#' the data. This theme is designed for use in PowerPoint with a white
#' background.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param xticks logical indicating whether tick marks should be displayed on
#'   the x-axis
#' @param yticks logical indicating whether tick marks should be displayed on
#'   the y-axis
#' @param family string with a font family name, defaults to Calibri
#'
#' @export
#'
theme_bg_ppt <- function(base_size = 11, base_family = "", xticks = TRUE,
                         yticks = TRUE, family = "Calibri") {
    p <- ggplot2::theme_bw(
        base_family = base_family,
        base_size = base_size
    ) +
        ggplot2::theme(
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(
                family = family,
                size = 12,
                color = "grey35"
            ),
            legend.title = ggplot2::element_text(color = "grey35"),
            panel.background = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = "grey35"),
            plot.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(color = "grey85"),
            axis.text.x = element_text(vjust = 0.1),
            axis.text = ggplot2::element_text(
                family = family,
                size = 14,
                color = "grey35"
            ),
            axis.title.x = element_text(vjust = 0.1),
            axis.title = ggplot2::element_text(
                family = family,
                size = 16,
                color = "grey35"
            ),
            axis.ticks = ggplot2::element_line(color = "grey50"),
            plot.caption = ggplot2::element_text(color = "gray35"),
            plot.title = ggplot2::element_text(
                family = family,
                size = 22,
                hjust = 0.5,
                color = "gray35"
            )
        )

    if (!xticks) p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    if (!yticks) p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    p
}
