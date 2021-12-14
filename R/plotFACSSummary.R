#' Plot Summarized FACS Data
#'
#' This function takes a dataframe as output by calling gs_pop_get_stats on a
#' gatingset and then plots a summary bargraph of a selected population. Note
#' that this function is not designed for facetting, so make sure you specify a
#' single population of interest to filter on!
#'
#' @param data A dataframe as output by gs_pop_get_stats. Requires at minimum a
#'   column called 'percent', a column called 'pop', and a third column to group
#'   data on specified by xGroup below.
#' @param filterPop A regular expression to filter on a single population for
#'   graphing.
#' @param ylab A string with which to label the y-axis.
#' @param xGroup An NSE-style selection to specify the column to group data by
#'   (i.e. the x-axis).
#' @param fillGroup An NSE-style selection to specify the column to sub-group
#'   data by (i.e. the fill scale within the x-axis). Normally, leave this NULL
#'   (the default) to make a standard summary bar plot. For experiments like the
#'   InfinityFlow in the gut, however, the plot needs two levels of grouping
#'   (population on the x-axis and then genotype within each population by
#'   fill), so this lets you specify that second level of grouping. Probably
#'   should leave statbar off for this one!
#' @param fillScale A vector of fill colours to colour the column. Defaults to
#'   c("grey50", "black") for a two-column graph. The manuscript also makes
#'   heavy use of c("grey75", "grey50", "grey25", "black") for four-column
#'   graphs.
#' @param ... Additional args to pass to stat_compare_means. vjust is
#'   particularly useful here.
#' @param statbar A string specifying whether stats should be plotted, and if
#'   so, whether they should have a comparison bar included. Defaults to "nobar"
#'   (i.e. plot stats but don't include the bar). Passing a numeric value sets
#'   the vertical position of the statbar and significance label (in y-axis
#'   units). Setting to anything else results in no stats being calculated or
#'   plotted.
#' @param fontFix Boolean specifying whether the fonts should be shrunk down as
#'   appropriate for figure publication-size plots. Defaults to TRUE so you
#'   don't forget to do it!
#'
#' @return A ggplot object showing the summarized bar graph.
#' @importFrom rlang .data
#' @export
plotFACSSummary <- function(data, filterPop, ylab, xGroup = .data$Genotype, fillGroup = NULL, fillScale = c("grey50", "black"),
                            ..., statbar = "nobar", fontFix = TRUE){
  xGroup <- ggplot2::enquo(xGroup)
  if(!is.null(fillGroup)){
    fillGroup <- ggplot2::enquo(fillGroup)
  }

  df <- data |>
    dplyr::filter(stringr::str_detect(.data$pop, filterPop))
  if(is.null(fillGroup)){
    gg <- ggplot2::ggplot(df, ggplot2::aes(!!xGroup, .data$percent, fill = !!xGroup)) +
      ggplot2::stat_summary(geom = "errorbar") +
      ggplot2::geom_bar(stat = "summary", color = "black")
  }else{
    gg <- ggplot2::ggplot(df, ggplot2::aes(!!xGroup, .data$percent, fill = !!fillGroup)) +
      ggplot2::stat_summary(geom = "errorbar", position = "dodge") +
      ggplot2::geom_bar(stat = "summary", color = "black", position = "dodge")
  }

  gg <- gg +
    ggplot2::scale_fill_manual(values = fillScale) +
    ggplot2::labs(y = ylab, x = ggplot2::element_blank()) +
    ggpubr::theme_pubr() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   legend.position = "none")

  if(statbar == "nobar"){
    gg <- gg + ggpubr::stat_compare_means(size = 6, label = "p.signif", ...)
  }else if(is.numeric(statbar)){
    gg <- statBar(gg, statbar, ...)
  }

  if(fontFix == TRUE){
    gg <- fontFix(gg)
  }

  return(gg)
}

#'
#' @examples
#' path_to_res <- system.file("extdata", "flowResults.Rds", package = "manuscriptIL23R")
#' res <- readRDS(path_to_res)
#' gg <- plotFACSSummary(res,
#'                       "CD8/IFNg",
#'                       ylab = "% IFNg (of CD8)",
#'                       xGroup = Genotype,
#'                       statbar = 60)
