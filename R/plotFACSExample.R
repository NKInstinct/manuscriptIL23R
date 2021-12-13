#' Plot FACS data using pretty plotting defaults
#'
#' This is meant to produce a publication-quality FACS plot, but can also be
#' used for exploratory analysis. Note that this function expects to draw GATED
#' data - you'll need to run a raw ggcyto call (or make a new function for
#' ungated) if you don't have a gate in mind.
#'
#' @param data A GatingHierarchy (for one plot) or GatingSet (for multiple).
#'   This function will facet over the GatingSet as expected.
#' @param x A string specifying the parameter to plot on the X axis (either as
#'   channel or marker name)
#' @param y A string specifying the parameter to plot on the Y axis. Currently,
#'   this function does not support histograms, may update in the future.
#' @param gate A string specifying the gate (or vector of strings, for multiple
#'   gates). Call gs_pop_get_paths on your GatingSet/Hierarchy for available
#'   options.
#' @param bins A numeric specifying the number of bins to pass to
#'   geom_density2d. Remember that this number should be WAY lower than what is
#'   passed to geom_hex.
#' @param fontFix Boolean specifying whether the fonts should be shrunk down as
#'   appropriate for figure publication-size plots. Defaults to TRUE so you
#'   don't forget to do it!
#' @param flowFix Boolean specifying whether the FACS plots should be cleaned up
#'   by removing the faceting strips and axis numbers. Defaults to TRUE so you
#'   don't forget to do it!
#'
#' @return A ggplot object containing the plotted FACS data
#'
#' @importFrom ggcyto %+%
#' @export
#'
plotFACSExample <- function(data, x, y, gate, bins = 30, fontFix = TRUE,
                            flowFix = TRUE){
  gg <- ggcyto::ggcyto(data, ggplot2::aes(!!x, !!y)) +
    ggplot2::geom_density2d(bins = bins, colour = "black", alpha = 0.5) +
    ggcyto::geom_gate(gate, colour = "black", alpha = 0.7) +
    ggcyto::geom_stats(adjust = 0.9) +
    ggpubr::theme_pubr() +
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_blank())
  gg <- ggcyto::as.ggplot(gg)

  if(fontFix == TRUE){
    gg <- gg +
      ggplot2::theme(text = ggplot2::element_text(size = 10,
                                                  family = "TT Arial"),
                     title = ggplot2::element_text(size = 10))
  }

  if(flowFix == TRUE){
    gg <- gg +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank(),
                     title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank())
  }
  return(gg)
}
#'
#' @examples
#'
#' path_to_gs <- system.file("extdata", package = "manuscriptIL23R")
#' gs <- flowWorkspace::load_gs(paste0(path_to_gs, "/example.gs"))
#' plotFACSExample(gs[[1]], "IFNg", "CD8a", "CD8 IFNg")
