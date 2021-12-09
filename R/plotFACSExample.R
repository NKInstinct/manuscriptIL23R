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
#' @import ggplot2 ggcyto
#' @export
#'
plotFACSExample <- function(data, x, y, gate, bins = 30, fontFix = TRUE, flowFix = TRUE){
  gg <- ggcyto(data, aes(!!x, !!y)) +
    geom_density2d(bins = bins, colour = "black", alpha = 0.5) +
    geom_gate(gate, colour = "black", alpha = 0.7) +
    geom_stats(adjust = 0.9) +
    theme_pubr() +
    theme(legend.position = "none",
          title = element_blank())
  gg <- as.ggplot(gg)

  if(fontFix == TRUE){
    gg <- gg +
      theme(text = element_text(size = 10, family = "TT Arial"),
            title = element_text(size = 10))
  }

  if(flowFix == TRUE){
    gg <- gg +
      theme(strip.background = element_blank(),
            strip.text = element_blank(),
            title = element_blank(),
            axis.text = element_blank())
  }
  return(gg)
}
#'
#' @examples
#'
#' if(interactive()){
#'   plotFACSExample(gs[[1]], "FSC-A", "SSC-A", "Lymphocytes")
#'   # Creates a nice plot of the Lymphocytes gate from the first GH in gs for
#'   # use in publications.
#' }
