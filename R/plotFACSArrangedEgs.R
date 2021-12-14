#' Arrange Example FACS Plots in stacks of Two
#'
#' This function creates the stacked example plots used throughout the
#' manuscript. Generally, these compare one WT and one IL23RKO example, but it
#' is extensible to other grids (such as the 2x2 used in the antibody graphs) by
#' simply passing a longer list of GatingHierarchies. By default, this also
#' applies biex scaling to both axes, and does NOT currently support modifying
#' their params from the default, so if that becomes important later I'll need
#' to add it in.
#'
#' @param egList List of GatingHierarchies to plot in order given (so start with
#'   WT!)
#' @param x String specifying x-axis parameter, as either channel or marker name
#' @param y String specifying y-axis parameter, as either channel or marker name
#' @param gate String specifying gate or gates (as character vector in that
#'   case)
#' @param ... Additional args to pass to plotFACSExample
#' @param ncol Numeric specifying the number of expected columns in the arranged
#'   output
#' @param xlim Length-two numeric vector giving coord_cartesian the x-axis
#'   limits
#' @param ylim Length-two numeric vector giving coord_cartesian the y-axis
#'   limits
#' @param biexTrans String specifying whether a flowjo-style biex transform
#'   should be applied to "both" (default), "x", or "y" axes. Any other input
#'   results in no transform.
#'
#' @return A ggarrange'd collection of ggplot objects
#'
#' @export
#'
plotFACSArrangedEgs <- function(egList, x, y, gate, ..., ncol = 1, xlim = c(-500, 20000), ylim = c(-500, 20000), biexTrans = "both"){
  egs <- purrr::map(egList,
             \(data) plotFACSExample(data, x, y, gate, ...) +
               ggplot2::coord_cartesian(xlim, ylim))
  if(biexTrans == "both"){
    egs <- purrr::map(egs, \(gg) gg + ggcyto::scale_x_flowjo_biexp() + ggcyto::scale_y_flowjo_biexp())
  }else if(biexTrans == "x"){
    egs <- purrr::map(egs, \(gg) gg + ggcyto::scale_x_flowjo_biexp())
  }else if(biexTrans == "y"){
    egs <- purrr::map(egs, \(gg) gg + ggcyto::scale_y_flowjo_biexp())
  }

  plots <- ggpubr::ggarrange(plotlist = egs, nrow = 2, ncol = ncol)
  return(plots)
}

#' @examples
#' path_to_gs <- system.file("extdata", "example.gs", package = "manuscriptIL23R")
#' gs <- flowWorkspace::load_gs(path_to_gs)
#' egList <- list(gs[[1]], gs[[2]])
#' plotFACSArrangedEgs(egList, "IFNg", "CD8a", "CD8 IFNg")
