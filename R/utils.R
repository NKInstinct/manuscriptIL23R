#flowFix -----------------------------------------------------------------------
flowFix <- function(gg){
  gg <- gg + ggplot2::theme(strip.background = ggplot2::element_blank(),
                            strip.text = ggplot2::element_blank(),
                            title = ggplot2::element_blank(),
                            axis.text = ggplot2::element_blank())
  return(gg)
}

# fontFix ----------------------------------------------------------------------
fontFix <- function(gg){
  gg <- gg + ggplot2::theme(text = ggplot2::element_text(size = 10,
                                                         family = "TT Arial"),
             title = ggplot2::element_text(size = 10))
  return(gg)
}

# statBar ----------------------------------------------------------------------
statBar <- function(gg, statbar, ...){
  gg <- gg +
    ggpubr::stat_compare_means(size = 6,
                               label = "p.signif",
                               label.x.npc = "center",
                               label.y = statbar,
                               ...) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.30, xend = 1.75,
                                       y = statbar-(statbar/50),
                                       yend = statbar-(statbar/50)))

  return(gg)
}
