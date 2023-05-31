#' Blank panel remover.
#'
#' This function removes the blank squares (diagonal and upper half) from pcaGrid and plotLoadingsGrid.
#' @param g the ggpairs object (pcaGrid or plotLoadingsGrid plot).

gPairsLower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1

  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1

  g
}
