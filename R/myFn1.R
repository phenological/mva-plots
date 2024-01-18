#' Function 1 for ggpairs.
#'
#' Shell function for scatter plot of plotscores with ellipses for the pcaGrid function. Any aesthetic parameters are included in the pcaGrid function.
#'
#' @param data The data used to plot.
#' @param mapping mapping for ggplot.
#' @param method the stat_ellipse method for plotting.

#function for the lower half of the PCA grid, the plotscores with ellipses set to the same parameter colour is set to.

  myFn1 <- function(data, mapping, method = "stat_ellipse"){
    p <- ggplot(data = data, mapping = mapping) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  return(p)
}
