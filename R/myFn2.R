#' Function 2 for ggpairs.
#'
#' Shell function for loadings with text points for the plotLoadingsGrid
#' function. Any aesthetic parameters are included in the plotlLoadingsGrid
#' function. Alternative function for symmetric_limits from the ggpmisc package.
#'
#' @param x x coordinates used to map where ellipses are placed
#' @keywords internal
#'
symmetricLimits <- function (x)
{
  max <- max(abs(x))
  c(-max, max)
}

myFn2 <- function(data, mapping){
  p <- ggplot(data = data, mapping = mapping) +
    scale_x_continuous(limits = symmetricLimits) +
    scale_y_continuous(limits = symmetricLimits) +
    theme_minimal()+
    theme(text = element_text(size = 3))+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  p
}

#

