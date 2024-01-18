#' symmetric_limits for ggplot object
#'
#' replaces the need for ggpmisc package
#' @param x x coordinates.

symmetric_limits <- function (x)
{
  max <- max(abs(x))
  c(-max, max)
}
