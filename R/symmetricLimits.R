# symmetric_limits for ggplot object replaces the need for ggpmisc package

symmetric_limits <- function (x)
{
  max <- max(abs(x))
  c(-max, max)
}
