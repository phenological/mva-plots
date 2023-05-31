#' Function 2 for ggpairs.
#'
#' Shell function for loadings with text points for the plotlLoadingsGrid function. Any aesthetic parameters are included in the plotlLoadingsGrid function.
#'

myFn2 <- function(data, mapping){
  p <- ggplot(data = data, mapping = mapping) +
    scale_x_continuous(limits = symmetric_limits) +
    scale_y_continuous(limits = symmetric_limits) +
    theme_minimal()+
    theme(text = element_text(size = 3))+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  p
}

#backup function for symmetric_limits without the use of the ggpmisc package.
# symmetric_limits <- function (x)
# {
#   max <- max(abs(x))
#   c(-max, max)
# }
