#' Simple STOCSY implementation
#' 
#' @param x numeric, x scale e.g. ppm
#' @param Y numeric, spectra matrix, spectra in rows
#' @param cshift numeric, driver chemical shift i.e. reference chem. shift value to compute correlation
#' @param roi numeric, upper and lower limit of the Region Of Interest to be plotted
#' @param breaks numeric, number of breaks in the x axis
#' @return a ggplot2 object with covariances with the driver c. shift plotted in the vertical axis , lines coloured by the absolute value of correlation to the driver c. shift, and a dotted black line marking the driver shift if it is within the roi
#' @import ggplot2
#' @importFrom colorRamps matlab.like2
#' @export
stocsy <- function(x,Y,cshift,roi,breaks=10){
  cs <- which.min(abs(x-cshift))
  cr <- abs(as.vector(cor(Y,Y[,cs])))
  cv <- as.vector(cov(Y,Y[,cs]))
  if (!missing(roi)){
    fi <- x >= roi[1] & x <= roi[2]
    x <- x[fi]
    cr <- cr[fi]
    cv <- cv[fi]
  }
  df <- data.frame( ppm = x, cr = cr, covariance = cv)
  p <- ggplot(df
              ,aes(x=ppm
                   ,y=covariance
                   ,colour=cr)) + geom_line() + scale_x_reverse(n.breaks=breaks)
  p <- p + scale_colour_gradientn(colors = colorRamps::matlab.like2(10)
                                  , limits=c(0,1)
                                  , name="|R|") + labs(caption=paste("Driver c. shift:",cshift,"ppm"))
  if (cshift >= min(x) & cshift <= max(x))
    p + theme_bw() + geom_vline(xintercept = cshift,linetype = 2)
  else
    p
}