#' Simple STOCSY implementation
#' 
#' @param x numeric, x scale e.g. ppm
#' @param Y numeric, spectra matrix, spectra in rows
#' @param driver numeric, reference to compute correlation. If length(driver) matches the number of spectra, it is interpreted as the vector of observations of the driver variable. Otherwise, driver[1] is interpreted as the chemical shift coordinate of the driver.
#' @param roi numeric, upper and lower limit of the Region Of Interest to be correlated. The whole spectra range by default.
#' @param plot, boolean, whether to make a plot of the STOCSY or not
#' @param breaks numeric, number of breaks in the x axis
#' @return a data.frame with the correlations and covariances of Y with the driver and the corresponding ppm scale, restricted to the given roi. The driver shift is written to the column names. Return value is invisible.
#' @import ggplot2
#' @importFrom colorRamps matlab.like2
#' @export
stocsy <- function(x, Y, driver, roi = range(x), plot = TRUE, breaks = 10){
  #Parse driver and driver chemical shift, if relevant
  if (is.numeric(driver)){
    if (length(driver) != dim(Y)[1]){
      cshift <- driver[1]
      driver <- Y[,which.min(abs(x-cshift))]
    } else cshift <- NULL
  }
  else{
    cat(crayon::red("mvaPlots::stocsy >>"
                    ,"Non-numeric driver\n"))
    stop()
  }
  #Filter to roi
  fi <- x >= roi[1] & x <= roi[2]
  x <- x[fi]
  Y <- Y[,fi]
  #Compute covariance and correlation
  # cs <- which.min(abs(x-cshift))
  cr <- abs(as.vector(cor(Y,driver)))
  cv <- as.vector(cov(Y,driver))
  #Make return data.frame
  df <- data.frame(ppm=x, corr=cr, covar=cv)
  #Plot if required
  if (plot){
    
    print(plotStocsy(df, breaks=breaks, cshift=cshift))
  }
  #Return result invisibly
  invisible(df)
}

#' Simple STOCSY visualization
#' 
#' @param stocsy, list, STOCSY results as returned by mvaPlots::stocsy()
#' @param cshift, optional, the driver chemical shift. If provided it is inserted as a caption in the figure.
#' @param breaks numeric, number of breaks in the x axis
#' @return a ggplot of the STOCSY, with ppm in the horizontal axis, covariance in the vertical axis, and correlation in the colour scale
#' @import ggplot2
#' @importFrom colorRamps matlab.like2
#' @export
plotStocsy <- function(df, cshift=NULL, breaks=10){
  p <- ggplot(df
              ,aes(x=ppm
                     ,y=covar
                     ,colour=corr)) + geom_line() + scale_x_reverse(n.breaks=breaks)
  p <- p + scale_colour_gradientn(colors = colorRamps::matlab.like2(10)
                                  , limits=c(0,1)
                                  , name="|R|")
  if(!is.null(cshift)){
    p <- p + labs(caption=paste("Driver c. shift:",cshift,"ppm"))
    if (cshift >= min(df$ppm) & cshift <= max(df$ppm))
      p + theme_bw() + geom_vline(xintercept = cshift,linetype = 2)
  }
  return(p)
}