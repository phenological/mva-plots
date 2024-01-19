#' Simple STOCSY implementation
#'
#' @param x numeric, x scale e.g. ppm
#' @param Y numeric, spectra matrix, spectra in rows
#' @param driver numeric, reference to compute correlation. If length(driver)
#' matches the number of spectra, it is interpreted as the vector of
#' observations of the driver variable. Otherwise, the first element of driver
#' (`driver[1]`) is interpreted as the chemical shift coordinate of the driver.
#' @param roi numeric, upper and lower limit of the Region Of Interest to be
#' correlated. The whole spectra range by default.
#' @param plot, boolean, whether to return the plot of the STOCSY (TRUE,
#' default) or the numbers
#' @param breaks numeric, number of breaks in the x axis or the STOCSY plot
#' @return if plot=TRUE, a ggplot2 object. Otherwise, a data.frame with the
#' correlations and covariances of Y with the driver and the corresponding ppm
#' scale, restricted to the given roi. The "driver" (either a cshift or a vector
#'  of intensities) is stored as an attribute of the data.frame.

stocsy <- function(x, Y, driver, roi = range(x), plot = TRUE, breaks = 10){
  #Parse the driver, which may be a chem. shift, into a driver vector
  if (is.numeric(driver)){
    if (length(driver) == dim(Y)[1]){
      driverV <- driver
    } else{
      driver <- driver[1]
      if (driver >= min(x) & driver <= max(x)){
        driverV <- Y[,which.min(abs(x-driver))]
      }
      else{
        cat(crayon::red("mvaPlots::stocsy >>", "Driver shift is not in the spectral range\n"))
        stop()
      }
    }
  }
  else{
    cat(crayon::red("mvaPlots::stocsy >>", "Non-numeric driver\n"))
    stop()
  }
  #Filter to roi
  fi <- x >= roi[1] & x <= roi[2]
  x <- x[fi]
  Y <- Y[,fi]
  #Compute covariance and correlation
  # cs <- which.min(abs(x-cshift))
  cr <- abs(as.vector(cor(Y,driverV)))
  cv <- as.vector(cov(Y,driverV))
  #Make return data.frame
  aDF <- data.frame(ppm = x, corr = cr, covar = cv)
  attr(aDF,"driver") <- driver
  #Plot if required
  if (plot){
    return(plotStocsy(aDF, breaks = breaks))
  }
  return(aDF)
}

#' Simple STOCSY visualization
#'
#' @param aDF, list, STOCSY results as returned by mvaPlots::stocsy()
#' @param breaks numeric, number of breaks in the x axis
#' @return a ggplot of the STOCSY, with ppm in the horizontal axis, covariance
#' in the vertical axis, and correlation in the color scale
#' @export
plotStocsy <- function(aDF, breaks = 10){

 continuousPalette<- c(
                        "#0000CC",
                        "#0000FF",
                        "#0055FF",
                        "#00AAFF",
                        "#00FFFF",
                        "#2BFFD5",
                        "#55FFAA",
                        "#80FF80",
                        "#AAFF55",
                        "#D4FF2B",
                        "#FFFF00",
                        "#FFAA00",
                        "#FF5500",
                        "#FF0000",
                        "#CC0000"
                        )


  driver <- attr(aDF, "driver")

  p <- ggplot(data = aDF,
              aes(x = ppm,
                  y = covar,
                  colour = corr)) +
        geom_line() +
        scale_x_reverse(n.breaks = breaks)

  p <- p +
       scale_colour_gradientn(colors = continuousPalette,
                              limits = c(0,1),
                              name = "|R|")
  if(!is.null(driver) & length(driver) == 1){
    p <- p +
         labs(caption = paste("Driver c. shift:", driver, "ppm"))

    if (driver >= min(aDF$ppm) & driver <= max(aDF$ppm))
      p <- p +
           geom_vline(xintercept = driver,
                      linetype = 2)


  }
  return(p)
}
