#labelling outliers

outlierLabels <- function(thresh = thresh, output = output, pcData = output$data, pcaGridPlot = pcaGridPlot, hotelStat = hotelStat, ellipseStat = ellipseStat, ellipseStat2 = ellipseStat2){

  #ellipse options

  X <- as.matrix(pcData$pcdf[,1:thresh])

  # Sample size
  n <- nrow(X)

  hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(0.95, 2, n - 2)

  #create ellipses and outlier tables for output
  for(i in 1:thresh)
  {
    for(j in 1:thresh)
    {
      if(j>i)
      {
        #set up changing the individual plots in the grid
        temp <- pcaGridPlot[j, i]

        #Hotelling's T2

        if(hotelStat == TRUE){

          ##for outliers
          rx <- sqrt(var(pcData$pcdf[i]) * hotFisN)
          ry <- sqrt(var(pcData$pcdf[j]) * hotFisN)

          insideOut <- list((pcData$pcdf[i]^2)/(rx^2) + (pcData$pcdf[j]^2)/(ry^2))
          idx <- which(insideOut[[1]] > 1)

          ##for the plot
          temp <- temp + geom_text(data=pcData$pcdf[idx,], aes(label = sampleID))
        }

        pcaGridPlot[j, i] <- temp
      }
    }
  }

  tempPGP <- pcaGridPlot


  return(output)
}
