#' Grid of PCAs
#'
#' Grid of PCAs with ellipses up to the threshold number, using the GGally:ggpairs function.
#'
#' Any aesthetic set to a numeric value will not appear in the legend. Any aesthetic set to a variable requires the legend title be specified.
#'
#' @param screeCumulativeThresholdObject An object computed from the screeCumulativeThreshold function.
#' @param CO A metadata variable represented by colour in the pca grid. Must be defined by as a variable. Must have a discrete scale.
#' @param SZ Either a numeric or a metadata variable represented by size in the pca grid. Default value of 1.
#' @param AL Either a numeric or a metadata variable represented by alpha in the pca grid. Default value of 0.5.
#' @param SH Either a numeric or a metadata variable represented by shape in the pca grid. Default is NULL.
#' @param COtitle Character/s representing the Colour legend title.
#' @param SZtitle Character/s representing the Size legend title.
#' @param ALtitle Character/s representing the Alpha legend title.
#' @param SHtitle Character/s representing the Shape legend title.
#'
#' @return grid of PCAs up to the threshold number.
#' @examples
#egA <- pcResults(BIOspcglyc[,1:8], annotation = BIOspcglyc[,-1:-8])
#egB <- screeCumulativeThreshold(pcResultsObject = egA, cutoff = 95)
#pcaGrid(screeCumulativeThresholdObject = egB, CO = "covid_status", SZ = 0.5, COtitle = "Covid Status")

#PCA grid with ggpairs

pcaGrid <-function(screeCumulativeThresholdObject, CO, SH = NULL, SZ = 1, AL = 0.5, COtitle, SHtitle = "NULL", SZtitle = "NULL", ALtitle = "NULL", gridTitle = "PCA Grid", hotelStat = FALSE, ellipseStat = FALSE, ellipseStat2 = "NULL", outlierLabels = FALSE, outlierID = FALSE){

  output <- plotInput(screeCumulativeThresholdObject, CO, SH, SZ, AL)
  thresh <- output$data$threshold
  test <- pcaGridLegend(screeCumulativeThresholdObject, x = "PC1", y = "PC2", CO, SH, AL, SZ, COtitle, SHtitle, SZtitle, ALtitle)

#Loop for creating titles of "PC(explained variance %)"

  title <- list()
  for (i in 1:thresh) {
    title[[i]] <- paste0('PC', i, ' (', round(output$data$pcSum$`Proportion of Variance`[i], 1), '%)')
  }
  title<-unlist(title)

#combinations for which aesthetics are set to a variable and those set to a single value. Colour is always defined by a variable.

  gp <- if(class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
          geom_point(aes(colour = output$CO, shape = output$SH), size = output$SZ, alpha = output$AL)
        } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
          geom_point(aes(colour = output$CO, size = output$SZ), shape = output$SH, alpha = output$AL)
        } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
          geom_point(aes(colour = output$CO, alpha = output$AL), shape = output$SH, size = output$SZ)
        } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
          geom_point(aes(colour = output$CO, shape = output$SH, size = output$SZ), alpha = output$AL)
        } else if (class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
          geom_point(aes(colour = output$CO, shape = output$SH, alpha = output$AL), size = output$SZ)
        } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
          geom_point(aes(colour = output$CO, size = output$SZ, alpha = output$AL), shape = output$SH)
        } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
          geom_point(aes(colour = output$CO, shape = output$SH, size = output$SZ, alpha = output$AL))
        } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
          geom_point(aes(colour = output$CO), shape = output$SH, size = output$SZ, alpha = output$AL)
        }

#create the PCA grid
  gridTitle = gridTitle

  pcaGridPlot<-GGally::ggpairs(data = output$data$pcdf[,1:thresh],
                               columnLabels = c(title),
                               title = gridTitle,
                               diag="blank",
                               upper="blank",
                               #upper=list(continuous = my_fn1),
                               lower=list(continuous = myFn1),
                               legend = grab_legend(test),
                               progress = F,
                               switch = "both") +
                               gp +
                               #stat_ellipse(aes(group=interaction(output$CO, color=output$CO), color=output$CO))+
                               theme_bw() +
                               theme(strip.background = element_rect(fill = "white"),
                               axis.text.x = (element_text(size=rel(0.7), angle=0)),
                               axis.text.y = (element_text(size=rel(0.7), angle=0)),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(fill = NA,colour = "grey35"))

#ellipses added
  output2 <- ellipseOptions(thresh = thresh, output = output, pcData = output$data, pcaGridPlot = pcaGridPlot, hotelStat = hotelStat, ellipseStat = ellipseStat, ellipseStat2 = ellipseStat2, outlierLabels = outlierLabels, outlierID = outlierID)

#remove empty grid spaces (lower and diagonal)
  pcaGridPlot <- gPairsLower(output2$tempPGP)

#append the completed plot to the output
  output2$plots <- append(output2$plots, list(pcaGridPlot = pcaGridPlot))

#remove tempPGP (the pcaGridPlot without blank squares removed)
  output2 <- list.remove(output2, 'tempPGP')

  return(output2)
}
