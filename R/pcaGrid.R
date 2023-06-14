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

# pcaGrid <-function(screeCumulativeThresholdObject, optns=list()){
#
#   #colour
#  # if("CO" %in% optns){
#  #    if(is("CO")[1] == "character"){
#  #      CO <- screeCumulativeThresholdObject$data$pcdf[,"CO"]
#  #    if(is(COtitle)[1] == "character"){
#  #      COtitle <- COtitle}
#  #      else if(!("COtitle"%in% optns)){
#  #        COtitle <- "Colour"
#  #        cat(yellow("Using default colour legend title", "!\n"))}
#  #    } else if(is("CO")[1] == "numeric"){
#  #      CO <- CO
#  #    }
#  #    else{
#  #      CO <- "red"
#  #    }
#

# if("CO" %in% names(optns)){
#   if(is("CO")[1]=="character"){
#     #CO <- screeCumulativeThresholdObject$data$pcdf[,optns$CO]
#   }else if(is((optns$CO)[1])=="numeric"){
#     CO <- optns$CO[1]
#   }
# } else{CO<-"black"}
#
# #shape
# if("SH" %in% optns){
#   if(is(SH)[1] == "chatacter"){
#     SH = screeCumulativeThresholdObject$data$pcdf[,SH]
#     if("SHtitle" %in% optns){
#       SHtitle = SHtitle
#     }else if(!("SHtitle" %in% optns)){
#       SHtitle = "Shape"
#       cat(yellow("Using default Shape legend title", "!\n"))
#     }
#   }else if(is(SH)[1]=="numeric"){
#     SH = SH
#   }
# }else if (!("SH" %in% optns)){
#   SH = "circle"
# }
#
#   #Size
#   if("SZ" %in% names(optns)){
#     if(is("SZ")[1] == "character"){
#       SZ = screeCumulativeThresholdObject$data$pcdf[,optns$SZ]
#       # if("SZtitle" %in% names(optns)){
#       #   SZtitle = optns$SZtitle
#       # }else if(!("SZ" %in% names(optns))){
#       #   SZtitle = "Size"
#       #   cat(yellow("Using default Size legend title", "!\n"))
#       # }
#     }else if(is("SZ")[1] == "numeric") {
#       "SZ" = as.numeric(optns$SZ[1])
#     }
#   }
# if(!("SZ" %in% names(optns))){
#     SZ = 3
#   }
#
#   #Alpha
#   if("AL" %in% optns){
#     if(is(AL)[1] == "character"){
#       AL = screeCumulativeThresholdObject$data$pcdf[,AL]
#       if("ALtitle" %in% optns){
#         ALtitle = ALtitle
#       }else if(!("AL" %in% optns)){
#         ALtitle="Alpha"
#         cat(yellow("Using default ALtitle Alpha legend title", "!\n"))
#       }
#     }else if(is(AL)[1]=="numeric"){
#       AL = AL
#     }
#   }else if(!("AL"%in% optns)){
#     AL = 0.5
#   }
#
# #Grid title (working)
# if("gridTitle" %in% names(optns)){
#   if(is("gridTitle")[1] =="character"){
#     gridTitle = optns$gridTitle
#    }
# }else{
#   gridTitle <- "PCA Grid"
#   cat(yellow("Using default gridTitle", "!\n"))
#  }
#
# #hotelling's ellipse
#   if("hotelStat" %in% optns){
#     if (is(hotelStat)[1] == "logical"){
#       hotelStat <- (hotelStat)
#     }
#   }else if(!("hotelStat"%in% optns)){
#     hotelStat <- FALSE
#   }
#
# #ellipseStat
#   if("ellipseStat" %in% optns){
#     if (is(ellipseStat)[1] == "logical"){
#       ellipseStat <- ellipseStat
#     }
#   }else if(!("ellipseStat"%in% optns)){
#     ellipseStat <- FALSE
#   }
#
# #ellipseStat2
#   if("ellipseStat2" %in% optns){
#     if(ellipseStat2 == "T" | ellipseStat2 == "NORM"){
#       ellipseStat2 <- ellipseStat2
#     }
#   }else if(!("ellipseStat2" %in% optns)){
#     ellipseStat2 <- "NULL"
#   }
#
# #outlierLabels
#   if("outlierLabels" %in% optns){
#     if(is(outlierLabels) == "logical"){
#       outlierLabels <- outlierLabels
#     }
#   }else if(!("outlierLabels" %in% optns)){
#     outlierLabels <- FALSE
#   }
#
# #outlierID
#   if("outlierID" %in% optns){
#     if(is(outlierID)=="character"){
#       oulierID <- outlierID
#     }
#   }else if(!("outlierID" %in% optns)){
#     oulierID <- FALSE
#   }
#
# #threshold (numver of PCAs) (working)
# if("thresh" %in% names(optns)){
#     if(is("thresh")[1]=="numeric"){
#       thresh <- optns$thresh[1]
#     }
#   }else{
#     thresh <- screeCumulativeThresholdObject$data$threshold
#   }
#
#   #test <- pcaGridLegend(screeCumulativeThresholdObject, x = "PC1", y = "PC2", CO, SH, AL, SZ, COtitle, SHtitle, SZtitle, ALtitle)
#
#   #Loop for creating titles of "PC(explained variance %)"
#
#   title <- list()
#   for (i in 1:thresh) {
#     title[[i]] <- paste0('PC', i, ' (', round(screeCumulativeThresholdObject$data$pcSum$`Proportion of Variance`[i], 1), '%)')
#   }
#   title<-unlist(title)
#
#   #combinations for which aesthetics are set to a variable and those set to a single value. Colour is always defined by a variable.
#
#   # gp <- if(class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
#   #   geom_point(aes(colour = CO, shape = SH), size = SZ, alpha = AL)
#   # } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
#   #   geom_point(aes(colour = CO, size = SZ), shape = SH, alpha = AL)
#   # } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
#   #   geom_point(aes(colour = CO, alpha = AL), shape = SH, size = SZ)
#   # } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
#   #   geom_point(aes(colour = CO, shape = SH, size = SZ), alpha = AL)
#   # } else if (class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
#   #   geom_point(aes(colour = CO, shape = SH, alpha = AL), size = SZ)
#   # } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
#   #   geom_point(aes(colour = CO, size = SZ, alpha = AL), shape = SH)
#   # } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
#   #   geom_point(aes(colour = CO, shape = SH, size = SZ, alpha = AL))
#   # } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
#   #   geom_point(aes(colour = CO), shape = SH, size = SZ, alpha = AL)
#   # }
#
#  gp <- if(class(CO) == "character" & class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
#     geom_point(aes(colour = CO, shape = SH), size = SZ, alpha = AL)
#   } else if (class(CO) == "character" & class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
#     geom_point(aes(colour = CO, size = SZ), shape = SH, alpha = AL)
#   } else if (class(CO) == "character" & class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
#     geom_point(aes(colour = CO, alpha = AL), shape = SH, size = SZ)
#   } else if (class(CO) == "character" & class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
#     geom_point(aes(colour = CO, shape = SH, size = SZ), alpha = AL)
#   } else if (class(CO) == "character" & class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
#     geom_point(aes(colour = CO, shape = SH, alpha = AL), size = SZ)
#   } else if (class(CO) == "character" & class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
#     geom_point(aes(colour = CO, size = SZ, alpha = AL), shape = SH)
#   } else if (class(CO) == "character" & class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
#     geom_point(aes(colour = CO, shape = SH, size = SZ, alpha = AL))
#   } else if (class(CO) == "character" & class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
#     geom_point(aes(colour = CO), shape = SH, size = SZ, alpha = AL)
#   } else if(class(CO) == "numeric" & class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
#     geom_point(aes(shape = SH), colour = CO, size = SZ, alpha = AL)
#   } else if (class(CO) == "numeric" & class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
#     geom_point(aes(size = SZ), colour = CO, shape = SH, alpha = AL)
#   } else if (class(CO) == "numeric" & class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
#     geom_point(aes(alpha = AL), colour = CO, colour = CO, shape = SH, size = SZ)
#   } else if (class(CO) == "numeric" & class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
#     geom_point(aes(shape = SH, size = SZ), colour = CO, alpha = AL)
#   } else if (class(CO) == "numeric" & class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
#     geom_point(aes(shape = SH, alpha = AL), colour = CO, size = SZ)
#   } else if (class(CO) == "numeric" & class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
#     geom_point(aes(size = SZ, alpha = AL), colour = CO, shape = SH)
#   } else if (class(CO) == "numeric" & class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
#     geom_point(aes(shape = SH, size = SZ, alpha = AL), colour = CO)
#   } else if (class(CO) == "numeric" & class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
#     geom_point(aes(),colour = CO, shape = SH, size = SZ, alpha = AL)
#   }
#
#   #create the PCA grid
#
#   pcaGridPlot<-GGally::ggpairs(data = screeCumulativeThresholdObject$data$pcdf[,1:thresh],
#                                columnLabels = c(title),
#                                title = gridTitle,
#                                diag="blank",
#                                upper="blank",
#                                #upper=list(continuous = my_fn1),
#                                lower=list(continuous = myFn1),
#                                #legend = grab_legend(test),
#                                progress = F,
#                                switch = "both") +
#                                gp +
#                                 #stat_ellipse(aes(group=interaction(output$CO, color=output$CO), color=output$CO))+
#                                 theme_bw() +
#                                 theme(strip.background = element_rect(fill = "white"),
#                                 axis.text.x = (element_text(size=rel(0.7), angle=0)),
#                                 axis.text.y = (element_text(size=rel(0.7), angle=0)),
#                                 panel.grid.major = element_blank(),
#                                 panel.grid.minor = element_blank(),
#                                 panel.border = element_rect(fill = NA,colour = "grey35"))
#
#   #ellipses added
#   output2 <- ellipseOptions(thresh = thresh, output = screeCumulativeThresholdObject, pcData = screeCumulativeThresholdObject$data, pcaGridPlot = pcaGridPlot, hotelStat = hotelStat, ellipseStat = ellipseStat, ellipseStat2 = ellipseStat2, outlierLabels = outlierLabels, outlierID = outlierID)
#
#   #remove empty grid spaces (lower and diagonal)
#   pcaGridPlot <- gPairsLower(output2$tempPGP)
#
#   #append the completed plot to the output
#   output2$plots <- append(output2$plots, list(pcaGridPlot = pcaGridPlot))
#
#   #remove tempPGP (the pcaGridPlot without blank squares removed) #uses rlist package, instead use two lines without package
#   idx <- which(names(output2)=="tempPGP")
#   output2 <- output2[-idx]
#   #output2 <- list.remove(output2, 'tempPGP')
#
#   return(output2)
# }
#
   #PCA grid with ggpairs

#add option to manually choose how many PC's
pcaGrid <-function(screeCumulativeThresholdObject,
                   CO = 10,
                   SH = "NULL",
                   SZ = 1,
                   AL = 0.5,
                   COtitle = "Colour",
                   SHtitle = "NULL",
                   SZtitle = "NULL",
                   ALtitle = "NULL",
                   gridTitle = "PCA Grid",
                   hotelStat = FALSE,
                   ellipseStat = FALSE,
                   ellipseStat2 = "NULL",
                   outlierLabels = FALSE,
                   outlierID = FALSE){


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

#remove tempPGP (the pcaGridPlot without blank squares removed) #uses rlist package, instead use two lines without package
  idx <- which(names(output2)=="tempPGP")
  output2 <- output2[-idx]
  #output2 <- list.remove(output2, 'tempPGP')

  return(output2)
}
