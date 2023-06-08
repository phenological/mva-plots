ellipseOptions <- function(thresh = thresh, output = output, pcData = output$data, pcaGridPlot = pcaGridPlot, hotelStat = hotelStat, ellipseStat = ellipseStat, ellipseStat2 = ellipseStat2, outlierLabels = outlierLabels, outlierID = outlierID){

# outlierID <- if(class(outlierID) == "logical"){
#     pcData$pcdf$outlierID = rep_len(NA, nrow(pcData$pcdf))
#   }else if (class(outlierID) == "character") {
#     outlierID = pcData$pcdf[,outlierID]
#   }
if(outlierLabels == "hotel" | outlierLabels== "T" | outlierLabels== "NORM"){
pcData$pcdf$outlierID <- pcData$pcdf[,outlierID]
}
#ellipse options

  X <- as.matrix(pcData$pcdf[,1:thresh])

  # Sample size
  n <- nrow(X)

  hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(0.95, 2, n - 2)

  outlierHotel <- list()
  outlierStatT <- list()
  outlierStatNorm <- list()

#create ellipses and outlier tables for output
  for(i in 1:thresh)
  {
    for(j in 1:thresh)
    {
      if(j>i)
      {
        #set up names for outlier list, eg PC1vPC2
        placeHolder <- paste0("PC", i, "vPC", j)

        #set up changing the individual plots in the grid
        temp <- pcaGridPlot[j, i]

        #Hotelling's T2

        if(hotelStat == TRUE){

          ##for the plot
          temp <- temp + gg_circle(rx = sqrt(var(pcData$pcdf[i]) * hotFisN),
                                   ry = sqrt(var(pcData$pcdf[j]) * hotFisN),
                                   xc = 0, yc = 0)
          ##for outliers
          rx <- sqrt(var(pcData$pcdf[i]) * hotFisN)
          ry <- sqrt(var(pcData$pcdf[j]) * hotFisN)

          insideOut <- list((pcData$pcdf[i]^2)/(rx^2) + (pcData$pcdf[j]^2)/(ry^2))
          idx <- which(insideOut[[1]] > 1)

          outlierIDX <- pcData$pcdf[idx,-1:-ncol(pcData$scores)]

          new_list <- setNames(list(outlierIDX), placeHolder)
          outlierHotel <- append(outlierHotel, new_list)

          ##label outliers
          if(hotelStat == TRUE & outlierLabels == "hotel"){
          temp <- temp + geom_text(data=pcData$pcdf[idx,], aes(label = outlierID), size=2, hjust = 0, vjust = 0)
          }
        }

        #stat_ellipse with type t method

        if(ellipseStat2 == "T"){

          ##for the plot
          temp <- temp + stat_ellipse( type = "t", geom = "polygon", fill = "gray", level = 0.95, alpha = .5, linetype = 2)

          ##for the outliers
          build <- ggplot_build(temp)$data
          points <- build[[3]]
          ell <- build[[4]]

          ##ensure correct element from build is selected
          if(hotelStat == TRUE)
          {
            ell <- build[[5]]
          }

          ## Find which points are inside the ellipse
          dat <- list(in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y)))
          idx <-which(dat$in.ell == FALSE)
          outlierIDX <- pcData$pcdf[idx, -1:-ncol(pcData$scores)]
          new_list <- setNames(list(outlierIDX), placeHolder)
          outlierStatT <- append(outlierStatT, new_list)

          ##label outliers
          if(ellipseStat2 == "T" & outlierLabels == "T"){
            temp <- temp + geom_text(data=pcData$pcdf[idx,], aes(label = outlierID), size=2, hjust = 0, vjust = 0)
          }

        }

        #stat_elipse with norm method
          if(ellipseStat2 == "NORM"){
            ##for plot
            temp <- temp + stat_ellipse( type = "norm", geom = "polygon", fill = "gray", level = 0.95, alpha = .5, linetype = 2)

            ##for the outliers
            build <- ggplot_build(temp)$data
            points <- build[[3]]
            ell <- build[[4]]

            ##ensure correct element from build is selected
            if(hotelStat == TRUE)
            {
              ell <- build[[5]]
            }

            ## Find which points are inside the ellipse
            dat <- list(in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y)))
            idx <-which(dat$in.ell == FALSE)
            outlierIDX <- pcData$pcdf[idx, -1:-ncol(pcData$scores)]
            new_list <- setNames(list(outlierIDX), placeHolder)
            outlierStatNorm <- append(outlierStatNorm, new_list)

            ##label outliers
            if(ellipseStat2 == "NORM" & outlierLabels == "NORM"){
              temp <- temp + geom_text(data=pcData$pcdf[idx,], aes(label = outlierID), size = 2, hjust = 0, vjust = 0)
            }
            }
          #stat_ellipse for separation

          if(ellipseStat == TRUE){
            temp <- temp + stat_ellipse(aes(group = interaction(output$CO, color = output$CO), color = output$CO))
          }


        pcaGridPlot[j, i] <- temp
      }
    }
  }

  output$data <- append(output$data, list(outlierHotel = outlierHotel,
                                          outlierStatT = outlierStatT,
                                          outlierStatNorm = outlierStatNorm))
  tempPGP <- pcaGridPlot
  output <- append(output, list(tempPGP = tempPGP))

  return(output)
}

# a <- append(a, list(threshold = t))
# output$data <- append(output$data, list(outlierHotel = outlierHotel,
#                                         outlierStat = outlierStat))
# output$plots <- append(output$plots, list(pcaGridPlot = pcaGridPlot))
#
# return(output)
#
# return(list(data = a,
#             plots = list(screeCumulativeThreshold = screeCumulativeThresholdPlot,
#                          combinedScreeCumulative = combinedScreeCumulative,
#                          screeplot = screeplot,
#                          cumulativeVariance = cumulativeVariance,
#                          thresholdTable = thresholdTable)))

# #list of outliers for hotelling's
# for(i in 1:thresh)
# {
#   for(j in 1:thresh)
#   {
#     if(j>i)
#     {
#       outliers <- list()
#
#       if (hotelStat == TRUE){
#       rx <- sqrt(var(output$data$pcdf[i]) * hotFisN)
#       ry <- sqrt(var(output$data$pcdf[j]) * hotFisN)
#
#       list(insideOut = (output$data$pcdf[i]^2)/(rx^2) + (output$data$pcdf[j]^2)/(ry^2))
#       idx <-which(insideOut > 1)
#
#       outliers[i,j] <- output$data$pcdf[idx,-i:-j]
#       }
#       if(ellipseStat2 == "TRUE"){
#       temp <- blankPlot[j, i]
#       build <- ggplot_build(temp)$data
#       points <- build[[1]]
#       ell <- build[[2]]
#
#       dat <- data.frame(
#           points[1:2],
#           in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y))
#           )
#         idx <- which(dat$in.ell == FALSE)
#         output$data$pcdf[idx,]
#       }
#     }}}



#outliers for stat_ellipse t



# if (hotelStat == TRUE) {
#   rx = sqrt(var(a$pcdf$PC4) * hotFisN)
#   ry = sqrt(var(a$pcdf$PC5) * hotFisN)
#
#   list(insideOut = (a$pcdf$PC4^2)/(rx^2) + (a$pcdf$PC5^2)/(ry^2))
#   idx <-which(insideOut > 1)
#   BIOspcglyc[idx,]
#
# }
#
# if (ellipseStat2 == TRUE)
# build <- ggplot_build(p)$data
# points <- build[[1]]
# ell <- build[[2]]
#
# # Find which points are inside the ellipse, and add this to the data
# dat <- data.frame(
#   points[1:2],
#   in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y))
# )


#' @param x scores in the x direction
#' @param y scores in the y direction
#' @param alpha confidence level, default is 0.95 (95%)
#' @param len the amount of points to be generated to make the ellipses, default is 200

#' hotellings <- function(x, y, alfa=0.95, len=200) {
#'
#'   n <- length(x)
#'   A <- 2
#'
#'   #generate numbers from 0 to 2pi(2*pi*radius being the perimeter of the circle)
#'   mypi <- seq(0, 2 * pi, length=len)
#'
#'   #radius for x direction using the variance, f-statistic and df
#'   r1 <- sqrt(var(x) * qf(alfa, 2, n - 2) * (2*(n^2 - 1)/(n * (n - 2))))
#'
#'   #radius for y direction
#'   r2 <- sqrt(var(y) * qf(alfa, 2, n - 2) * (2*(n^2 - 1)/(n * (n - 2))))
#'
#'   #generate coordinates for ellipses
#'   cbind(r1 * cos(mypi) + mean(x), r2 * sin(mypi) + mean(y))
#' }
#' scores<- a$pcdf[,1:8]
#' confidence_ellipse95 <- hotellings(scores[,1], scores[,2], alfa = 0.95, len=200)
#' confidence_ellipse99 <- hotellings(scores[,1], scores[,2], alfa = 0.99, len=200)
#'
#' plot(scores[,1], scores[,2], xlim = c(min(confidence_ellipse99[,1]), max(confidence_ellipse99[,1])),
#'      ylim = c(min(confidence_ellipse99[,2]), max(confidence_ellipse99[,2])), ylab = 'Scores PC2', xlab = 'Scores PC1')
#' abline(h = 0, v = 0)
#' points(confidence_ellipse95, type = 'l', lty = 2, col = 'darkgreen')
#' points(confidence_ellipse99, type = 'l', lty = 2, col = 'red')
#'
#' #' @title Hotelling's T2 ellipse in 2D
#' #' @description This function is used to calculate hotellings T2 ellipse
#' #' @param x num vector descrbing dimension 1
#' #' @param y, num vector descrbing dimension 2
#' #' @param alpha num, probability level
#' #' @references Geladi, P and Kowalski, B.R. (1986), Partial least squares and regression: a tutorial. \emph{Analytica Chimica Acta}, 185, 1-17.
#' #' @return data.frame containing H.T2 ellipse cooredinates
#' #' @author \email{torben.kimhofer@@murdoch.edu.au}
#' #' @importFrom ellipse ellipse
#' #' @importFrom stats cov
#' #' @family NMR ++
#' #' @keywords internal
#' .hotellingsT2 <- function(x, y, alpha = 0.95) {
#'   SD <- cov(cbind(x, y), use = "complete.obs")
#'   el <- ellipse(SD, centre = colMeans(cbind(x, y), na.rm = TRUE), level = alpha)
#'   colnames(el) <- c("V1", "V2")
#'   xlim <- c(min((c(el[, 1], x))), max((c(el[, 1], x))))
#'   xlim <- xlim + c(diff(range(xlim)) * -0.05, diff(range(xlim)) * +0.05)
#'   ylim <- c(min((c(el[, 2], y))), max((c(el[, 2], y))))
#'   ylim <- ylim + c(diff(range(ylim)) * -0.05, diff(range(ylim)) * +0.05)
#'   df <- as.data.frame(el)
#'
#'   return(df)
#' }
#'
#' # Calculate Hotellings T2 elipse
#' df=.hotellingsT2(x=sc[,1], y=sc[,2])
#'
#' #kap<-kappa(obj@X)
#'
#' g <- ggplot() +
#'   geom_polygon(data = df, aes_string(x = "V1", y = "V2"), fill = NA, alpha = 0.4, colour='black', linetype=3) +
#'   geom_hline(yintercept = 0, colour = "gray70") +
#'   geom_vline(xintercept = 0, colour = "gray70")+
#'   theme_bw()+
#'   labs(caption = expression('Dashed line: Hotelling\'s T'^2~'ellipse ('*alpha*'=0.95)'))
#'
#' #generate coordinates for ellipse
#' library(HotellingEllipse)
#' ellipseCoord(data = pcdf, pcx = , pcy = , conf.limit = 95)
#'
#' ellipseCoord <- function(data, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200) {
#'
#'   # matrix of data
#'   X <- as.matrix(data)
#'
#'   # Sample size
#'   n <- nrow(X)
#'
#'   # Confidence limit
#'   alpha <- as.numeric(conf.limit)
#'
#'   # Number of points
#'   m <- as.numeric(pts)
#'   p <- seq(0, 2*pi, length.out = m)
#'
#'   # # Hotelling’s T-square limit
#'   Tsq_limit <- (2*(n-1)/(n-2))*stats::qf(p = alpha, df1 = 2, df2 = (n-2))
#'
#'   # Coordinate points
#'   rx <- sqrt(Tsq_limit*stats::var(X[, pcx]))
#'   ry <- sqrt(Tsq_limit*stats::var(X[, pcy]))
#'
#'   #coordinates
#'   res.coord <- tibble::tibble(
#'     x = rx*cos(p) + mean(X[, pcx], na.rm = TRUE),
#'     y = ry*sin(p) + mean(X[, pcy], na.rm = TRUE)
#'   )
#'
#'   return(res.coord)
#'
#'
#'
#'  #for PC's up to threshold, calc coordinates for hotelling's
#'   data= a$pcdf[,1:8]
#'   thresh = 3
#'   conf.limit = 0.95
#'   pts = 200
#'
#'   X <- as.matrix(data)
#'   n <- nrow(X)
#'   alpha <- as.numeric(conf.limit)
#'   m <- as.numeric(pts)
#'   p <- seq(0, 2*pi, length.out = m)
#'
#'   Tsq_limit <- (2*(n-1)/(n-2))*stats::qf(p = alpha, df1 = 2, df2 = (n-2))
#'
#'   radiusList <- list()
#'   for (i in 1:thresh){
#'     radiusList[[i]] <- sqrt(Tsq_limit*stats::var(X[, i]))
#'     }
#'
#'   radiusList<- unlist(radiusList)
#'
#'   xCoordList <- list()
#'   for (i in 1:thresh){
#'     xCoordList[[i]] <- (radiusList[i])*cos(p) + mean(X[, i], na.rm = TRUE)
#'   }
#'
#'   yCoordList <- list()
#'   for (i in 1:thresh){
#'     yCoordList[[i]] <- (radiusList[i])*sin(p) + mean(X[, i], na.rm = TRUE)
#'   }
#'
#'
#'   pca_scores %>%
#'     ggplot(aes(x = Dim.1, y = Dim.3)) +
#'     geom_point(aes(fill = Tsq), shape = 21, size = 3, color = "black") +
#'     scale_fill_viridis_c(option = "viridis") +
#'     geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), size = .5, linetype = "dotted") +
#'     geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .5, linetype = "dashed") +
#'     geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
#'     geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
#'     labs(
#'       title = "Scatterplot of PCA scores",
#'       subtitle = "PC1 vs. PC3",
#'       x = "PC1",
#'       y = "PC3",
#'       fill = "T2 stats",
#'       caption = "Figure 1"
#'     ) +
#'     theme_bw()
#'
#'
#'   myFn3 <- function(data, mapping, method="geom_ellipse"){
#'     p <- ggplot(data = data, mapping = mapping) +
#'       theme_minimal() +
#'       geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#'       geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#'       scale_color_brewer(palette = "Set2")
#'     return(p)
#'   }

