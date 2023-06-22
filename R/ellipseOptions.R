ellipseOptions <- function(model = model, pcaGridPlot = pcaGridPlot, thresh = thresh, optns = optns){
#add in option for CI % level

#(ci = ci, thresh = thresh, model$data$pcdf = model$data$pcdf, pcaGridPlot = pcaGridPlot, ellipse = ellipse, outlierLabels = outlierLabels)

  if("ci" %in% names(optns)) {
    ci <- optns$ci
  } else{ci <- 0.95}

  if("outlierLabels" %in% names(optns)) {
    model$data$pcdf$outlierID <- optns$outlierLabels
  }

#ellipse options

  if("ellipse" %in% names(optns)){
  X <- as.matrix(model$data$pcdf[,1:thresh])

  # Sample size
  n <- nrow(X)

  hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)

  outlierHotellings <- list()
  outlierT <- list()
  outlierNormal <- list()

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

        if(optns$ellipse == "hotellings"){

          ##for the plot
          temp <- temp + gg_circle(rx = sqrt(var(model$data$pcdf[i]) * hotFisN),
                                   ry = sqrt(var(model$data$pcdf[j]) * hotFisN),
                                   xc = 0, yc = 0)
          ##for outliers
          rx <- sqrt(var(model$data$pcdf[i]) * hotFisN)
          ry <- sqrt(var(model$data$pcdf[j]) * hotFisN)

          insideOut <- list((model$data$pcdf[i]^2)/(rx^2) + (model$data$pcdf[j]^2)/(ry^2))
          idx <- which(insideOut[[1]] > 1)

          outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]

          new_list <- setNames(list(outlierIDX), placeHolder)
          outlierHotellings <- append(outlierHotellings, new_list)

          ##label outliers
          if(optns$ellipse == "hotellings" & "outlierLabels" %in% names(optns)){
          temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(label = outlierID), size=2, hjust = 0, vjust = 0)
          }
        }

        #stat_ellipse with type t method

        if(optns$ellipse == "T"){

          ##for the plot
          temp <- temp + stat_ellipse(type = "t", geom = "polygon", fill = "gray", level = ci, alpha = .5, linetype = 2)

          ##for the outliers
          build <- ggplot_build(temp)$data
          points <- build[[3]]
          ell <- build[[4]]

          # ##ensure correct element from build is selected
          # if(hotelStat == TRUE)
          # {
          #   ell <- build[[5]]
          # }

          ## Find which points are inside the ellipse
          dat <- list(in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y)))
          idx <-which(dat$in.ell == FALSE)
          outlierIDX <- model$data$pcdf[idx, -1:-ncol(model$data$scores)]
          new_list <- setNames(list(outlierIDX), placeHolder)
          outlierT <- append(outlierT, new_list)

          ##label outliers
          if(optns$ellipse == "T" & "outlierLabels" %in% names(optns) ){
            temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(label = outlierID), size=2, hjust = 0, vjust = 0)
          }

        }

        #stat_elipse with norm method
          if(optns$ellipse == "normal"){
            ##for plot
            temp <- temp + stat_ellipse( type = "norm", geom = "polygon", fill = "gray", level = ci, alpha = .5, linetype = 2)

            ##for the outliers
            build <- ggplot_build(temp)$data
            points <- build[[3]]
            ell <- build[[4]]

            # ##ensure correct element from build is selected
            # if(hotelStat == TRUE)
            # {
            #   ell <- build[[5]]
            # }

            ## Find which points are inside the ellipse
            dat <- list(in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y)))
            idx <-which(dat$in.ell == FALSE)
            outlierIDX <- model$data$pcdf[idx, -1:-ncol(model$data$scores)]
            new_list <- setNames(list(outlierIDX), placeHolder)
            outlierNormal <- append(outlierNormal, new_list)

            ##label outliers
            if(optns$ellipse == "normal" & "outlierLabels" %in% names(optns)){
              temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(label = outlierID), size = 2, hjust = 0, vjust = 0)
            }
            }
          #stat_ellipse for separation

        if(optns$ellipse == "colour"){
          temp <- temp +
            stat_ellipse(aes(color = model$data$pcdf$colourCoding))
        }
        pcaGridPlot[j, i] <- temp
      }
    }
  }
}

  # model$data <- append(model$data, list(outliers = list(hotelling = outlierHotellings,
  #                                                         statT = outlierT,
  #                                                         statNorm = outlierNormal)))

  model$plots <- append(model$plot, list(pcaGridPlot = pcaGridPlot))

  return(model)
}

###############################################################################################################################
# X <- as.matrix(model$data$pcdf[,1:thresh])
#
# # Sample size
# n <- nrow(X)
#
# hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)
#
# outlierHotellings <- list()
# outlierT <- list()
# outlierNormal <- list()
#
# #create ellipses and outlier tables
# for(i in 1:thresh)
# {
#   for(j in 1:thresh)
#   {
#     if(j>i)
#     {
#       #set up names for outlier list, eg PC1vPC2
#       placeHolder <- paste0("PC", i, "vPC", j)
#
#       #set up changing the individual plots in the grid
#       temp <- pcaGridPlot[j, i]
#
#       #Hotelling's T2
#
#       if(optns$ellipse == "hotellings"){
#
#         ##for the plot
#         temp <- temp + gg_circle(rx = sqrt(var(model$data$pcdf[i]) * hotFisN),
#                                  ry = sqrt(var(model$data$pcdf[j]) * hotFisN),
#                                  xc = 0,
#                                  yc = 0)
#         ##for outliers
#         ###condition to check if a point is an outlier
#         rx <- sqrt(var(model$data$pcdf[i]) * hotFisN)
#         ry <- sqrt(var(model$data$pcdf[j]) * hotFisN)
#
#         insideOut <- list((model$data$pcdf[i]^2)/(rx^2) + (model$data$pcdf[j]^2)/(ry^2))
#         idx <- which(insideOut[[1]] > 1)
#         ###the list of outliers
#         outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]
#         ###append the list of outliers
#         new_list <- setNames(list(outlierIDX),
#                              placeHolder)
#         outlierHotellings <- append(outlierHotellings,
#                                     new_list)
#
#         ##label outliers
#         # if("outlierLabel" %in% names(optns)){
#         #   temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(x = model$data$pcdf[,PC1], y = model$data$pcdf[,PC2], label = model$pcdf$outlierID), size=2, hjust = 0, vjust = 0)
#         # }
#       }
#
#       #stat_ellipse with type t method
#
#       if(optns$ellipse == "T"){
#
#         ##for the plot
#         temp <- temp +
#           stat_ellipse(type = "t",
#                        geom = "polygon",
#                        fill = "gray",
#                        level = ci,
#                        alpha = .5,
#                        linetype = 2)
#
#         ##for the outliers
#         build <- ggplot_build(temp)$data
#         points <- build[[3]]
#         ell <- build[[4]]
#
#         ## Find which points are inside the ellipse
#         dat <- list(in.ell = as.logical(point.in.polygon(points$x,
#                                                          points$y,
#                                                          ell$x,
#                                                          ell$y)))
#         idx <-which(dat$in.ell == FALSE)
#         outlierIDX <- model$data$pcdf[idx, -1:-ncol(model$data$scores)]
#         new_list <- setNames(list(outlierIDX),
#                              placeHolder)
#         outlierT <- append(outlierT,
#                            new_list)
#
#         ##label outliers
#         # if("outlierLabel" %in% names(optns)){
#         #   temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(x = PCi, y = PCj, label = model$data$pcdf$outlierID), size=2, hjust = 0, vjust = 0)
#         # }
#
#       }
#
#       #stat_elipse with norm method
#       if(optns$ellipse == "normal"){
#         ##for plot
#         temp <- temp +
#           stat_ellipse(type = "norm",
#                        geom = "polygon",
#                        fill = "gray",
#                        level = ci,
#                        alpha = .5,
#                        linetype = 2)
#
#         ##for the outliers
#         build <- ggplot_build(temp)$data
#         points <- build[[3]]
#         ell <- build[[4]]
#
#         ## Find which points are inside the ellipse
#         dat <- list(in.ell = as.logical(point.in.polygon(points$x,
#                                                          points$y,
#                                                          ell$x,
#                                                          ell$y)))
#         idx <-which(dat$in.ell == FALSE)
#         outlierIDX <- model$data$pcdf[idx, -1:-ncol(model$data$scores)]
#         new_list <- setNames(list(outlierIDX),
#                              placeHolder)
#         outlierNormal <- append(outlierNormal,
#                                 new_list)
#
#         ##label outliers
#         # if("outlierLabel" %in% names(optns)){
#         #   temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(label = outlierID), size = 2, hjust = 0, vjust = 0)
#         # }
#       }
#       #stat_ellipse for separation
#
#       if(optns$ellipse == "colour"){
#         temp <- temp +
#           stat_ellipse(aes(color = model$data$pcdf$colourCoding))
#       }
#
#       pcaGridPlot[j, i] <- temp
#     }
#   }
# }

