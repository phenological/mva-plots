#' Plot Scores.
#'
#' Grid of the score plots using GGally::ggpairs up to a threshold number.
#'
#' @param model A pcResults object.
#' @param optns An empty list for aesthetic options.
#' @param gridTitle A parameter for the \code{optns} list. A character for the title of the grid.
#' @param thresh A parameter for the \code{optns} list. A numeric for the number of PCAs to display in the grid. The default is calculated in the pcResults function.
#' @param colourCoding A parameter for the \code{optns} list. Either a column from the data frame (must be discrete) or a character of the colour desired. Default colour is "black"
#' @param shapeCoding A parameter for the \code{optns} list. Either a column from the data frame (must be discrete) or a character of the shape desired. Default shape is "circle".
#' @param sizeCoding A parameter for the \code{optns} list. Either a column from the data frame or a numeric of the size desired. Default size is 3.
#' @param alphaCoding A parameter for the \code{optns} list. Either a column from the data frame or a numeric of the alpha desired. Default size is 0.5.
#' @param colourTitle A parameter for the \code{optns} list. A character of the desired colour legend title when \code{colourCoding} is a variable. No colour legend will appear if \code{colourCoding} is set to a simple aesthetic such as "green". Default "Colour".
#' @param shapeTitle A parameter for the \code{optns} list. A character of the desired shape legend title when \code{shapeCoding} is a variable. No shape legend will appear if \code{shapeCoding} is set to a simple aesthetic such as "square". Default "Shape".
#' @param sizeTitle A parameter for the \code{optns} list. A character of the desired shape legend title when \code{sizeCoding} is a variable. No size legend will appear if \code{sizeCoding} is set to a simple aesthetic such as 2. Default "Size".
#' @param alphaTitle A parameter for the \code{optns} list. A character of the desired alpha legend title when \code{alphaCoding} is a variable. No size legend will appear if \code{alphaCoding} is set to a simple aesthetic such as 0.3. Default "Alpha".
#' @return The model list appended with the grid of loadings under plots.
#' @examples
#' data(iris)
#' a <- pcResults(data = iris[,1:4], annotation=[,5], center = TRUE, scale. = TRUE)
#' b <- plotscores(model = a, optns=list(colourCoding = iris[,"Species"], colourTitle = "Flower Species", gridTitle = "Iris PCA grid", thresh = 3, alphaCoding = 0.7))

plotscores<-function(model, optns=list()){

#Grid title (working)
  if("gridTitle" %in% names(optns)){
      gridTitle = optns$gridTitle
  }else{
    gridTitle <- "PCA Grid"
    #cat(yellow("Using default gridTitle", "!\n"))
  }

#number of pcas (working)
if("thresh" %in% names(optns)){
  thresh = optns$thresh[1]
}else{thresh = model$data$threshold}

#colour (working)
 if("colourCoding" %in% names(optns)){
   if((length(optns$colourCoding))>1){
     model$data$pcdf$colourCoding <- optns$colourCoding
   }
 }else {optns$colourCoding="black"}

#shape (working)
  if("shapeCoding" %in% names(optns)){
    if((length(optns$shapeCoding))>1){
      model$data$pcdf$shapeCoding <- optns$shapeCoding
    }
  }else{optns$shapeCoding="circle"}

#size (working)
 if("sizeCoding" %in% names(optns)){
   if((length(optns$sizeCoding))>1){
     model$data$pcdf$sizeCoding <- optns$sizeCoding
   }
 }else{optns$sizeCoding=3}

#alpha (working)
   if("alphaCoding" %in% names(optns)){
     if((length(optns$alphaCoding))>1){
       model$data$pcdf$alphaCoding <- optns$alphaCoding
     }
   }else{optns$alphaCoding=0.5}

#legend titles
  if("colourTitle" %in% names(optns)){
    colourTitle = optns$colourTitle
  } else{colourTitle="Colour"}

  if("shapeTitle" %in% names(optns)){
    shapeTitle = optns$shapeTitle
  } else{shapeTitle="Shape"}

  if("sizeTitle" %in% names(optns)){
    sizeTitle = optns$sizeTitle
  } else{sizeTitle="Size"}

  if("alphaTitle" %in% names(optns)){
    alphaTitle = optns$alphaTitle
  } else{alphaTitle="Alpha"}

#correct input for ggplot and ggpairs objects
gp<- if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1 ){
  geom_point(color = optns$colourCoding,
             shape = optns$shapeCoding,
             size = optns$sizeCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 shape = model$data$pcdf$shapeCoding,
                 size = model$data$pcdf$sizeCoding,
                 alpha = model$data$pcdf$alphaCoding))
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(color = model$data$pcdf$colourCoding),
             shape = optns$shapeCoding,
             size = optns$sizeCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(shape = model$data$pcdf$shapeCoding),
             color = optns$colourCoding,
             size = optns$sizeCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(size = model$data$pcdf$sizeCoding),
             color = optns$colourCoding,
             shape = optns$shapeCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(alpha = model$data$pcdf$alphaCoding),
             color = optns$colourCoding,
             shape = optns$shapeCoding,
             size = optns$sizeCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 shape = model$data$pcdf$shapeCoding),
             size = optns$sizeCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$shapeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 size = model$data$pcdf$sizeCoding),
             shape = optns$shapeCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 alpha = model$data$pcdf$alphaCoding),
             shape = optns$shapeCoding,
             size = optns$sizeCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(shape = model$data$pcdf$shapeCoding,
                 size = model$data$pcdf$sizeCoding),
             color = optns$colourCoding,
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(shape = model$data$pcdf$shapeCoding,
                 alpha = model$data$pcdf$alphaCoding),
             color = optns$colourCoding,
             size = optns$sizeCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(size = model$data$pcdf$sizeCoding,
                 alpha = model$data$pcdf$alphaCoding),
             color = optns$colourCoding,
             shape = optns$shapeCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) == 1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 shape = model$data$pcdf$shapeCoding,
                 size = model$data$pcdf$sizeCoding),
             alpha = optns$alphaCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 shape = model$data$pcdf$shapeCoding,
                 alpha = model$data$pcdf$alphaCoding),
             size = optns$sizeCoding)
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) >1){
  geom_point(aes(color = model$data$pcdf$colourCoding,
                 size = model$data$pcdf$sizeCoding,
                 alpha = model$data$pcdf$alphaCoding),
             shape = optns$shapeCoding)
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) > 1){
  geom_point(aes(shape = model$data$pcdf$shapeCoding,
                 size = model$data$pcdf$sizeCoding,
                 alpha = model$data$pcdf$alphaCoding),
             color = optns$colourCoding)
}


gu<- if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1 ){
  guides(color = "none",
         shape = "none",
         size = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) > 1){
  guides()
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  guides(shape = "none",
         size = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  guides(color = "none",
         size = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) == 1){
  guides(color = "none",
         shape = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  guides(color = "none",
         shape = "none",
         size = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  guides(size = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$shapeCoding)) == 1 & (length(optns$alphaCoding)) == 1){
  guides(shape = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  guides(shape = "none",
         size = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) == 1){
  guides(color = "none",
         alpha = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  guides(color = "none",
         size = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) > 1){
  guides(color = "none",
         shape = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) == 1){
  guides(alpha = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) == 1 & (length(optns$alphaCoding)) > 1){
  guides(size = "none")
} else if((length(optns$colourCoding)) > 1 & (length(optns$shapeCoding)) == 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) >1){
  guides(shape = "none")
} else if((length(optns$colourCoding)) == 1 & (length(optns$shapeCoding)) > 1 & (length(optns$sizeCoding)) > 1 & (length(optns$alphaCoding)) > 1){
  guides(color = "none")
}

#plot used for its legend
  test <- ggplot(data = model$data$pcdf,
                 aes(x = "PC1",
                     y = "PC2")) +
          gp +
          gu +
          scale_alpha(range = c(0.1, 1)) +
          scale_color_brewer(palette="Set2") +
          labs(color = colourTitle,
               shape = shapeTitle,
               size = sizeTitle,
               alpha = alphaTitle ) +
          theme_minimal()

#axis labels
title <- list()
for (i in 1:thresh) {
  title[[i]] <- paste0('PC', i, ' (', round(model$data$pcSum$`Proportion of Variance`[i], 1), '%)')
}
title<-unlist(title)

#grid of PCAs
pcaGridPlot<-GGally::ggpairs(data = model$data$pcdf[,1:thresh],
                             columnLabels = c(title),
                             title = gridTitle,
                             diag="blank",
                             upper="blank",
                             #upper=list(continuous = my_fn1),
                             lower=list(continuous = myFn1),
                             legend = GGally::grab_legend(test),
                             progress = F,
                             switch = "both") +
                      gp +
                      theme_bw() +
                      theme(strip.background = element_rect(fill = "white"),
                            axis.text.x = (element_text(size = rel(0.7),
                                                        angle = 0)),
                            axis.text.y = (element_text(size = rel(0.7),
                                                        angle = 0)),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_rect(fill = NA,
                                                        colour = "grey35"))

#confidence interval
if("confidenceInterval" %in% names(optns)){
  ci <- optns$confidenceInterval
}else{ci <-0.95}

#labels for outliers
# if("outlierLabels" %in% names(optns)){
#   model$data$pcdf$outlierID <- optns$outlierLabels
# }

#ellipse options
if("ellipse" %in% names(optns)){
  X <- as.matrix(model$data$pcdf[,1:thresh])

  # Sample size
  n <- nrow(X)

  hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)

  outlierHotellings <- list()
  outlierT <- list()
  outlierNormal <- list()

#create ellipses and outlier tables
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
                                   xc = 0,
                                   yc = 0)
          ##for outliers
          ###condition to check if a point is an outlier
          rx <- sqrt(var(model$data$pcdf[i]) * hotFisN)
          ry <- sqrt(var(model$data$pcdf[j]) * hotFisN)

          insideOut <- list((model$data$pcdf[i]^2)/(rx^2) + (model$data$pcdf[j]^2)/(ry^2))
          idx <- which(insideOut[[1]] > 1)
          ###the list of outliers
          outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]
          ###append the list of outliers
          new_list <- setNames(list(outlierIDX),
                               placeHolder)
          outlierHotellings <- append(outlierHotellings,
                                      new_list)

          ##label outliers
          # if("outlierLabel" %in% names(optns)){
          #   temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(x = model$data$pcdf[,PC1], y = model$data$pcdf[,PC2], label = model$pcdf$outlierID), size=2, hjust = 0, vjust = 0)
          # }
        }

        #stat_ellipse with type t method

        if(optns$ellipse == "T"){

          ##for the plot
          temp <- temp +
                  stat_ellipse(type = "t",
                               geom = "polygon",
                               fill = "gray",
                               level = ci,
                               alpha = .5,
                               linetype = 2)

          ##for the outliers
          build <- ggplot_build(temp)$data
          points <- build[[3]]
          ell <- build[[4]]

          ## Find which points are inside the ellipse
          dat <- list(in.ell = as.logical(point.in.polygon(points$x,
                                                           points$y,
                                                           ell$x,
                                                           ell$y)))
          idx <-which(dat$in.ell == FALSE)
          outlierIDX <- model$data$pcdf[idx, -1:-ncol(model$data$scores)]
          new_list <- setNames(list(outlierIDX),
                               placeHolder)
          outlierT <- append(outlierT,
                             new_list)

          ##label outliers
          # if("outlierLabel" %in% names(optns)){
          #   temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(x = PCi, y = PCj, label = model$data$pcdf$outlierID), size=2, hjust = 0, vjust = 0)
          # }

        }

        #stat_elipse with norm method
        if(optns$ellipse == "normal"){
          ##for plot
          temp <- temp +
                  stat_ellipse(type = "norm",
                               geom = "polygon",
                               fill = "gray",
                               level = ci,
                               alpha = .5,
                               linetype = 2)

          ##for the outliers
          build <- ggplot_build(temp)$data
          points <- build[[3]]
          ell <- build[[4]]

          ## Find which points are inside the ellipse
          dat <- list(in.ell = as.logical(point.in.polygon(points$x,
                                                           points$y,
                                                           ell$x,
                                                           ell$y)))
          idx <-which(dat$in.ell == FALSE)
          outlierIDX <- model$data$pcdf[idx, -1:-ncol(model$data$scores)]
          new_list <- setNames(list(outlierIDX),
                               placeHolder)
          outlierNormal <- append(outlierNormal,
                                  new_list)

          ##label outliers
          # if("outlierLabel" %in% names(optns)){
          #   temp <- temp + geom_text(data=model$data$pcdf[idx,], aes(label = outlierID), size = 2, hjust = 0, vjust = 0)
          # }
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

pcaGridPlot <- gPairsLower(pcaGridPlot)

model$data <- append(model$data, list(outliers = list(hotellings = outlierHotellings,
                                                      statT = outlierT,
                                                      statNorm = outlierNormal)))
model$plots <- append(model$plots, list(pcaGrid = pcaGridPlot))

return(model)

}
