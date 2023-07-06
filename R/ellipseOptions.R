ellipseOptions <- function(model = model, pcaGridPlot = pcaGridPlot, thresh = thresh, optns = optns){

#option for CI % level
  if("ci" %in% names(optns)) {
    ci <- optns$ci
  } else{ci <- 0.95}

#option for outlier labels on plot
  if("outlierLabels" %in% names(optns)) {
    model$data$pcdf$outlierID <- optns$outlierLabels
  }

#ellipse options

  if("ellipse" %in% names(optns)){
  X <- as.matrix(model$data$pcdf[,1:thresh])

  # Sample size
  n <- nrow(X)

  hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)

  outliers <- list()


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
          temp <-
            temp + gg_circle(
              rx = sqrt(var(model$data$pcdf[i]) * hotFisN),
              ry = sqrt(var(model$data$pcdf[j]) * hotFisN),
              xc = 0,
              yc = 0
            )
          ##for outliers
          rx <- sqrt(var(model$data$pcdf[i]) * hotFisN)
          ry <- sqrt(var(model$data$pcdf[j]) * hotFisN)

          insideOut <- list((model$data$pcdf[i]^2)/(rx^2) + (model$data$pcdf[j]^2)/(ry^2))
          idx <- which(insideOut[[1]] > 1)

          outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]

          new_list <- setNames(list(outlierIDX), placeHolder)
          outliers <- append(outliers, new_list)

          ##label outliers
          if(optns$ellipse == "hotellings" & "outlierLabels" %in% names(optns)){
            temp <-
              temp + geom_text(
                data = model$data$pcdf[idx, ],
                aes(label = outlierID),
                size = 2,
                hjust = 0,
                vjust = 0
              )
          }
        }

        #stat_ellipse with type t method

        if(optns$ellipse == "T"){

          ##for the plot
          temp <-
            temp + stat_ellipse(
              type = "t",
              geom = "polygon",
              fill = "gray",
              level = ci,
              alpha = .5,
              linetype = 2
            )

          ##for the outliers
          build <- ggplot_build(temp)$data
          points <- build[[3]]
          ell <- build[[4]]

          ## Find which points are inside the ellipse
          rx <- (max(ell$x)-min(ell$x))/2
          ry <- (max(ell$y)-min(ell$y))/2
          h <- max(ell$x) - rx
          k <- max(ell$y) - ry

          insideOut <- list((((model$data$pcdf[i]-h)^2)/(rx^2)) + (((model$data$pcdf[j]-k)^2)/(ry^2)))
          idx <- which(insideOut[[1]] > 1)
          outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]

          new_list <- setNames(list(outlierIDX), placeHolder)
          outliers <- append(outliers, new_list)

          ##label outliers
          if(optns$ellipse == "T" & "outlierLabels" %in% names(optns) ){
            temp <-
              temp + geom_text(
                data = model$data$pcdf[idx, ],
                aes(label = outlierID),
                size = 2,
                hjust = 0,
                vjust = 0
              )
          }

        }

        #stat_elipse with norm method
          if(optns$ellipse == "normal"){
            ##for plot
            temp <-
              temp + stat_ellipse(
                type = "norm",
                geom = "polygon",
                fill = "gray",
                level = ci,
                alpha = .5,
                linetype = 2
              )

            ##for the outliers
            build <- ggplot_build(temp)$data
            points <- build[[3]]
            ell <- build[[4]]

            ## Find which points are inside the ellipse
            rx <- (max(ell$x)-min(ell$x))/2
            ry <- (max(ell$y)-min(ell$y))/2
            h <- max(ell$x) - rx
            k <- max(ell$y) - ry

            insideOut <- list((((model$data$pcdf[i]-h)^2)/(rx^2)) + (((model$data$pcdf[j]-k)^2)/(ry^2)))
            idx <- which(insideOut[[1]] > 1)
            outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]

            new_list <- setNames(list(outlierIDX), placeHolder)
            outliers <- append(outliers, new_list)

            ##label outliers
            if(optns$ellipse == "normal" & "outlierLabels" %in% names(optns)){
              temp <-
                temp + geom_text(
                  data = model$data$pcdf[idx, ],
                  aes(label = outlierID),
                  size = 2,
                  hjust = 0,
                  vjust = 0
                )
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

if("ellipse" %in% names(optns)){
  outliers <- outliers
}else{outliers <- list()}

  output <- list(pcaGridPlot = pcaGridPlot,
                 outliers = outliers
                )

  return(output)
}
