gridEllipseOptions <- function(model = model, pcaGridPlot = pcaGridPlot, thresh = thresh, optns = optns){

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
          #####NB: c() or as.vector around rx and ry as otherwise warnings about depreciated vector arithmetic occur
          temp <-
            temp + gg_circle(
              rx = c(sqrt(var(model$data$pcdf[i]) * hotFisN)),
              ry = c(sqrt(var(model$data$pcdf[j]) * hotFisN)),
              xc = 0,
              yc = 0
            )
          ##for outliers
          rx <- c(sqrt(var(model$data$pcdf[i]) * hotFisN))
          ry <- c(sqrt(var(model$data$pcdf[j]) * hotFisN))

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

        if(optns$ellipse == "t"){

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
          if(optns$ellipse == "t" & "outlierLabels" %in% names(optns) ){
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

        if(optns$ellipse == "color"){
          temp <- temp +
            stat_ellipse(aes(color = optns$color))
            #stat_ellipse(aes(color = model$data$pcdf$color))
          outliers <- list()
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
#individual###############?
  return(output)
}

singleEllipseOptions <- function(model, df, PCi, PCj, plot, optns){
  if("ellipse" %in% names(optns)){
    if(optns$ellipse == "color"){
    #####colour#########
      # if("y1" %in% colnames(df)){
      #   ellipse <- stat_ellipse(aes(color = df$y1))
      # } else {ellipse <- stat_ellipse(aes(color = optns$color))}
      if(is(model)[1] == "opls"){
        ellipse <- stat_ellipse(aes(color = df$y1))
      }
      if(is(model)[1]=="list") {
        ellipse <- stat_ellipse(aes(color = optns$color))
        }
      gl <- labs()
      idx <-list()
    }

    #option for CI % level
    if("ci" %in% names(optns)) {
      ci <- optns$ci
    } else{ci <- 0.95}

    ##########Hotelling's##############
    if(optns$ellipse == "hotellings"){

      n <- nrow(df)

      hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)

      ellipse <- gg_circle(rx = sqrt(var(df[,PCi]) * hotFisN),
                           ry = sqrt(var(df[,PCj]) * hotFisN),
                           xc = 0,
                           yc = 0)
      gl <-  labs(caption = bquote(paste("Dashed line: Hotelling's T"^{2} ~"ellipse ("* alpha *" ~ ", .(ci), ")")))

      ##for outliers
      rx <- sqrt(var(df[, PCi]) * hotFisN)
      ry <- sqrt(var(df[, PCj]) * hotFisN)

      insideOut <- list((df[,PCi]^2)/(rx^2) + (df[,PCj]^2)/(ry^2))
      idx <- which(insideOut[[1]] > 1)
    }
    ##########T##############
    if(optns$ellipse == "t"){
      #norm or t
      ellipse <- stat_ellipse(type = "t",
                              geom = "polygon",
                              fill = "gray",
                              level = ci,
                              alpha = .5,
                              linetype = 2)
      gl <-  labs(caption = bquote(paste("Dashed line: t distribution" ~"ellipse ("* alpha *" ~ ", .(ci), ")")))
    }
    ###############normal#######
    if(optns$ellipse == "normal"){
      ellipse <- stat_ellipse(type = "norm",
                              geom = "polygon",
                              fill = "gray",
                              level = ci,
                              alpha = .5,
                              linetype = 2)
      gl <-  labs(caption = bquote(paste("Dashed line: normal distribution" ~"ellipse ("* alpha *" ~ ", .(ci), ")")))

    }
  }else{ellipse <- labs(caption = expression("no ellipse"))
  gl <- labs()}


  plot <- plot + ellipse + gl

  if(optns$ellipse == "t" | optns$ellipse =="normal"){

    ##for outliers
    build <- ggplot_build(plot)$data
    points <- build[[3]]
    ell <- build[[4]]

    ## Find which points are inside the ellipse
    rx <- (max(ell$x)-min(ell$x))/2
    ry <- (max(ell$y)-min(ell$y))/2
    h <- max(ell$x) - rx
    k <- max(ell$y) - ry

    insideOut <- list((((df[,PCi]-h)^2)/(rx^2)) + (((df[,PCj]-k)^2)/(ry^2)))
    idx <- which(insideOut[[1]] > 1)

  }
  output <- list(plot = plot,
                 outliers = idx)

  return(output)
}
