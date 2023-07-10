#' Plot Scores.
#'
#' Grid of the score plots using GGally::ggpairs up to a threshold number.
#'
#' @param model A PCA object.
#' @param optns An empty list for aesthetic options.
#' @param gridTitle A parameter for the \code{optns} list. A character for the title of the grid.
#' @param thresh A parameter for the \code{optns} list. A numeric for the number of PCAs to display in the grid. The default is calculated in the PCA function.
#' @param colour A parameter for the \code{optns} list. Either a column from the data frame (must be discrete) or a character of the colour desired. Default colour is "black"
#' @param shape A parameter for the \code{optns} list. Either a column from the data frame (must be discrete) or a character of the shape desired. Default shape is "circle".
#' @param size A parameter for the \code{optns} list. Either a column from the data frame or a numeric of the size desired. Default size is 3.
#' @param alpha A parameter for the \code{optns} list. Either a column from the data frame or a numeric of the alpha desired. Default size is 0.5.
#' @param colourTitle A parameter for the \code{optns} list. A character of the desired colour legend title when \code{colour} is a variable. No colour legend will appear if \code{colour} is set to a simple aesthetic such as "green". Default "Colour".
#' @param shapeTitle A parameter for the \code{optns} list. A character of the desired shape legend title when \code{shape} is a variable. No shape legend will appear if \code{shape} is set to a simple aesthetic such as "square". Default "Shape".
#' @param sizeTitle A parameter for the \code{optns} list. A character of the desired shape legend title when \code{size} is a variable. No size legend will appear if \code{size} is set to a simple aesthetic such as 2. Default "Size".
#' @param alphaTitle A parameter for the \code{optns} list. A character of the desired alpha legend title when \code{alpha} is a variable. No size legend will appear if \code{alpha} is set to a simple aesthetic such as 0.3. Default "Alpha".
#' @param ellipse A parameter for the \code{optns} list. A character or either "colour", "hotellings", "T", or "normal" depending on desired method of calculation.
#' @param outlierLabels A parameter for the \code{optns} list. A column from the data frame to label outliers with (only compatible with ellipse set to hotellings, T or normal). You can set it to, for example, outlierLabels=row.names(iris) to more easily identify the outlier position in your dataframe.
#' @return The model list appended with the grid of loadings under plots.
#' @examples
#' data(iris)
#' a <- PCA(data = iris[,1:4], annotation=[,5], center = TRUE, scale. = TRUE)
#' b <- plotscores(model = a, optns=list(colour = iris[,"Species"], colourTitle = "Flower Species", gridTitle = "Iris PCA grid", thresh = 3, alpha = 0.7))
#' To access a single plot from the grid: b[["plots]][["pcaGrid"]][j,i], where j is the vertical and i is the horizontal position of the specific plot in the grid.

plotScores<-function(model, optns=list()){

  #Grid title (working)
  if("gridTitle" %in% names(optns)){
    gridTitle = optns$gridTitle
  }else{
    gridTitle <- "PCA Grid"
  }

  #number of pcas (working)
  if("thresh" %in% names(optns)){
    thresh = optns$thresh[1]
  }else{thresh = model$data$threshold}

  #colour (working)
  if("colour" %in% names(optns)){
    if((length(optns$colour))>1){
      model$data$pcdf$colour <- optns$colour
    }
  }else {optns$colour="black"}

  #shape (working)
  if("shape" %in% names(optns)){
    if((length(optns$shape))>1){
      model$data$pcdf$shape <- optns$shape
    }
  }else{optns$shape="circle"}

  #size (working)
  if("size" %in% names(optns)){
    if((length(optns$size))>1){
      model$data$pcdf$size <- optns$size
    }
  }else{optns$size=3}

  #alpha (working)
  if("alpha" %in% names(optns)){
    if((length(optns$alpha))>1){
      model$data$pcdf$alpha <- optns$alpha
    }
  }else{optns$alpha=0.5}

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

  #ggplot2 treats integers and doubles as continuous variables, and treats only factors, characters, and logicals as discrete.

  # library(colorRamps)
  # > blue2green2red(15)
  # [1] "#0000CC" "#0000FF" "#0055FF" "#00AAFF" "#00FFFF"
  # [6] "#2BFFD5" "#55FFAA" "#80FF80" "#AAFF55" "#D4FF2B"
  # [11] "#FFFF00" "#FFAA00" "#FF5500" "#FF0000" "#CC0000"
  gs <- if(is(optns$colour)[1] == "numeric"){
    scale_color_gradientn(
      colours = c(
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
      ),
      na.value = "grey50",
      guide = "colourbar"
    )}
  else{scale_color_brewer(palette = "Set2")}

  #correct input for ggplot and ggpairs objects
  gp<- if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1 ){
    geom_point(color = optns$colour,
               shape = optns$shape,
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    geom_point(aes(color = model$data$pcdf$colour,
                   shape = model$data$pcdf$shape,
                   size = model$data$pcdf$size,
                   alpha = model$data$pcdf$alpha))
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = model$data$pcdf$colour),
               shape = optns$shape,
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(shape = model$data$pcdf$shape),
               color = optns$colour,
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    geom_point(aes(size = model$data$pcdf$size),
               color = optns$colour,
               shape = optns$shape,
               alpha = optns$alpha)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(alpha = model$data$pcdf$alpha),
               color = optns$colour,
               shape = optns$shape,
               size = optns$size)
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = model$data$pcdf$colour,
                   shape = model$data$pcdf$shape),
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$colour)) > 1 & (length(optns$size)) > 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = model$data$pcdf$colour,
                   size = model$data$pcdf$size),
               shape = optns$shape,
               alpha = optns$alpha)
  } else if((length(optns$colour)) > 1 & (length(optns$size)) == 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(color = model$data$pcdf$colour,
                   alpha = model$data$pcdf$alpha),
               shape = optns$shape,
               size = optns$size)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    geom_point(aes(shape = model$data$pcdf$shape,
                   size = model$data$pcdf$size),
               color = optns$colour,
               alpha = optns$alpha)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(shape = model$data$pcdf$shape,
                   alpha = model$data$pcdf$alpha),
               color = optns$colour,
               size = optns$size)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    geom_point(aes(size = model$data$pcdf$size,
                   alpha = model$data$pcdf$alpha),
               color = optns$colour,
               shape = optns$shape)
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = model$data$pcdf$colour,
                   shape = model$data$pcdf$shape,
                   size = model$data$pcdf$size),
               alpha = optns$alpha)
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(color = model$data$pcdf$colour,
                   shape = model$data$pcdf$shape,
                   alpha = model$data$pcdf$alpha),
               size = optns$size)
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) >1){
    geom_point(aes(color = model$data$pcdf$colour,
                   size = model$data$pcdf$size,
                   alpha = model$data$pcdf$alpha),
               shape = optns$shape)
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    geom_point(aes(shape = model$data$pcdf$shape,
                   size = model$data$pcdf$size,
                   alpha = model$data$pcdf$alpha),
               color = optns$colour)
  }


  gu<- if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1 ){
    guides(color = "none",
           shape = "none",
           size = "none",
           alpha = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    guides()
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    guides(shape = "none",
           size = "none",
           alpha = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    guides(color = "none",
           size = "none",
           alpha = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    guides(color = "none",
           shape = "none",
           alpha = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    guides(color = "none",
           shape = "none",
           size = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    guides(size = "none",
           alpha = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$size)) > 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) == 1){
    guides(shape = "none",
           alpha = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$size)) == 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) > 1){
    guides(shape = "none",
           size = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    guides(color = "none",
           alpha = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    guides(color = "none",
           size = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    guides(color = "none",
           shape = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    guides(alpha = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    guides(size = "none")
  } else if((length(optns$colour)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) >1){
    guides(shape = "none")
  } else if((length(optns$colour)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    guides(color = "none")
  }

  #plot used for its legend
  test <- ggplot(data = model$data$pcdf,
                 aes(x = model$data$pcdf$PC1,
                     y = model$data$pcdf$PC2)) +
    gs +
    gp +
    gu +
    scale_alpha(range = c(0.1, 1)) +
    #scale_color_brewer(palette="Set2") +
    labs(color = colourTitle,
         shape = shapeTitle,
         size = sizeTitle,
         alpha = alphaTitle ) +
    theme_minimal()

#ensure that lack of legend doesn't stop ggpairs from working
  if ((length(optns$colour)) == 1 &
      (length(optns$shape)) == 1 &
      (length(optns$size)) == 1 &
      (length(optns$alpha)) == 1) {
    testLegend <- NULL
  } else{
    testLegend <- GGally::grab_legend(test)
  }

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
                               legend = testLegend,
                               progress = F,
                               switch = "both") +
    gs +
    gp +
    theme_bw() +
    theme(legend.position = "right",
      strip.background = element_rect(fill = "white"),
          axis.text.x = (element_text(size = rel(0.7),
                                      angle = 0)),
          axis.text.y = (element_text(size = rel(0.7),
                                      angle = 0)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA,
                                      colour = "grey35"))


  plotGT <- ellipseOptions (model = model,
                            pcaGridPlot = pcaGridPlot,
                            thresh = thresh,
                            optns = optns
                          )

  pcaGridPlot <- gPairsLower(plotGT$pcaGridPlot)

  model$plots <- append(model$plots, list(pcaGrid = pcaGridPlot))

  model$data <- append(model$data, list(outliers = plotGT$outliers))

  print(pcaGridPlot)
  invisible(model)


}
