#' Plot Scores.
#'
#' Grid of the score plots using GGally::ggpairs up to a threshold number.
#'
#' @param model A PCA or oplsda model.
#' @param optns An empty list for aesthetic options.
#' @param plotTitle A parameter for the \code{optns} list. A character for the title of the plot.
#' @param thresh A parameter for the \code{optns} list. A numeric for the number of PCAs to display in the grid. The default is calculated in the PCA function.
#' @param color A parameter for the \code{optns} list. Either a column from the data frame or a character of the color desired (example "blue"). Default color is "black". When using on a ropls object, it must match the quantitative data type (discrete or continuous).
#' @param shape A parameter for the \code{optns} list. Either a column from the data frame (must be discrete) or a character of the shape desired. Default shape is "circle".
#' @param size A parameter for the \code{optns} list. Either a column from the data frame or a numeric of the size desired. Default size is 3.
#' @param alpha A parameter for the \code{optns} list. Either a column from the data frame or a numeric of the alpha desired. Default size is 0.5.
#' @param discretePalette A parameter for the \code{optns} list. Color palette for discrete values, you can assign colors to specific factors, example: discretePalette = c("control" = "purple", "treatment" = "orange"). Or supply a concatenated list, example (and the default): discretePalette = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"). Hexadecimal or color names accepted.
#' @param continuousPalette A parameter for the \code{optns} list. Color palette for continuous values, use hexadecimal values (example and default: continuousPalette =c("#0000CC","#0000FF","#0055FF","#00AAFF","#00FFFF","#2BFFD5","#55FFAA","#80FF80","#AAFF55","#D4FF2B","#FFFF00","#FFAA00","#FF5500","#FF0000","#CC0000")), grDevices names (example: continousPalette = rainbow(4)) or color names (example : continuousPalette =c("purple", "orange")).
#' @param theme A parameter for the \code{optns} list. Personalize the plot theme you would like applied as you would using theme() in ggplot. Example set theme = theme(legend.position = "left", text=element_text(size=5)) in optns.
#' @param colorTitle A parameter for the \code{optns} list. A character of the desired color legend title when \code{color} is a variable. No color legend will appear if \code{color} is set to a simple aesthetic such as "green". Default "color".
#' @param shapeTitle A parameter for the \code{optns} list. A character of the desired shape legend title when \code{shape} is a variable. No shape legend will appear if \code{shape} is set to a simple aesthetic such as "square". Default "Shape".
#' @param sizeTitle A parameter for the \code{optns} list. A character of the desired shape legend title when \code{size} is a variable. No size legend will appear if \code{size} is set to a simple aesthetic such as 2. Default "Size".
#' @param alphaTitle A parameter for the \code{optns} list. A character of the desired alpha legend title when \code{alpha} is a variable. No size legend will appear if \code{alpha} is set to a simple aesthetic such as 0.3. Default "Alpha".
#' @param ellipse A parameter for the \code{optns} list. A character or either "color", "hotellings", "t", or "normal" depending on desired method of calculation. If using color, a discrete variable must be supplied to color.
#' @param ci A parameter for the \code{optns} list. Set your own limit for ellipses drawn. The default is ci = 0.95 (95% confidence interval).
#' @param outlierLabels A parameter for the \code{optns} list. Only compatible with ellipse set to hotellings, T or normal. For ropls object, supply "outlierLabels" and rownames will appear. For PCA modek, supply a column from the data frame to label outliers. You can set it to, for example, outlierLabels=row.names(iris) to identify the outlier position in your dataframe.
#' @param annotation A parameter for the \code{optns} list when supplying PCA object. Supply the columns you'd like listed for the outliers, otherwise only that listed for outlierLabels will be listed.
#' @return The model list appended with the grid of loadings under plots.
#' @examples
#' data(iris)
#' a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
#' b <- plotscores(model = a, optns=list(color = iris[,"Species"], discretePalette = c("setosa" = "purple", "versicolor = "orange", "virginica" = "steelblue"), colorTitle = "Flower Species", gridTitle = "Iris PCA grid", thresh = 3, alpha = 0.7))
#' #To access a single plot from the grid: b[["plots]][["pcaGrid"]][j,i], where j is the vertical and i is the horizontal position of the specific plot in the grid.
#' @import methods
#' @import ggplot2
#' @import GGally
#' @export



plotScores<-function(model, optns=list()){

  #outlier annotation
  if("annotation" %in% names(optns)){
    if(is(model)[1] == "list"){
      model$data$pcdf <- cbind(model$data$pcdf, optns$annotation)
    }
    if(is(model)[1] == "opls"){
      model@suppLs[["annotation"]] <- optns$annotation
    }
  }

  if("outlierLabels" %in% names(optns)){
    if(!("annotation" %in% names(optns))){
      if(is(model)[1] == "list"){
        model$data$pcdf <- cbind(model$data$pcdf, optns$outlierLabels)
      }
      if(is(model)[1] == "opls"){
        model@suppLs[["annotation"]] <- optns$outlierLabels
      }

    }
  }

  #theme
  if(!("theme" %in% names(optns))){
   theme <- theme()
  } else{theme <- optns$theme}

  #plot title
  if("plotTitle" %in% names(optns)){
    plotTitle = optns$plotTitle
  }else{
    plotTitle <- "Scores Plot"
  }

  #color
  if(!("color" %in% names(optns))) {
    if (is(model)[1] == "list")
      optns$color = "black"
    if (is(model)[1] == "opls") {
      optns$color <- model@suppLs[["yMCN"]]
    }
  }

  #shape
  if(!("shape" %in% names(optns))){
    optns$shape="circle"}

  #size
  if(!("size" %in% names(optns))){
    optns$size=3}

  #alpha
  if(!("alpha" %in% names(optns))){
    optns$alpha=0.5}

  #legend titles
  if(!("colorTitle" %in% names(optns))){
   optns$colorTitle <- "Color"}

  if(!("shapeTitle" %in% names(optns))){
    optns$shapeTitle <- "Shape"}

  if(!("sizeTitle" %in% names(optns))){
    optns$sizeTitle <- "Size"}

  if(!("alphaTitle" %in% names(optns))){
    optns$alphaTitle <- "Alpha"}

  #palettes
  if(!("continuousPalette" %in% names(optns))){
    optns$continuousPalette <- c(
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
                                "#CC0000")
  }

  if(!("discretePalette" %in% names(optns))){
    optns$discretePalette <- c("#66C2A5",
                               "#FC8D62",
                               "#8DA0CB",
                               "#E78AC3",
                               "#A6D854",
                               "#FFD92F",
                               "#E5C494",
                               "#B3B3B3")
  }

  #ggplot2 treats integers and doubles as continuous variables, and treats only factors, characters, and logicals as discrete.

  # library(colorRamps)
  # > blue2green2red(15)
  # [1] "#0000CC" "#0000FF" "#0055FF" "#00AAFF" "#00FFFF"
  # [6] "#2BFFD5" "#55FFAA" "#80FF80" "#AAFF55" "#D4FF2B"
  # [11] "#FFFF00" "#FFAA00" "#FF5500" "#FF0000" "#CC0000"


  #correct input for ggplot and ggpairs objects
#geom_point
  gp<- if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1 ){
    geom_point(color = optns$color,
               shape = optns$shape,
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    geom_point(aes(color = optns$color,
                   shape = optns$shape,
                   size = optns$size,
                   alpha = optns$alpha))
  } else if((length(optns$color)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = optns$color),
               shape = optns$shape,
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(shape = optns$shape),
               color = optns$color,
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    geom_point(aes(size = optns$size),
               color = optns$color,
               shape = optns$shape,
               alpha = optns$alpha)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(alpha = optns$alpha),
               color = optns$color,
               shape = optns$shape,
               size = optns$size)
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = optns$color,
                   shape = optns$shape),
               size = optns$size,
               alpha = optns$alpha)
  } else if((length(optns$color)) > 1 & (length(optns$size)) > 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = optns$color,
                   size = optns$size),
               shape = optns$shape,
               alpha = optns$alpha)
  } else if((length(optns$color)) > 1 & (length(optns$size)) == 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(color = optns$color,
                   alpha = optns$alpha),
               shape = optns$shape,
               size = optns$size)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    geom_point(aes(shape = optns$shape,
                   size = optns$size),
               color = optns$color,
               alpha = optns$alpha)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(shape = optns$shape,
                   alpha = optns$alpha),
               color = optns$color,
               size = optns$size)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    geom_point(aes(size = optns$size,
                   alpha = optns$alpha),
               color = optns$color,
               shape = optns$shape)
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    geom_point(aes(color = optns$color,
                   shape = optns$shape,
                   size = optns$size),
               alpha = optns$alpha)
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    geom_point(aes(color = optns$color,
                   shape = optns$shape,
                   alpha = optns$alpha),
               size = optns$size)
  } else if((length(optns$color)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) >1){
    geom_point(aes(color = optns$color,
                   size = optns$size,
                   alpha = optns$alpha),
               shape = optns$shape)
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    geom_point(aes(shape = optns$shape,
                   size = optns$size,
                   alpha = optns$alpha),
               color = optns$color)
  }

#geom_guide
  gu<- if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1 ){
    guides(color = "none",
           shape = "none",
           size = "none",
           alpha = "none")
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    guides()
  } else if((length(optns$color)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    guides(shape = "none",
           size = "none",
           alpha = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    guides(color = "none",
           size = "none",
           alpha = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    guides(color = "none",
           shape = "none",
           alpha = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    guides(color = "none",
           shape = "none",
           size = "none")
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) == 1){
    guides(size = "none",
           alpha = "none")
  } else if((length(optns$color)) > 1 & (length(optns$size)) > 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) == 1){
    guides(shape = "none",
           alpha = "none")
  } else if((length(optns$color)) > 1 & (length(optns$size)) == 1 & (length(optns$shape)) == 1 & (length(optns$alpha)) > 1){
    guides(shape = "none",
           size = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    guides(color = "none",
           alpha = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    guides(color = "none",
           size = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    guides(color = "none",
           shape = "none")
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) == 1){
    guides(alpha = "none")
  } else if((length(optns$color)) > 1 & (length(optns$shape)) > 1 & (length(optns$size)) == 1 & (length(optns$alpha)) > 1){
    guides(size = "none")
  } else if((length(optns$color)) > 1 & (length(optns$shape)) == 1 & (length(optns$size)) > 1 & (length(optns$alpha)) >1){
    guides(shape = "none")
  } else if((length(optns$color)) == 1 & (length(optns$shape)) > 1 & (length(optns$size)) > 1 & (length(optns$alpha)) > 1){
    guides(color = "none")
  }


  ##########ropls objects############

  #ropls score plots
  if(is(model)[1]=="opls"){

    if(grepl("O", model@typeC) == TRUE){
      df <- as.data.frame(cbind(model@scoreMN, model@orthoScoreMN))
      df2 <- as.data.frame(cbind(model@loadingMN, model@orthoLoadingMN))
      gl <- labs(x = paste0('tp1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
                 y = paste0('to1'))
    }else{
      df <- as.data.frame(model@scoreMN)
      df2 <- as.data.frame(model@loadingMN)
      gl <- labs(x = paste0('t1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
                 y = paste0('t2 (', round(model@modelDF[["R2X"]][2]*100, 1), '%)'))
    }

    df <- cbind(df, model@suppLs[["yMCN"]])
    df$rownames <- rownames(df)

if("outlierLabels" %in% names(optns)){
  df$outlierID <- optns$outlierLabels
}

    #colors
    if(grepl("DA", model@typeC) == TRUE){
      gc <- scale_colour_manual(values = optns$discretePalette)
    }else{gc <-scale_color_gradientn(colors = optns$continuousPalette,
                                     na.value = "grey50",
                                     guide = "colorbar")
    }
  }

#########PCA objects##################
  if(is(model)[1]=="list"){

    df <- model$data$pcdf

    gc <- if(is(optns$color)[1] == "numeric" | is(optns$color)[1] == "integer" | is(optns$color)[1] == "double"){
      scale_color_gradientn(
        colors = optns$continuousPalette,
        na.value = "grey50",
        guide = "colorbar"
      )}
    else{scale_colour_manual(values = optns$discretePalette)}

    #number of pcas (working)
    if("thresh" %in% names(optns)){
      thresh = optns$thresh[1]
    }else{thresh = model$data$threshold}

    #axis labels
    title <- list()
    for (i in 1:thresh) {
      title[[i]] <- paste0('PC', i, ' (', round(model$data$pcSum$`Proportion of Variance`[i], 1), '%)')
    }
    title<-unlist(title)

    if("PCi" %in% names(optns)){
      PCi <- optns$PCi
      PCj <- optns$PCj
      gl <- labs(x = title[PCi], y = title[PCj])
    }
  }
##########ropls or LEGEND#############

    if(!("PCi" %in% names(optns))){
      PCi <- 1
      PCj <- 2
      if(is(model)[1]=="list"){
        gl <- labs()
      }
      }

########ALL (base plot)##############
  onePlot <- ggplot(data = df,
                    aes(x = df[,PCi], y = df[,PCj])) +
          ggtitle(plotTitle) +
          gl +
          gc +
          gp +
          gu +
          scale_alpha(range = c(0.1, 1)) +
          labs(color = optns$colorTitle,
               shape = optns$shapeTitle,
               size = optns$sizeTitle,
               alpha = optns$alphaTitle ) +
          geom_hline(yintercept = 0, colour = "gray70") +
          geom_vline(xintercept = 0, colour = "gray70") +
          theme_bw() +
          theme

#########ellipse & outliers########
if(is(model)[1] == "opls" && "ellipse" %in% names(optns)){

  #make ellipse
  output <- singleEllipseOptions(model = model, df = df, PCi = PCi, PCj = PCj, plot = onePlot, optns = optns)

  #idx <- 1
  idx <- output$outliers
  onePlot <- output$plot


  #add labels
  if("outlierLabels" %in% names(optns)){

    if(grepl("O", model@typeC) == TRUE){
      onePlot <- onePlot +
        geom_label(data = df[idx,],
                   aes(x = p1,
                       y = o1,
                       label = outlierID),
                   size = 2,
                   hjust = 0,
                   vjust = 0,
                   label.size = NA,
                   fill = NA)
    }

    if(!(grepl("O", model@typeC) == TRUE)){
      onePlot <- onePlot +
        geom_label(data = df[idx,],
                   aes(x = p1,
                       y = p2,
                       label = outlierID),
                   size = 2,
                   hjust = 0,
                   vjust = 0,
                   label.size = NA,
                   fill = NA)

    }

  }

  print(onePlot)

  model@suppLs[["outlierID"]] <- df[idx, "outlierID"]
  model@suppLs[["ScoresPlot"]] <- onePlot
  return(model)

}


  if (is(model)[1] == "opls" & !("ellipse" %in% names(optns)))
  {

    print(onePlot)

    model@suppLs[["ScoresPlot"]] <- onePlot
    return(model)

  }

#########single PCA##############
if(is(model)[1] == "list"){
  if("PCi" %in% names(optns)){
    if("ellipse" %in% names(optns)){
      output <- singleEllipseOptions(model = model, df = model$data$pcdf, PCi = PCi, PCj = PCj, plot = onePlot, optns = optns)
      idx <- output$outliers
      onePlot <- output$plot

      if("outlierLabels" %in% names(optns)) {
        model$data$pcdf$outlierID <- optns$outlierLabels
        onePlot <- onePlot +
                   geom_label(data = model$data$pcdf[idx, ],
                              aes(x = model$data$pcdf[idx, PCi],
                                  y = model$data$pcdf[idx, PCj],
                                  label = outlierID),
                              size = 2,
                              hjust = 0,
                              vjust = 0,
                              label.size = NA,
                              fill = NA)
    }
    }
    model$plots <- append(model$plots, list(pcaSingle = onePlot))
    invisible(model)
    return(onePlot)
  }

}

###########PCA grid###########
if(is(model)[1] == "list" && !("PCi" %in% names(optns))){
  #ensure that lack of legend doesn't stop ggpairs from working
  if ((length(optns$color)) == 1 &
      (length(optns$shape)) == 1 &
      (length(optns$size)) == 1 &
      (length(optns$alpha)) == 1) {
    testLegend <- NULL
  } else{
    testLegend <- grab_legend(onePlot)
  }

  #grid of PCAs
  pcaGridPlot<-ggpairs(data = model$data$pcdf[,1:thresh],
                               columnLabels = c(title),
                               title = plotTitle,
                               diag="blank",
                               upper="blank",
                               #upper=list(continuous = my_fn1),
                               lower=list(continuous = myFn1),
                               legend = testLegend,
                               progress = F,
                               switch = "both") +
    gc +
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
                                      color = "grey35")) +
    theme


  plotGT <- gridEllipseOptions (model = model,
                            pcaGridPlot = pcaGridPlot,
                            thresh = thresh,
                            optns = optns
  )

  pcaGridPlot <- gPairsLower(plotGT$pcaGridPlot)

  model$plots <- append(model$plots, list(pcaGrid = pcaGridPlot))

  model$data <- append(model$data, list(outliers = plotGT$outliers))

  print(pcaGridPlot)
  invisible(model)}



}
