#' Biplot.
#'
#' A biplot plot for an O-PLS(DA) or PCA model. O-PLS is only available for the
#' first predictive and first orthogonal component. Displayed PLS components may
#' be changed. Loadings are displayed as vector arrows. A maximum of 50 features
#' can be displayed in a biplot. If stored, the plot will append to the model.
#'
#' @param model A PCA or oplsda model.
#' @param PCi A numeric for the component on the x-axis. Default is 1 and cannot
#' be changed for an OPLS (including DA). This means for an OPLS it will always
#' be the first predictive component.
#' @param PCj A numeric for the component on the y-axis. Default is 2 and cannot
#' be changed for an OPLS (including DA). This means for OPLS it will always be
#' the first orthogonal component.
#' @param zoom A double from 0 to 1 for the maximum of the loadings axis. The
#' default is 1.
#' @param optns An empty list for additional options:
#'  \itemize{
#'    \item{plotTitle} {A character for the title of the plot.}
#'    \item{color} {Either a column from the data frame or a character of the
#'    color desired (example "blue"). Default color is "black". When using on a
#'    ropls object, it must match the quantitative data type (discrete or continuous).}
#'    \item{shape} {Either a column from the data frame (must be discrete) or a
#'    character of the shape desired. Default shape is "circle".}
#'    \item{size} {Either a column from the data frame or a numeric of the size
#'    desired. Default size is 3.}
#'    \item{alpha} {Either a column from the data frame or a numeric of the alpha
#'    desired. Default size is 0.5.}
#'    \item{discretePalette} {Color palette for discrete values, you can assign
#'    colors to specific factors, example:
#'    discretePalette = c("control" = "purple", "treatment" = "orange").
#'    Or supply a concatenated list, example (and the default):
#'    discretePalette = c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC").
#'    Hexadecimal or color names accepted.}
#'    \item{continuousPalette} {Color palette for continuous values, use
#'    hexadecimal values (example and default:
#'    continuousPalette =c("#0000CC","#0000FF","#0055FF","#00AAFF","#00FFFF",
#'    "#2BFFD5","#55FFAA","#80FF80","#AAFF55","#D4FF2B","#FFFF00","#FFAA00",
#'    "#FF5500","#FF0000","#CC0000")), grDevices names (example:
#'    continousPalette = rainbow(4)) or color names (example :
#'    continuousPalette =c("purple", "orange")).}
#'    \item{theme} {Personalize the plot theme you would like applied as you
#'    would using theme() in ggplot. Example set
#'    theme = theme(legend.position = "left", text = element_text(size=5)).}
#'    \item{extra} {Add extra ggplot arguments that are not for theme(), example
#'    extra = scale_shape_manual(labels = c("A", "B", "C"),values = c(8, 17, 2))}
#'    \item{ellipse} {A character or either "color", "hotellings", "t", or "normal"
#'    depending on desired method of calculation. If using color, a discrete variable
#'    must be supplied to color.}
#'    \item{ci} {Set your own limit for ellipses drawn. The default is ci = 0.95
#'    (95 percent confidence interval).}
#'    \item {colorTitle} {A character of the desired color legend title when \code{color}
#'    is a variable. No color legend will appear if \code{color} is set to a simple
#'    aesthetic such as "green". Default "color".}
#'    \item {shapeTitle} {A character of the desired shape legend title when \code{shape}
#'    is a variable. No shape legend will appear if \code{shape} is set to a simple
#'    aesthetic such as "square". Default "Shape".}
#'    \item {sizeTitle} {A character of the desired shape legend title when \code{size}
#'    is a variable. No size legend will appear if \code{size} is set to a simple
#'    aesthetic such as 2. Default "Size".}
#'    \item {alphaTitle} {A character of the desired alpha legend title when \code{alpha}
#'    is a variable. No size legend will appear if \code{alpha} is set to a simple
#'    aesthetic such as 0.3. Default "Alpha".}
#' }
#'
#' @return The model with the appended biplot. Under plots for a PCA and in suppLs for
#' an O-PLS(DA)
#'
#' @examples
#' data(iris)
#' a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
#' b <- biplot(model = a,
#'                 PCi = 1,
#'                 PCj = 3,
#'                 zoom = 0.9,
#'                 optns = list(color = iris[,"Species"],
#'                              discretePalette = c("setosa" = "purple",
#'                                                  "versicolor" = "orange",
#'                                                  "virginica" = "steelblue"),
#'                               colorTitle = "Flower Species",
#'                               plotTitle = "Iris biplot",
#'                               alpha = 0.7))
#'
#' @import methods
#' @import ggplot2
#' @import ggrepel
#' @export



biplot <- function(model, zoom = 1, PCi = 1, PCj = 2, optns = list()){

  if(zoom > 1){
    stop("Zoom must be between 0 and 1, set as the desired maximum of the loadings axis")
  }
  if(zoom != 1){
    zoom <- (1/zoom)
  }

  if("plotTitle" %in% names(optns)){
    plotTitle <- optns$plotTitle
  }else{optns[["plotTitle"]] <- "Biplot"
  plotTitle <- "Biplot"}

  if("theme" %in% names(optns)){
    theme <- optns$theme
  }else{theme = theme(legend.position = "bottom")}

  #PCA
  if(is(model)[1] == "list"){
    scores <- model[["data"]][["scores"]]
    loadings <- model[["data"]][["loadings"]]

    optns[["PCi"]] <- PCi
    optns[["PCj"]] <- PCj
  }

  #OPLS
  if(is(model)[1] == "opls"){
    scores <- model@scoreMN
    loadings <- model@loadingMN

   if(grepl("O", model@typeC) == TRUE){
     scores <- as.data.frame(cbind(model@scoreMN, model@orthoScoreMN), check.names = F)
     loadings <- as.data.frame(cbind(model@loadingMN, model@orthoLoadingMN), check.names = F)
     PCi = 1
     PCj = 2
   }
    optns[["PCi"]] <- PCi
    optns[["PCj"]] <- PCj
  }

  #if the number of loadings is too high, stop
  if(nrow(loadings) > 50){
    stop("Max of 50 variables allowed for a biplot")
  }

  ps <-
  plotScores(model = model,
             optns = optns)

  if(is(model)[1] == "opls"){
    ps <- ps@suppLs[["ScoresPlot"]]
  }

  # Plots
  scalef <- max(abs(scores)) + 0.1



  # scalef <- scale
  # Get arrow end point locations (loadings*scaling for correct scale)
  l.x <- loadings[,PCi]*scalef*zoom
  l.y <- loadings[,PCj]*scalef*zoom

  # Get label positions (%15 further than end of arrows)
  l.posx <- l.x*1.15
  l.posy <- l.y*1.15

  # Get labels for plot (variable names)
  l.labels <- row.names(loadings)
  lim <- 1.1

  secondary_limits <- c(-lim, lim) * scalef



  #biplot
  bp <- ps +
    geom_segment(data = loadings,
                 aes(x = 0,
                     y = 0,
                     xend = l.x,
                     yend = l.y),
                 color = "black",
                 arrow = arrow(length = unit(0.2, "cm"),
                               type = "closed")) +
    geom_text_repel(data = loadings,
                    aes(x = l.posx,
                        y = l.posy,
                        label = l.labels),
                    color = "black",
                    size = 3,
                    hjust = 0) +
    scale_x_continuous(sec.axis = sec_axis(transform = ~ . / (scalef*zoom),
                                           breaks = round(c(-1, -0.5, 0, 0.5, 1)/zoom, digits = 2),
                                           name = paste0(colnames(loadings)[PCi]," Loadings")),
                       limits = secondary_limits,
                       expand = c(0, 0)) +
    scale_y_continuous(sec.axis = sec_axis(transform = ~ . / (scalef*zoom),
                                           breaks = round(c(-1, -0.5, 0, 0.5, 1)/zoom, digits = 2),
                                           name = paste0(colnames(loadings)[PCj]," Loadings")),
                       limits = secondary_limits,
                       expand = c(0, 0)) +
    theme +
    labs(title = plotTitle)

  #append to the model
  if(is(model)[1] == "list"){
    model$plots <- append(model$plots, list(biplot = bp))
  }
  if(is(model)[1] == "opls"){
    model@suppLs[["biplot"]] <- bp
  }


  print(bp)
  invisible(model)
}
