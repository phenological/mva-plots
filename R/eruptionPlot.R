#' Eruption Plot.
#'
#' Eruption plot.
#'
#' @param model A PCA or oplsda object.
#' @param optns An empty list for additional options.
#' @param factor A parameter for the \code{optns} list used when supplying a PCA object. An object the same length as the data used to build the PCA model that must be a two factor variable such as treatment and control.
#' @param plotTitle A parameter for the \code{optns} list. A character for the title of the plot. Default is "Eruption Plot".
#' @param method A parameter for the \code{optns} list. Determines the method to adjust p-values by. The options the same as listed in stats::p.adjust ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"). The default is "none".
#' @param PC A parameter for the \code{optns} list when supplying a PCA object. A numeric for which principal component to use for the loadings (for the plot y-axis) and scores (if correlation is chosen for \code{colourCoding}).
#' @param colourCoding A parameter for the \code{optns} list. Colour coding for the eruption plot, either the adjusted and rescaled p-value or the correlation. The default is the p-value. To switch set to "correlation".
#' @return The model list appended with the eruption plot and the data to make the plot.
#'
#' @examples
#' data(mtcars)
#'
#' a <- PCA(data = mtcars[,1:7], annotation = mtcars[,8:11], center = TRUE, scale. = TRUE)
#' b <- eruptionPlot(model = a, factor=mtcars[,"vs"], optns=list(colourCoding = "correlation", plotTitle = "mtcars eruption", method = "bonferroni"))
#' To access a single plot from the grid: b[["plots]][["pcaGrid"]][j,i], where j is the vertical and i is the horizontal position of the specific plot in the grid.

eruptionPlot <- function(model, optns = list()){

  if("plotTitle" %in% names(optns)){
    plotTitle = optns$plotTitle
  }else{
    plotTitle <- "Eruption Plot"
  }

  ###for pvalue adjustment, options the same as listed in stats::p.adjust c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  if("method" %in% names(optns)){
    method = optns$method
  }else{
    method <- "none"
  }

  #choose which principal component loadings to plot (default PC1) and which PC scores to calculate correlation with
  if("PC" %in% names(optns)){
    PC <- optns$PC
  }else{
    PC <- 1
  }

  if(is(model)[1]== "list"){
    id <- as.data.frame(colnames(model$data$rawData))
    model$data$rawData$factor <- as.numeric(as.factor(optns$factor))
    df <- model$data$rawData
    pcLoadings<-as.data.frame(model$data$loadings[,PC])
    gl <- labs(x = "Cliff's Delta",
               y = paste0("PC", PC, "loadings"))
  }

  if(is(model)[1]=="opls"){
    id <- as.data.frame(colnames(as.data.frame(model@suppLs[["x"]])))
    df <- as.data.frame(model@suppLs[["x"]])
    df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
    if(grepl("O", model@typeC) == TRUE){
      pcLoadings<-as.data.frame(model@orthoLoadingMN[,PC])
      gl <- labs(x = "Cliff's Delta",
                 y = paste0("PC", PC, " orthoLoadings"))
    }else{
      pcLoadings<-as.data.frame(model@loadingMN[,PC])
      gl <- labs(x = "Cliff's Delta",
                 y = paste0("PC", PC, "predLoadings"))}
  }

  #ensure "factor" isn't included in id
  if(any(id == "factor")){
    idx <- which(id == "factor")
    id <- as.data.frame(id[-idx,])
  }

#cliffs delta
    cd <- cliffsDelta(model = model, optns = optns)

#correlations between scaled + centered original data and scores
    if(is(model)[1]== "list"){
      corr <- t(as.data.frame(cor(model$data$scores[,PC], model$data$dataSC)))
    }

    if(is(model)[1]=="opls"){
      corr <- t(as.data.frame(cor(model@scoreMN[,PC], model@suppLs[["xModelMN"]])))
    }

#Fold change
  fc <- foldChange(model = model, optns = optns)

#adjusted p-value
pval<-list()
for(i in 1:(ncol(df)-1)){
  pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
}

unlist(pval)
pvalAdjusted <- p.adjust(pval, method = method)
pvalRescaled <- abs(log10(pvalAdjusted))
pvalRescaled <- as.data.frame(pvalRescaled)

#eruption data frame
ed<-cbind(cd, fc, pvalRescaled, pcLoadings, id, corr)

colnames(ed)<-c("cd", "log2FC", "pvalRescaled", "PCloadings", "id", "correlation")

#univariate plot
# ggplot(data = results, aes(x = log2FC, y = pvalRescaled)) +
#   geom_point(size = 3,
#              shape = 16,
#              alpha = 0.3) +
#   theme_minimal() +
#   geom_hline(yintercept = -log10(0.05), col = "black") +
#   geom_vline(xintercept = c(-0.6, 0.6), col = "black") +
#   geom_label_repel(aes(label = id),
#                    colour = "black",
#                    min.segment.length = 0.001) +
#   labs(title = plotTitle,
#        x = "log2FoldChange",
#        y = "-log10 adj.p-val")

#plot colourcoding
if("colourCoding" %in% names(optns)) {
  if (optns$colourCoding == "correlation") {
    plot <- ggplot(data = ed, aes(x = cd,
                                  y = PCloadings,
                                  colour = correlation))

  }
} else{
  plot <- ggplot(data = ed, aes(x = cd,
                                y = PCloadings,
                                colour = pvalRescaled))
}

#plot
eruptionPlot <- plot +
  geom_label_repel(aes(label = id),
                   colour = "black",
                   min.segment.length = 0.001) +
  geom_point(size = 3,
             shape = 16,
             alpha = 0.3) +
  theme_bw() +
  scale_x_continuous(limits = c(-1, 1)) +
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
    guide = "colourbar") +
  ggtitle(plotTitle) +
  gl +
  # labs(x = "Cliff's Delta",
  #      y = paste0("PC", PC, "loadings")) +
  theme(panel.grid.minor = element_blank(),
        plot.tag = element_text(face = "bold",
                                size = 25),
        legend.position = "right",
        legend.direction = "vertical")

#append to data and plots
# model$plots <- append(model$plots, list(eruptionPlot = eruptionPlot))
# model$data <- append(model$data, list(eruptionData=ed))
print(eruptionPlot)
# return(model)
}

# #logFC	PValue	FDR cliffsDelta PrincipalComponentLoadings

