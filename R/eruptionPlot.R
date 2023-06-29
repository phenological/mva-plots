#' Eruption Plot.
#'
#' Eruption plot.
#'
#' @param model A pcResults object.
#' @param factor A column from the data frame of metadata. Must be a two factor variable such as treatment and control.
#' @param optns An empty list for additional options.
#' @param plotTitle A parameter for the \code{optns} list. A character for the title of the plot. Default is "Eruption Plot".
#' @param method A parameter for the \code{optns} list. Determines the method to adjust p-values by. The options the same as listed in stats::p.adjust ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"). The default is "none".
#' @param PC A parameter for the \code{optns} list. A numeric for which principal component to use for the loadings (for the plot y-axis) and scores (if correlation is chosen for \code{colourCoding}).
#' @param colourCoding A parameter for the \code{optns} list. Colour coding for the eruption plot, either the adjsuted and rescaled p-value or the correlation. The default is the p-value. To switch set to "correlation".
#' @return The model list appended with the eruption plot and the data to make the plot.
#'
#' @examples
#' data(mtcars)
#'
#' a <- pcResults(data = mtcars[,1:7], annotation = mtcars[,8:11], center = TRUE, scale. = TRUE)
#' b <- eruptionPlot(model = a, factor=mtcars[,"vs"], optns=list(colourCoding = "correlation", plotTitle = "mtcars eruption", method = "bonferroni"))
#' To access a single plot from the grid: b[["plots]][["pcaGrid"]][j,i], where j is the vertical and i is the horizontal position of the specific plot in the grid.

eruptionPlot <- function(model, factor, optns = list()){

  id <- as.data.frame(colnames(model$data$rawData))

  #ensure "factor" isn't included in id
  if(any(id=="factor")){
    idx <- which(id == "factor")
    id <- as.data.frame(id[-idx,])
  }

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
    PC = optns$PC
  }else{
    PC <- 1
  }

    model$data$rawData$factor <- as.numeric(as.factor(factor))

    df <- model$data$rawData

#cliffs delta
    cd <- cliffsDelta(model = model, factor = factor)

#correlations between scaled + centered original data and scores
  corr <- t(as.data.frame(cor(model$data$scores[,PC], model$data$dataSC)))

#Fold change
  fc <- foldChange(model = model, factor = factor)

#adjusted p-value
pval<-list()
for(i in 1:(ncol(df)-1)){
  pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
}

unlist(pval)
pvalAdjusted <- p.adjust(pval, method = method)
pvalRescaled <- abs(log10(pvalAdjusted))
pvalRescaled <- as.data.frame(pvalRescaled)

#loadings and id
pcLoadings<-as.data.frame(model$data$loadings[,PC])
#id<-as.data.frame(row.names(pcLoadings))

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
  # geom_point(size = 3,
  #            shape = 1) +
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
    guide = "colourbar"
  ) +
  ggtitle(plotTitle) +
  labs(x = "Cliff's Delta",
       y = paste0("PC", PC, "loadings")) +
  theme(panel.grid.minor = element_blank(),
        plot.tag = element_text(face = "bold",
                                size = 25),
        legend.position = "right",
        legend.direction = "vertical")

#append to data and plots
model$plots <- append(model$plots, list(eruptionPlot = eruptionPlot))
model$data <- append(model$data, list(eruptionData=ed))

return(model)
}

# #logFC	PValue	FDR cliffsDelta PrincipalComponentLoadings

