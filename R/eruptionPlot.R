#' Eruption Plot.
#'
#' Eruption plot.
#'
#' @param model A PCA or oplsda object.
#' @param optns An empty list for additional options.
#' @param factor A parameter for the \code{optns} list used when supplying a PCA object. An object the same length as the data used to build the PCA model that must be a two factor variable such as treatment and control.
#' @param plotTitle A parameter for the \code{optns} list. A character for the title of the plot. Default is "Eruption Plot".
#' @param method A parameter for the \code{optns} list. Determines the method to adjust p-values by. The options the same as listed in stats::p.adjust ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"). The default is "bonferroni".
#' @param PC A parameter for the \code{optns} list when supplying a PCA object. A numeric for which principal component to use for the loadings (for the plot y-axis) and scores (if correlation is chosen for \code{colourCoding}).
#' @param color A parameter for the \code{optns} list. Color coding for the eruption plot, choose from the adjusted and re-scaled p-value "pval", correlation "corr", log2 fold change "fc" or cliff's delta "cd". The default is the correlation.
#' @param continuousPalette A parameter for the \code{optns} list. Color palette for continuous values, use hexadecimal values (example and default: continuousPalette =c("#0000CC","#0000FF","#0055FF","#00AAFF","#00FFFF","#2BFFD5","#55FFAA","#80FF80","#AAFF55","#D4FF2B","#FFFF00","#FFAA00","#FF5500","#FF0000","#CC0000")), grDevices names (example: continousPalette = rainbow(4)) or color names (example : continuousPalette =c("purple", "orange")).
#' @param x A parameter for the \code{optns} list. Choose your x-axis using the same options stated for color. The default is cliff's delta.
#' @param y A parameter for the \code{optns} list. Choose your y-axis using the same options stated for color. The default is loadings.
#' @return The eruption plot is printed and the model is appended with the eruption data (cliffs Delta, p-value, correlation, loadings). For ropls object, eruptionData is found in suppLs.
#'
#' @import ggplot2
#' @import scales
#' @import ggrepel
#' @import GGally
#' @import egg
#' @examples
#' data(mtcars)
#'
#' a <- PCA(data = mtcars[,1:7], center = TRUE, scale. = TRUE)
#' b <- eruptionPlot(model = a,  optns=list(factor=mtcars[,"vs"], color = "corr", plotTitle = "mtcars eruption", method = "bonferroni"))

eruptionPlot <- function(model, optns = list()){

  if("plotTitle" %in% names(optns)){
    plotTitle = optns$plotTitle
  }else{
    plotTitle <- "Eruption Plot"
  }

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

  ###for pvalue adjustment, options the same as listed in stats::p.adjust c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  if("method" %in% names(optns)){
    method = optns$method
  }else{
    method <- "bonferroni"
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
    pcLoadings<-as.data.frame(abs(model$data$loadings[,PC]))

  }

  if(is(model)[1]=="opls"){
    id <- as.data.frame(colnames(as.data.frame(model@suppLs[["x"]])))
    df <- as.data.frame(model@suppLs[["x"]])
    df$factor <- as.numeric(as.factor(model@suppLs[["yMCN"]]))
    pcLoadings<-as.data.frame(abs(model@loadingMN[,PC]))
  }

  #ensure "factor" isn't included in id
  if(any(id == "factor")){
    idx <- which(id == "factor")
    id <- as.data.frame(id[-idx,])
  }

########cliffs delta##########
    cd <- cliffsDelta(model = model, optns = optns)

##########correlations between scaled + centered original data and scores######
    if(is(model)[1]== "list"){
      corr <- abs(t(as.data.frame(cor(model$data$scores[,PC], model$data$dataSC))))
    }

    if(is(model)[1]=="opls"){
      corr <- abs(t(as.data.frame(cor(model@scoreMN[,PC], model@suppLs[["xModelMN"]]))))
    }

##########Fold change#########
  fc <- foldChange(model = model, optns = optns)

########adjusted p-value########
pval<-list()
for(i in 1:(ncol(df)-1)){
  pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
}

unlist(pval)
pvalUnadjusted <- t(as.data.frame(pval))
pvalAdjusted <- p.adjust(pval, method = method)
pvalRescaled <- abs(log10(pvalAdjusted))
pvalRescaled <- as.data.frame(pvalRescaled)

#eruption data frame
ed<-cbind(cd, fc, pvalRescaled, pvalUnadjusted, pcLoadings, id, corr)

colnames(ed)<-c("cd", "fc", "pval", "pvalRaw", "loadings", "id", "corr")

#for graph labs
labels<-list("Cliff's Delta", "Log2FC", "|log10pval|", "|loadings|", "id", "|corr|")
names(labels)<-c("cd", "fc", "pval", "loadings", "id", "corr")

if("color" %in% names(optns)) {
  color <- ed[, optns$color]

} else{
  color <- ed$corr
  optns$color <- "corr"
}

if (optns$color == "pval") {
  color_breaks <- c(0, 1.3, max(ed[,"pval"]))
  values <- scales::rescale(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.3, 1.5, 2, max(ed[,"pval"])))
  limits <- c(0, max(ed[,"pval"]))
}
else{
  color_breaks <- c(0, 0.5, 1)
  values <- NULL
  limits <- c(0, 1)
}

if("x" %in% names(optns)){
  x <- ed[,optns$x]
}else{x <- ed$cd
optns$x <-"cd"}

if("y" %in% names(optns)){
  y <- ed[,optns$y]
}else{y <- ed$loadings
optns$y <- "loadings"}

############plot#########
eruptionPlot <- ggplot(data = ed, aes(x = x,
                                      y = y,
                                      color = color)) +
  labs(x = labels[optns$x],
       y = labels[optns$y],
       color = labels[optns$color]) +
  geom_label_repel(aes(label = id),
                   colour = "black",
                   min.segment.length = 0.001) +
  geom_point(size = 3,
             shape = 16,
             alpha = 0.3) +
  theme_bw() +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_gradientn(
    values = values,
    colours = optns$continuousPalette,  # Use the custom color palette,
    breaks = color_breaks,  # Specify the breaks
    labels = scales::number_format(accuracy = 0.01), #2 decimal points on colourbar scale
    limits = limits,
    trans = "identity",
    na.value = "grey50",
    guide = "colourbar"
  ) +
  ggtitle(plotTitle) +
  theme(panel.grid.minor = element_blank(),
        plot.tag = element_text(face = "bold",
                                size = 25),
        legend.position = "right",
        legend.direction = "vertical")

########p-value legend##########
#makes a separate plot with evenly spread colourbar

  if("color" %in% names(optns)){

    if(optns$color == "pval"){
      ma<- round(max(ed$pval),1)
      color_breaks <- c(0, 1.3, ma)
      values <- scales::rescale(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.3, 1.5, 2, ma))
      limits <- c(0, ma)

      plot1 <- ggplot(data = ed,
                      aes(x = cd,
                          y = fc,
                          color = pval)) +
        geom_point() +
        scale_color_gradientn(
          colours = optns$continuousPalette,  # Use the custom color palette,
          breaks = c(0,12,ma),  # Specify the breaks
          labels=c("0", "1.3", ma),
          limits = limits,
          na.value = "grey50",
          guide = "colourbar"
        ) +
        labs(color = "|log10pval|") +
        theme_minimal() +
        theme(legend.title = element_text(margin = margin(t = 100)))

      eruptionPlot<- eruptionPlot +
                      labs( caption = paste0("p-value adjustment method: ", method) )

      eruptionPlot <- ggarrange(plots = list(eruptionPlot),
                                legend.grob = (ggpubr::get_legend(plot1)),
                                legend = "right")

    }

  }

print(eruptionPlot)
#########append#############

#append to data and plots
if(is(model)[1] == "list"){
  model$plots <- append(model$plots, list(eruptionPlot = eruptionPlot))
  model$data <- append(model$data, list(eruptionData = ed))
}

if(is(model)[1] == "opls"){
  model@suppLs[["eruptionData"]] <- append(x = data.frame(), values = ed)
  model@suppLs[["eruptionPlot"]] <- eruptionPlot
}

invisible(model)

}

