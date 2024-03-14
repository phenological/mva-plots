#' Eruption Plot.
#'
#' Eruption plot.
#'
#' @param model A PCA or oplsda object.
#' @param optns An empty list for additional options.
#' @param optns A list for additional options:
#'   \itemize{
  #'     \item{factor} {Used when supplying a PCA object. An object the same
  #'     length as the data used to build the PCA model that must be a two
  #'     factor variable such as treatment and control.}
  #'     \item{external} {Must be a numeric vector. An externally derived
  #'     p-value. If you do not wish to use the automatically calculated
  #'     p-value, which uses the non-parametric Kruskal-Wallis, provide your
  #'     own. If you have already adjusted it, set method to "none". It will
  #'     automatically be log10 scaled.}
  #'     \item{color} {Color coding for the eruption plot, choose from the
  #'     adjusted and re-scaled p-value "pval", correlation "corr", log2 fold
  #'     change "fc" or cliff's delta "cd". The default is the correlation.}
  #'     \item{fun} {Either mean or median for foldchange calculation. The
  #'     default is mean.}
  #'     \item{plotTitle} {A string specifying the plot title.Default is
  #'     "Eruption Plot".}
  #'     \item{method} {A string specifying the method parameter. Determines the
  #'     method to adjust p-values by. The options the same as listed in
  #'     stats::p.adjust ("holm", "hochberg", "hommel", "bonferroni", "BH",
  #'     "BY","fdr", "none"). The default is "bonferroni".}
  #'     \item{PC} {For a PCA object. A numeric for which principal component to
  #'     use for the loadings (for the plot y-axis) and scores (if correlation
  #'     is chosen for \code{colourCoding}).}
  #'     \item{continuousPalette} {Color palette
  #'     for continuous values, use hexadecimal values (example and default:
  #'      continuousPalette = c("#0000CC","#0000FF","#0055FF","#00AAFF","#00FFFF",
  #'      "#2BFFD5","#55FFAA","#80FF80","#AAFF55","#D4FF2B","#FFFF00","#FFAA00",
  #'      "#FF5500","#FF0000","#CC0000")),
  #'      grDevices names (example: continousPalette = rainbow(4)) or
  #'      color names (example : continuousPalette =c("purple", "orange")).}
  #'      \item{x} {Choose your x-axis using the same options stated for color.
  #'      The default is cliff's delta.}
  #'      \item{y} {Choose your y-axis using the same options stated for color.
  #'      The default is loadings.}
  #'   }
#' @return The eruption plot is printed and the model is appended with the
#' eruption data (cliffs Delta, p-value, correlation, loadings). For ropls
#' object, eruptionData is found in suppLs.
#'
#' @import ggplot2
#' @import scales
#' @import ggrepel
#' @import GGally
#' @import egg
#' @import methods
#' @importFrom ggpubr get_legend
#' @importFrom ggrepel geom_label_repel
#' @examples
#' data(mtcars)
#'
#' a <- PCA(data = mtcars[,1:7], center = TRUE, scale. = TRUE)
#' b <- eruptionPlot(model = a,
#'                   optns = list(factor = mtcars[,"vs"],
#'                               color = "corr",
#'                               plotTitle = "mtcars eruption",
#'                               method = "bonferroni"))
#' @export

eruptionPlot <- function(model, optns = list()){

  if (!"control" %in% names(optns)) {
    optns$control <- 1
    #print warning
    warning(paste0("No control specified in optns for factor. The first entry was set as the control"))
  }

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
     optns$method = optns$method
  }else{
    optns$method <- "bonferroni"
  }

  #choose which principal component loadings to plot (default PC1) and which PC scores to calculate correlation with
  if("PC" %in% names(optns)){
    PC <- optns$PC
  }else{
    PC <- 1
  }

  if(is(model)[1] == "list"){
    id <- as.data.frame(colnames(model$data$rawData), check.names = F)
    df <- model$data$rawData
    df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
    df <- as.data.frame(df, check.names = F)
    pcLoadings<-as.data.frame(abs(model$data$loadings[,PC]), check.names = F)

  }

  if(is(model)[1] == "opls"){
    df <- as.data.frame(model@suppLs[["x"]], check.names = F)
    id <- as.data.frame(colnames(df))
    if (!("factor" %in% optns)){
      optns[["factor"]] <- relevel(as.factor(model@suppLs[["yMCN"]]), ref = optns$control)
    }
    df[,"factor"] <- as.numeric(optns$factor)
    pcLoadings<-as.data.frame(abs(model@loadingMN[,PC]), check.names = F)
  }

  #stop if there are more than 2 groups in factor
  if(length(unique(df[,"factor"])) > 2){
    stop("Error: You have more than 2 levels in your factor")
  }

  #ensure "factor" isn't included in id
  if(any(id == "factor")){
    idx <- which(id == "factor")
    id <- as.data.frame(id[-idx,], check.names = F)
  }

########cliffs delta##########
    cd <- cliffsDelta(model = model, optns = optns)

##########correlations between scaled + centered original data and scores######
    if(is(model)[1]== "list"){
      corr <- abs(t(as.data.frame(cor(model$data$scores[,PC], model$data$dataSC), check.names = F)))
    }

    if(is(model)[1]=="opls"){
      corr <- abs(t(as.data.frame(cor(model@scoreMN[,PC], model@suppLs[["xModelMN"]]), check.names = F)))
    }

##########Fold change#########
  fc <- foldChange(model = model, optns = optns)

########p-value########

  if("external" %in% names(optns)){
    pval <- as.vector(optns$external)
    pvalUnadjusted <- (as.data.frame(pval))
  } else{
    pval<-list()
    for(i in 1:(ncol(df)-1)){
      pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
    }
    unlist(pval)
    pvalUnadjusted <- t(as.data.frame(pval, check.names = F))
  }

########p-value adjustment########
#keep for external provided p-value
#pvalUnadjusted <- t(as.data.frame(pval))
pvalAdjusted <- p.adjust(pval, method = optns$method)
pvalRescaled <- abs(log10(pvalAdjusted))
pvalRescaled <- as.data.frame(pvalRescaled, check.names = F)

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
  values <- rescale(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.3, 1.5, 2, max(ed[,"pval"])))
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
    labels = number_format(accuracy = 0.01), #2 decimal points on colourbar scale
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
      values <- rescale(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.3, 1.5, 2, ma))
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
                      labs( caption = paste0("p-value adjustment method: ", optns$method) )

      eruptionPlot <- ggarrange(plots = list(eruptionPlot),
                                legend.grob = (get_legend(plot1)),
                                legend = "right")

    }

  }


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
print(eruptionPlot)
}

