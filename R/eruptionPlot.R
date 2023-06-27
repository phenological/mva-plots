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
  scale_colour_gradientn(colours = rainbow(7)) +
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

#   function (ref, comp)
#   {
#     if (!is.numeric(ref) | !is.numeric(comp))
#       stop("Input not numeric")
#     ind_ref <- is.na(ref) | is.infinite(ref)
#     ind_comp <- is.na(comp) | is.infinite(comp)
#     if (any(ind_ref)) {
#       ref <- ref[!ind_ref]
#       message("Removing NA or infinite values from reference group.")
#     }
#     if (any(ind_comp)) {
#       comp <- comp[!ind_comp]
#       message("Removing NA or infinite values from comparator group.")
#     }
#     if (length(ref) < 5 | length(comp) < 5)
#       stop("Low number of values (< 5)")
#     top_counts <- vapply(ref, function(x, y = comp) {
#       names(x) <- NULL
#       names(y) <- NULL
#       c(length(which(x > y)), length(which(x < y)))
#     }, FUN.VALUE = c(2, length(ref)))
#     out <- ((sum(top_counts[1, ]) - sum(top_counts[2, ]))/(length(ref) *
#                                                              length(comp))) * (-1)
#     return(out)
#   }
#
#
#   mag.levels = c(0.147,0.33,0.474) ## effect sizes from (Hess and Kromrey, 2004)
#   magnitude = c("negligible","small","medium","large")
#
#   #Perform FDR adjustment on P-values (false discovery rate method or Banjamini and Hochberg method). make -log10 value for volcano plot
#
#   p_adj<- -log10(p.adjust(p.val, method = "fdr"))
#
#
#                         df <- data
#
#                         eruptionPlot<- ggplot(data = df, aes(x = df$LogFC, y = df$p_adj)) +
#                                       geom_point() +
#                                       theme_minimal() +
#                                       geom_hline(yintercept = -log10(0.05),
#                                                  col = "red") +
#                                       geom_vline(xintercept = c(-0.6, 0.6),
#                                                  col = "red") +
#                                       geom_label_repel(aes(label = rownames(df)),
#                                                        size = 2) +
#                                       labs(title = title,
#                                            x = "log2FoldChange",
#                                            y = "-log10 adj.p-val")
#
#                         return(eruptionPlot)
#                         }
#
# #From metabom8:
#
#   function (model, pc = 1, p_adj = "BH", invert_es = FALSE)
# {
#   if (is.na(p_adj) || is.infinite(p_adj) || length(p_adj) >
#       1) {
#     p_adj <- "none"
#   }
#   if (length(unique(model@Y$ori)) > 2) {
#     stop("Eruption plot defined for two-level outcome.")
#   }
#   if (is.na(pc) || is.infinite(pc) || length(pc) > 1)
#     stop("Check pc argument.")
#   if (grepl("o", pc)) {
#     pc1 <- as.numeric(gsub("o", "", pc))
#     if (is.na(pc1) || is.infinite(pc1) || pc1 > nrow(model@p_orth)) {
#       stop("Check pc argument and help section.")
#     }
#     ddl <- data.frame(x = model@p_orth[pc1, ], id = colnames(model@X))
#   }
#   else {
#     ddl <- data.frame(x = model@p_pred[1, ], id = colnames(model@X))
#   }
#   Y <- model@Y$ori
#   if (invert_es) {
#     comp <- unique(Y)[2]
#     cat("Using ", unique(Y)[1], " as reference group for Cliff's delta. Swap reference / comparator with invert_es=TRUE (see function help).")
#   }
#   else {
#     comp <- unique(Y)[1]
#     cat("Using ", unique(Y)[2], " as reference group for Cliff's delta.")
#   }
#
# #scores? and cliffs delta
#   uni <- t(apply(model@X, 2, function(x, idx = which(Y == comp),
#                                     y = Y) {
#     c(es_cdelta(x[idx], x[-idx]), kruskal.test(x, y)$p.value)
#   }))
#
# #data frame
#   ddl <- cbind(ddl, uni)
#   colnames(ddl) <- c("p1", "id", "Cd", "pval")
#   ddl$p1 <- abs(ddl$p1)
#
#   adj_log <- p_adj %in% c("holm", "hochberg", "hommel", "bonferroni",
#                           "BH", "BY", "fdr")
#   if (adj_log) {
#     ddl$pval_adjusted <- p.adjust(ddl$pval, method = p_adj)
#     ddl$pval_transformed <- abs(log10(ddl$pval_adjusted))
#   }
#   else {
#     ddl$pval_transformed <- abs(log10(ddl$pval))
#   }
#
# #eruption plot
#   gl2 <- ggplot(ddl, aes_string(x = "Cd", y = "p1", colour = "pval_transformed")) +
#     geom_label_repel(aes_string(label = "id"), colour = "black", min.segment.length = 0.001) +
#     geom_point(size = 3, shape = 1) +
#     geom_point(size = 3, shape = 16, alpha = 0.3) +
#     theme_bw() +
#     scale_x_continuous(limits = c(-1, 1)) +
#     scale_colour_gradientn(colours = matlab.like2(10)) +
#     labs(x = "Cliff's Delta", y = paste0("|p_", pc, "|")) +
#     theme(panel.grid.minor = element_blank(), plot.tag = element_text(face = "bold", size = 25), legend.position = "top", legend.direction = "horizontal")
#
#  if (adj_log) {
#     gl2 <- gl2 + labs(colour = expression("| log10( p value"["adj"] ~
#                                             ") |"))
#   }
#   else {
#     gl2 <- gl2 + labs(colour = "| log10( p value ) |")
#   }
#
#   ddl = ddl[, !colnames(ddl) %in% "pval_transformed"]
#
#   return(list(data = ddl, plot = gl2))
# }
