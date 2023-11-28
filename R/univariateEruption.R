#' Univariate eruption
#'

#' classifications:NS, log2FC, p-value, p-value and log2FC
#' NS not significant in any parameter
#' log2FC significant only
#' p-value signficant only
#' p-value and log2FC signficiant
# eruptionPlot <- function(model, factor, optns = list()){}
#
# id <- as.data.frame(colnames(model$data$rawData))
#
# #ensure "factor" isn't included in id
# if(any(id=="factor")){
#   idx <- which(id == "factor")
#   id <- as.data.frame(id[-idx,])
# }
#
# if("plotTitle" %in% names(optns)){
#   plotTitle = optns$plotTitle
# }else{
#   plotTitle <- "Eruption Plot"
# }
#
# ###for pvalue adjustment, options the same as listed in stats::p.adjust c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
# if("method" %in% names(optns)){
#   method = optns$method
# }else{
#   method <- "none"
# }
# model$data$rawData$factor <- as.numeric(as.factor(factor))
# df <- model$data$rawData
#
# cd <- cliffsDelta(model = model, factor = factor)
# fc <- foldChange(model = model, factor = factor)
# #adjusted p-value
# pval<-list()
# for(i in 1:(ncol(df)-1)){
#   pval[[i]]<-kruskal.test(df[,i], df[,"factor"])$p.value
# }
#
# unlist(pval)
# pvalAdjusted <- p.adjust(pval, method = method)
# pvalRescaled <- abs(log10(pvalAdjusted))
# pvalRescaled <- as.data.frame(pvalRescaled)
# ed<-cbind(cd, fc, pvalRescaled, id)
#
# colnames(ed)<-c("cd", "log2FC", "pvalRescaled", "PCloadings", "id", "correlation")
#
# #significance
# df$significance <- with(ed, ifelse(abs(log2FC) > 2 & pvalRescaled > -log10(0.05), "log2FC & adj.pval",
#                                    ifelse(abs(log2FC) >2 & pvalRescaled < -log10(0.05), "log2FC",
#                                           ifelse(abs(log2FC) <2 & pvalRescaled > -log10(0.05), "adj.pval",
#                                                  ifelse(abs(log2FC) <2 & pvalRescald < -log10(0.05), "NS")))))
# #significance
# "abs(log2FC)" >2
# "pvalRescaled"> -log10(0.05)
#
#
# #univariate plot
# ggplot(data = results, aes(x = log2FC, y = pvalRescaled, colour = significance)) +
#   geom_point(size = 3,
#              shape = 16,
#              alpha = 0.3) +
#   theme_bw() +
#   geom_hline(yintercept = -log10(0.05), col = "black") +
#   geom_vline(xintercept = c(-0.6, 0.6), col = "black") +
#   geom_label_repel(aes(label = id),
#                    colour = "black",
#                    min.segment.length = 0.001) +
#   ggtitle(plotTitle) +
#   labs(x = "log2FoldChange",
#        y = "-log10 adj.p-val")
#
#
# eruptionPlot <- plot +
#   geom_label_repel(aes(label = id),
#                    colour = "black",
#                    min.segment.length = 0.001) +
#   # geom_point(size = 3,
#   #            shape = 1) +
#   geom_point(size = 3,
#              shape = 16,
#              alpha = 0.3) +
#   theme_bw() +
#   scale_x_continuous(limits = c(-1, 1)) +
#   scale_colour_gradientn(colours = rainbow(7)) +
#
#   labs(x = "Cliff's Delta",
#        y = paste0("PC", PC, "loadings")) +
#   theme(panel.grid.minor = element_blank(),
#         plot.tag = element_text(face = "bold",
#                                 size = 25),
#         legend.position = "right",
#         legend.direction = "vertical")
