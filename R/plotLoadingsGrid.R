#' Grid of plotloadings
#'
#' Grid of loadings plots up to the threshold number, using the GGally:ggpairs function.
#'
#' @param screeCumulativeThresholdObject An object computed from the screeCumulativeThreshold function.
#'
#' @return grid of loadings plots, up to the threshold number.
#' @examples
#egA <- pcResults(BIOspcglyc[,1:8], annotation = BIOspcglyc[,-1:-8])
#egB <- screeCumulativeThreshold(pcResultsObject = egA, cutoff = 95)
#egD <- plotLoadingGrid(screeCumulativeThresholdObject = egB)

#Make PCA grid with ggpairs
plotLoadingGrid <- function(screeCumulativeThresholdObject, gridTitle = "Loadings Plot Grid"){

output <- screeCumulativeThresholdObject
df <- as.data.frame(output$data$loadings)
thresh <- output$data$threshold

#Loop for creating titles of "PC(explained variance %)"
title <- list()
for (i in 1:thresh) {
  title[[i]] <- paste0('PC', i, ' (', round(output$data$pcSum$`Proportion of Variance`[i], 1), '%)')
}
title<-unlist(title)

#create loadings plot grid
gridTitle = gridTitle

plotLoadingGrid <- GGally::ggpairs(df[,1:thresh],
                           columnLabels = c(title),
                           title = gridTitle,
                           diag="blank",
                           upper="blank",
                           #upper=list(continuous =my_fn1),
                           lower=list(continuous =myFn2),
                           #legend = grab_legend(test),
                           progress = F, switch="both") +
                           geom_point(color= "blue", size = 1) +
                           geom_text_repel(aes(label = rownames(df)), size = 3.5) +
                           #geom_text_repel(aes(label = rownames(df)), size = 3, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
                           #geom_text(aes(label = rownames(df)), size = 2, colour = "blue", hjust=0, vjust=0) +
                           theme_bw() +
                           theme(strip.background = element_rect(fill = "white"), axis.text.x=(element_text(size=rel(0.7), angle=0)),
                                                    axis.text.y=(element_text(size=rel(0.7), angle=0)), panel.grid.major = element_blank(),
                                                    panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA,colour = "grey35"))

#remove empty grid spaces (lower and diagonal)

final_plot <- gPairsLower(plotLoadingGrid)

output$plots <- append(output$plots, list(plotLoadingGrid = plotLoadingGrid))

return(output)
}
