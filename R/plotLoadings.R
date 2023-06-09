#' Plot loadings.
#'
#' Grid of the loadings plots using GGally::ggpairs up to a threshold number.
#'
#' @param model A PCA object.
#' @param optns An empty list for aesthetic options.
#' @param gridTitle A parameter for the optns list. A character for the title of the grid.
#' @param thresh A parameter for the optns list. A numeric for the number of PCAs to display in the grid. The default is calculated in the PCA function.
#'
#' @return The model list appended with the grid of loadings under plots.
#' @examples
#' data(iris)
#' a <- PCA(data = iris[,1:4], annotation=[,5], center = TRUE, scale. = TRUE)
#' b <- plotLoadings(model = a, optns = list(gridTitle = "Iris Dataset PC Loadings", thresh = 3))
#'
#' To access a single plot from the grid: b[["plots]][["plotLoadingsGrid"]][j,i], where j is the vertical and i is the horizontal position of the specific plot in the grid.


plotLoadings <- function(model, optns=list()){
  #Grid title (working)
  if("gridTitle" %in% names(optns)){
    gridTitle = optns$gridTitle
  }else{
    gridTitle <- "Plot Loadings Grid"
    #cat(yellow("Using default gridTitle", "!\n"))
  }

  #number of pcas (working)
  if("thresh" %in% names(optns)){
    thresh = optns$thresh[1]
  }else{thresh = model$data$threshold}

  #Loop for creating titles of "PC(explained variance %)"
  title <- list()
  for (i in 1:thresh) {
    title[[i]] <- paste0('PC', i, ' (', round(model$data$pcSum$`Proportion of Variance`[i], 1), '%)')
  }
  title<-unlist(title)

 df<- as.data.frame(model$data$loadings)

  plotLoadingGrid <- GGally::ggpairs(data = df[,1:thresh],
                                     columnLabels = c(title),
                                     title = gridTitle,
                                     diag="blank",
                                     upper="blank",
                                     #upper=list(continuous =my_fn1),
                                     lower=list(continuous =myFn2),
                                     progress = F,
                                     switch="both") +
                                     geom_point(color= "blue",
                                               size = 1) +
                                    #geom_label(label = rownames(df))+
                                     geom_text_repel(aes(label = rownames(df)),
                                                        size = 3.5) +
                                     theme_bw() +
                                     theme(strip.background = element_rect(fill = "white"),
                                          axis.text.x = (element_text(size = rel(0.7),
                                                                    angle = 0)),
                                          axis.text.y = (element_text(size = rel(0.7),
                                                                      angle = 0)),
                                          panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank(),
                                          panel.border = element_rect(fill = NA,
                                                                      colour = "grey35"))

  plotLoadingGrid <- gPairsLower(plotLoadingGrid)

  model$plots <- append(model$plots, list(plotLoadingGrid = plotLoadingGrid))

  print(plotLoadingGrid)
  invisible(model)
}
