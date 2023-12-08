#' Plot loadings.
#'
#' Grid of the loadings plots using GGally::ggpairs up to a threshold number.
#'
#' @param model A PCA, oplsda or ropls object.
#' @param optns An empty list for aesthetic options.
#' @param plotTitle A parameter for the optns list. A character for the title of the grid.
#' @param theme A parameter for the \code{optns} list. Personalize the plot theme you would like applied as you would using theme() in ggplot. Example set theme = theme(legend.position = "left", text=element_text(size=5)) in optns.
#' @param thresh A parameter for the optns list. A numeric for the number of PCAs to display in the grid. The default is calculated in the PCA function.
#'
#' @return The model list appended with the grid of loadings under plots.
#' @import GGally
#' @import methods
#' @importFrom ggrepel geom_text_repel
#' @examples
#' data(iris)
#' a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
#' b <- plotLoadings(model = a, optns = list(gridTitle = "Iris Dataset PC Loadings", thresh = 3))
#' @export


plotLoadings <- function(model, optns=list()){
  #plot title (working)
  if("plotTitle" %in% names(optns)){
    plotTitle = optns$plotTitle
  }else{
    plotTitle <- "Loadings Plot"
  }

  #theme
  if(!("theme" %in% names(optns))){
    theme <- theme()
  } else{theme <- optns$theme}

  #########ropls objects##################
  if(is(model)[1]=="opls"){

    if(grepl("O", model@typeC) == TRUE){
      df <- as.data.frame(cbind(model@loadingMN, model@orthoLoadingMN))
      gl <- labs(x = paste0('p1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
                 y = paste0('po1'))
    }else{
      df <- as.data.frame(model@loadingMN)
      gl <- labs(x = paste0('p1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
                 y = paste0('p2 (', round(model@modelDF[["R2X"]][2]*100, 1), '%)'))
    }

      PCi <- 1
      PCj <- 2

      onePlot <- ggplot(data = df,
                        aes(x = df[,PCi], y = df[,PCj])) +
        ggtitle(plotTitle) +
        gl +
        geom_point(color= "blue",
                   size = 1) +
        geom_text_repel(aes(label = rownames(df)),
                        size = 3.5) +
        geom_hline(yintercept = 0, colour = "gray70") +
        geom_vline(xintercept = 0, colour = "gray70") +
        theme_bw() +
        theme

      print(onePlot)

      model@suppLs[["LoadingsPlot"]] <- onePlot
      return(model)
    }

  #########PCA objects##################
  if(is(model)[1]=="list"){

    df<- as.data.frame(model$data$loadings)

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

  if("PCi" %in% names(optns)){
    PCi <- optns$PCi
    PCj <- optns$PCj
    gl <- labs(x = title[PCi], y = title[PCj])

    onePlot <- ggplot(data = df,
                      aes(x = df[,PCi], y = df[,PCj])) +
      ggtitle(plotTitle) +
      gl +
      geom_point(color= "blue",
                 size = 1) +
      geom_text_repel(aes(label = rownames(df)),
                      size = 3.5) +
      geom_hline(yintercept = 0, colour = "gray70") +
      geom_vline(xintercept = 0, colour = "gray70") +
      theme_bw() +
      theme

    return(onePlot)
    }

  if(!("PCi" %in% names(optns))){
    plotLoadingGrid <- ggpairs(data = df[,1:thresh],
                                       columnLabels = c(title),
                                       title = plotTitle,
                                       diag="blank",
                                       upper="blank",
                                       #upper=list(continuous =my_fn1),
                                       lower=list(continuous =myFn2),
                                       progress = F,
                                       switch="both") +
      geom_point(color= "blue",
                 size = 1) +
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
                                        colour = "grey35")) +
      theme

    plotLoadingGrid <- gPairsLower(plotLoadingGrid)

    model$plots <- append(model$plots, list(plotLoadingGrid = plotLoadingGrid))

    print(plotLoadingGrid)
    invisible(model)
  }
  }

  }

