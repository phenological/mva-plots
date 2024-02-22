#' Plot loadings.
#'
#' Grid of the loadings plots using GGally::ggpairs up to a threshold number.
#'
#' @param model A PCA, oplsda or ropls object.
#' @param flat A logical for a flat O-PLS(DA) loadings plot. Only applicable to
#' oplsda models with an orthogonal component. Default is FALSE.
#' @param optns An empty list for addtional options:
#'    \itemize{
#'     \item{plotTitle}{A character for the title of the grid.}
#'     \item{theme}{Personalize the plot theme you would like applied as you
#'     would using theme() in ggplot. Example set
#'     theme = theme(legend.position = "left", text=element_text(size=5)) in optns.}
#'     \item{thresh}{A numeric for the number of PCAs to display in the grid.
#'     The default is calculated in the PCA function.}
#' }
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


plotLoadings <- function(model, flat = FALSE, optns=list()){
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
      df <- as.data.frame(cbind(model@loadingMN, model@orthoLoadingMN), check.names = F)
      gl <- labs(x = paste0('p1 (', round(model@modelDF[["R2X"]][1]*100, 1), '%)'),
                 y = paste0('po1'))
    }else{
      df <- as.data.frame(model@loadingMN, check.names = F)
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

      if(flat == TRUE){
        onePlot <- ggplot(data = df,
                          aes(x = df[,PCi], y = 0.001)) +
          ggtitle(plotTitle) +
          gl +
          geom_point(color= "blue",
                     size = 1) +
          scale_y_continuous(limits = c(0, 0.1),
                             expand = c(0, 0)) +  # Set limits and remove expansion
          geom_text_repel(
            aes(label = rownames(df)),
            size = 3,
            angle = 90,
            #segment.color = NA, # Remove the connecting segments
            nudge_x = 0.011,        # Center the label horizontally
            nudge_y = 0.0051,      # Move the label above the point
            direction = "x",    # Orient the labels vertically
            hjust = 0.1,        # Center the label horizontally
            vjust = -1,
            box.padding = 0.05) +       # Move the label above the point
          theme_bw() +
          theme+
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.line.y=element_blank())

      }


      print(onePlot)

      model@suppLs[["LoadingsPlot"]] <- onePlot
      return(model)
    }

  #########PCA objects##################
  if(is(model)[1]=="list"){

    df<- as.data.frame(model$data$loadings, check.names = F)

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

