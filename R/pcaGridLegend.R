#' Legend plot for ggpairs.
#'
#' This function creates a graph that is used in ggpairs based functions, such as code\pcaGrid, for its legend. It uses PC1 and PC2 as they will always be computed.
#'
#' Any aesthetic set to a numeric value will not appear in the legend. Any aesthetic set to a variable requires the legend title be specified.
#'
#' @param CO Colour, must be defined by a variable. Must have a discrete scale.
#' @param SZ Size.
#' @param AL Alpha.
#' @param SH Shape.
#' @param COtitle Character/s representing the Colour legend title.
#' @param SZtitle Character/s representing the Size legend title.
#' @param ALtitle Character/s representing the Alpha legend title.
#' @param SHtitle Character/s representing the Shape legend title.
#' @return A plot used in the grab_legend function in the pcaGrid and plotLoadingsGrid functions.


#plot of PC1 vs PC2, used for the legend of the PCA grid.
pcaGridLegend<-function(screeCumulativeThresholdObject, x = "PC1", y = "PC2", CO, SH, AL, SZ, COtitle, SHtitle, SZtitle, ALtitle){

# Get required data for plotting
  output <- plotInput(screeCumulativeThresholdObject, CO, SH, SZ, AL)

#combinations for which legend guides should appear
  gu <- if(class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
    guides(alpha = "none", size = "none")
  } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
    guides(alpha = "none", shape = "none")
  } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
    guides(shape = "none", size = "none")
  } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
    guides(alpha = "none" )
  } else if (class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
    guides(size = "none")
  } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
    guides(shape = "none")
  } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
    guides()
  } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
    guides(shape = "none", size = "none", alpha = "none")
  }

#plot used only for its legend in ggpairs in pcaGrid function
  test <- ggplot(data = output$data$pcdf, aes(x = .data[[x]], y = .data[[y]], color = output$CO, shape = output$SH, alpha = output$AL, size = output$SZ)) +
    geom_point() +
    gu +
    scale_alpha(range = c(0.1, 1)) +
    # scale_fill_manual(values=c('red','blue','green','red','blue')) +
    # scale_colour_manual(values=c('red','blue','green','red','blue')) +
    scale_color_brewer(palette="Set2") +
    labs(color = COtitle, shape = SHtitle, size = SZtitle, alpha = ALtitle ) +
    theme_minimal()
  return(test)
}
