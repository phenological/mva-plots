
#' Manhattan plot
#'
#' @param data Data to be supplied
#' @param x numeric or factor, that goes on x-axis
#' @param y numeric, can be p-value, can be concentration depending on data
#' @param point_shape numeric, geom_point shape in ggplot. default is 1; an empty circle
#' @param point_size numeric, geom_point size in ggplot. default is 2
#' @param point_alpha numeric, geom_point alpha in ggplot. default is 1
#' @param show_legend boolean, to show legends on the right hand side of the plot. default is FALSE. If interactive is TRUE then it overwrite to show_legend = TRUE
#' @param log_scaling boolean, to apply -log10 to the y value. default is TRUE. If it is TRUE then threshold value is automatically log scaled.
#' @param threshold numeric, add horizontal dash line to the plot. If log_scaling =TRUE then threshold value is automatically log scaled.
#' @param interactive boolean, to make the plot interactive. default is FALSE.
#' @return a ggplot2 object.
#' @importFrom plotly ggplotly
#' @importFrom crayon red
#' @export

manhattanPlot<-function(data,x,y,point_shape = 1,point_size = 2,point_alpha = 1,show_legend=FALSE,log_scaling=TRUE,threshold = NA,xlab = "",ylab = "",main_title = "Manhattan Plot",interactive = FALSE){
  if(!exists("x")|!exists("y")){
    cat(red("mvaPlots::Manhattanplot >>"
                    ,"Please specify x and y for the plot\n"))
    stop()
  }
  x = data[[x]]
  y = data[[y]]
  if(log_scaling==TRUE){
    y<- -log10(as.numeric(y))
  }else{
    y<-as.numeric(y)
  }

  Manhattan_plot<-ggplot(data,aes(x,y)) +
    geom_point(aes(color = factor(x)),
               shape = point_shape,
               size = point_size,
               alpha = point_alpha) +
    theme_minimal() +
    labs(title = main_title,
         x = xlab,
         y = ylab)

  # show legend or not
  if(show_legend==FALSE){
    Manhattan_plot <- Manhattan_plot +
                      theme(legend.position = "n")
  }

  if(!exists("threshold")){
    if(log_scaling==TRUE){
      threshold <- -log10(as.numeric(threshold))
    }else{
      threshold <- as.numeric(threshold)
    }

    Manhattan_plot <- Manhattan_plot +
                      geom_hline(yintercept = threshold,
                                 linetype = "dashed",
                                 color = "black")
  }

  if(interactive==TRUE){
    Manhattan_plot <- ggplotly(Manhattan_plot)
  }

  return(Manhattan_plot)
}


# devtools::load_all("~/git/phenological/fusion/")
# library(tidyverse)
# da<-local(get(load("~/OneDrive - Murdoch University/datasets/covid19/bioGune/dataElements/covid19_bioGune_PLA_LIPO.daE")))
# tdf<-data.frame(da@obsDescr[[1]],
#                 da@.Data)
# data<-tdf%>%pivot_longer(cols = !sampleID:Sample.file.name,
#                          names_to = "para",
#                          values_to = "y")%>%
#   mutate(log10_y = -log10(y))
#
# x = sampleID
#
# manhattanPlot(data = data, x = "sampleID", y = "y",point_shape=1,log_scaling = TRUE,threshold = 1,interactive = TRUE)



