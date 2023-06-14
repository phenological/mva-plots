
# plotOptions <- list(colorCoding = "name of column / or vector of same size",
#                     colorLabels = "vector of same length as factor(colorCoding)",
#                     shapeCoding = "name of column / or vector of same size",
#                     shapeCodingLabels = "vector of same length as factor(colorLabel)",
#                     sizeCoding = "name of column / or vector of same size",
#                     sizeLabel = "vector of same length as factor(sizeCoding)")
# optns = list(plotOptns = plotOptions)
#
# plotOptions <- list(colorCoding = "covid_status")
# optns = list(plotOptns = plotOptions)
#
#
# optns = list(colorCoding = "name of column / or vector of same size",
#              colorLabels = "vector of same length as factor(colorCoding)",
#              shapeCoding = "name of column / or vector of same size",
#              shapeCodingLabels = "vector of same length as factor(colorLabel)",
#              sizeCoding = "name of column / or vector of same size",
#              sizeLabel = "vector of same length as factor(sizeCoding)")

#plotScores(mod, optns)




shellFunction <- function(data, mapping, method="stat_ellipse"){
  p <- ggplot(data = data, mapping = mapping) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashapeCodinged", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashapeCodinged", color = "black") +
    scale_color_brewer(palette = "Set2")
  # scale_fill_manual(values=c('red','blue', 'green')) +
  # scale_colour_manual(values=c('red','blue', 'green'))
  return(p)
}

plotscores<-function(model, optns=list()){

if("thresh" %in% names(optns)){
  thresh = thresh
}else{thresh=model$data$threshold}

if("colourCoding" %in% names(optns)){
    "colourCoding" = model$data$pcdf[,optns$colourCoding]
  }
else{colorCoding = "black"}

if("shapeCoding" %in% names(optns)){
  shapeCoding = shapeCoding
}
else{shapeCoding = "circle"}

if("sizeCoding" %in% names(optns)){
  sizeCoding = sizeCoding
}
else{sizeCoding = 3}

if("alphaCoding" %in% names(optns)){
  alphaCoding = alphaCoding
}
  else{alphaCoding = 0.5}

gp <- if(length(colourCoding) > 1 & length(shapeCoding) > 1 & length(sizeCoding) == 1 & length(alphaCoding) == 1) {
  geom_point(aes(colour = colourCoding, shape = shapeCoding), size = sizeCoding, alpha = alphaCoding)
}
else if (length(colourCoding) > 1 & length(shapeCoding) == 1 & length(sizeCoding) > 1 & length(alphaCoding) == 1) {
  geom_point(aes(colour = colourCoding, size = sizeCoding), shape = shapeCoding, alpha = alphaCoding)
}
else if (length(colourCoding) > 1 & length(shapeCoding) == 1 & length(sizeCoding) == 1 & length(alphaCoding) > 1) {
  geom_point(aes(colour = colourCoding, alpha = alphaCoding), shape = shapeCoding, size = sizeCoding)
}
else if (length(colourCoding) > 1 & length(shapeCoding) > 1 & length(sizeCoding) > 1 & length(alphaCoding) == 1) {
  geom_point(aes(colour = colourCoding, shape = shapeCoding, size = sizeCoding), alpha = alphaCoding)
}
else if (length(colourCoding) > 1 & length(shapeCoding) > 1 & length(sizeCoding) == 1 & length(alphaCoding) > 1) {
  geom_point(aes(colour = colourCoding, shape = shapeCoding, alpha = alphaCoding), size = sizeCoding)
}
else if (length(colourCoding) > 1 & length(shapeCoding) == 1 & length(sizeCoding) > 1 & length(alphaCoding) > 1) {
  geom_point(aes(colour = colourCoding, size = sizeCoding, alpha = alphaCoding), shape = shapeCoding)
}
else if (length(colourCoding) > 1 & length(shapeCoding) > 1 & length(sizeCoding) > 1 & length(alphaCoding) > 1) {
  geom_point(aes(colour = colourCoding, shape = shapeCoding, size = sizeCoding, alpha = alphaCoding))
}
else if (length(colourCoding) > 1 & length(shapeCoding) == 1 & length(sizeCoding) == 1 & length(alphaCoding) == 1) {
  geom_point(aes(colour = colourCoding), shape = shapeCoding, size = sizeCoding, alpha = alphaCoding)
}
else if(length(colourCoding) == 1 & length(shapeCoding) > 1 & length(sizeCoding) == 1 & length(alphaCoding) == 1) {
  geom_point(aes(shape = shapeCoding), colour = colourCoding, size = sizeCoding, alpha = alphaCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) == 1 & length(sizeCoding) > 1 & length(alphaCoding) == 1) {
  geom_point(aes(size = sizeCoding), colour = colourCoding, shape = shapeCoding, alpha = alphaCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) == 1 & length(sizeCoding) == 1 & length(alphaCoding) > 1) {
  geom_point(aes(alpha = alphaCoding), colour = colourCoding, shape = shapeCoding, size = sizeCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) > 1 & length(sizeCoding) > 1 & length(alphaCoding) == 1) {
  geom_point(aes(shape = shapeCoding, size = sizeCoding), colour = colourCoding, alpha = alphaCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) > 1 & length(sizeCoding) == 1 & length(alphaCoding) > 1) {
  geom_point(aes(shape = shapeCoding, alpha = alphaCoding), colour = colourCoding, size = sizeCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) == 1 & length(sizeCoding) > 1 & length(alphaCoding) > 1) {
  geom_point(aes(size = sizeCoding, alpha = alphaCoding), colour = colourCoding, shape = shapeCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) > 1 & length(sizeCoding) > 1 & length(alphaCoding) > 1) {
  geom_point(aes(shape = shapeCoding, size = sizeCoding, alpha = alphaCoding), colour = colourCoding)
}
else if (length(colourCoding) == 1 & length(shapeCoding) == 1 & length(sizeCoding) == 1 & length(alphaCoding) == 1) {
  geom_point(aes(),colour = colourCoding, shape = ShapeCoding, size = sizeCoding, alpha = alphaCoding)
}

title <- list()
for (i in 1:thresh) {
  title[[i]] <- paste0('PC', i, ' (', round(model$data$pcSum$`Proportion of Variance`[i], 1), '%)')
}
title<-unlist(title)

pcaGridPlot<-GGally::ggpairs(data = model$data$pcdf[,1:thresh],
                             columnLabels = c(title),
                             title = "gridTitle",
                             diag="blank",
                             upper="blank",
                             #upper=list(continuous = my_fn1),
                             lower=list(continuous = shellFunction),
                             #legend = grab_legend(test),
                             progress = F,
                             switch = "both") +
                              gp +
                              #stat_ellipse(aes(group=interaction(output$CO, color=output$CO), color=output$CO))+
                              theme_bw() +
                              theme(strip.background = element_rect(fill = "white"),
                              axis.text.x = (element_text(size=rel(0.7), angle=0)),
                              axis.text.y = (element_text(size=rel(0.7), angle=0)),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_rect(fill = NA,colour = "grey35"))


return(pcaGridPlot)

}
