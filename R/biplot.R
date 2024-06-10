#still requires a legend and refine how the zoom input works, put limit on number of features back in.

biplot <- function(model, zoom = 1, PCi = 1, PCj = 2, optns = list()){

  if(zoom > 1){
    stop("Zoom must be between 0 and 1, set as the desired maximum of the loadings axis")
  }
  if(zoom != 1){
    zoom <- (1/zoom)
  }

  if("plotTitle" %in% names(optns)){
    plotTitle <- optns$plotTitle
  }else{optns[["plotTitle"]] <- "Biplot"
  plotTitle <- "Biplot"}

  if("theme" %in% names(optns)){
    theme <- optns$theme
  }else{theme = theme(legend.position = "bottom")}

  #PCA
  if(is(model)[1] == "list"){
    scores <- model[["data"]][["scores"]]
    loadings <- model[["data"]][["loadings"]]

    optns[["PCi"]] <- PCi
    optns[["PCj"]] <- PCj
  }

  #OPLS
  if(is(model)[1] == "opls"){
    scores <- model@scoreMN
    loadings <- model@loadingMN

   if(grepl("O", model@typeC) == TRUE){
     scores <- as.data.frame(cbind(model@scoreMN, model@orthoScoreMN), check.names = F)
     loadings <- as.data.frame(cbind(model@loadingMN, model@orthoLoadingMN), check.names = F)
     PCi = 1
     PCj = 2
   }
    optns[["PCi"]] <- PCi
    optns[["PCj"]] <- PCj
  }

  #if the number of loadings is too high, stop
  if(nrow(loadings) > 50){
    stop("Max of 50 variables allowed for a biplot")
  }

  ps <-
  plotScores(model = model,
             optns = optns)

  if(is(model)[1] == "opls"){
    ps <- ps@suppLs[["ScoresPlot"]]
  }

  # Plots
  scalef <- max(abs(scores)) + 0.1



  # scalef <- scale
  # Get arrow end point locations (loadings*scaling for correct scale)
  l.x <- loadings[,PCi]*scalef*zoom
  l.y <- loadings[,PCj]*scalef*zoom

  # Get label positions (%15 further than end of arrows)
  l.posx <- l.x*1.15
  l.posy <- l.y*1.15

  # Get labels for plot (variable names)
  l.labels <- row.names(loadings)
  lim <- 1.1

  secondary_limits <- c(-lim, lim) * scalef



  #biplot
  bp <- ps +
    geom_segment(data = loadings,
                 aes(x = 0,
                     y = 0,
                     xend = l.x,
                     yend = l.y),
                 color = "black",
                 arrow = arrow(length = unit(0.2, "cm"),
                               type = "closed")) +
    geom_text_repel(data = loadings,
                    aes(x = l.posx,
                        y = l.posy,
                        label = l.labels),
                    color = "black",
                    size = 3,
                    hjust = 0) +
    scale_x_continuous(sec.axis = sec_axis(transform = ~ . / (scalef*zoom),
                                           breaks = round(c(-1, -0.5, 0, 0.5, 1)/zoom, digits = 2),
                                           name = paste0(colnames(loadings)[PCi]," Loadings")),
                       limits = secondary_limits,
                       expand = c(0, 0)) +
    scale_y_continuous(sec.axis = sec_axis(transform = ~ . / (scalef*zoom),
                                           breaks = round(c(-1, -0.5, 0, 0.5, 1)/zoom, digits = 2),
                                           name = paste0(colnames(loadings)[PCj]," Loadings")),
                       limits = secondary_limits,
                       expand = c(0, 0)) +
    theme +
    labs(title = plotTitle)

  print(bp)

}
