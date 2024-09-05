#' Plot split correlation plot
#'
#' 
#'
#' @param corList A list of 2 correlation matrices. The first list will be presented on the left hand side of the circle.
#' @param type A type of plotting format, the choice is full, lower and upper only
#' @param optns An empty list for additional options:
#'  \itemize{
#'    \item{plotTitle} {A character for the title of the plot. default is "Split Correlation Plot"}
#'    \item{sizeTitle} {A numeric for the font size of the title. default is 12}
#'    \item{sizeLabel} {A numeric for the font size of the row/column label default is 8}
#'    \item{showLabelX} {A logical (TRUE/FALSE) to display the column label or not. default is TRUE}
#'    \item{showLabelY} {A logical (TRUE/FALSE) to display the raw label or not. default is TRUE}
#'    \item{showLegend} {A logical (TRUE/FALSE) to display the legend or not. default is TRUE}
#'    \item{c1Name} {A character to describe the first correlation matrix (= left circle). This name will go on the legend. default is "correlation 1"}
#'    \item{c2Name} {A character to describe the second correlation matrix (= right circle). This name will go on the legend. default is "correlation 2"}
#'    \item{showScaleLegend} {A logical (TRUE/FALSE) to display the color scale legend or not. default is TRUE}
#'    }
#'    
#' @return plot of the two split correlation matrix
#' @import scales
#' @example
#' corList<-list()
#' corList[[1]]<-cor(tdf[idx_healthy,])
#' corList[[2]]<-cor(tdf[idx_nonhealthy,])
#' plotSplitCor(corList = corList,type = "lower")
#' plotSplitCor(corList = corList,type = "upper",optns = list(plotTitle = "Healthy vs non-Healthy",
#'                                                               c1Name = "Healthy",
#'                                                               c2Name = "NonHealthy"))
#' 
#'  @export


plotSplitCor <- function(corList,
                           type = c("full", "lower", "upper"),
                           optns = list()) {
  if (length(corList) != 2) {
    stop("corList must contain 2 correlation matrices as a list")
  }
  if (dim(corList[[1]])[1] != dim(corList[[2]])[1] |
      dim(corList[[1]])[2] != dim(corList[[2]])[2]) {
    stop("2 correlation matrices must have the same dimensions")
  }
  if (!type %in% c("full", "lower", "upper")) {
    stop("Please select a type of the plot")
  }
  if (!("plotTitle" %in% names(optns))) {
    optns$plotTitle <- "Split Correlation Plot"
  }
  if (!("sizeTitle" %in% names(optns))) {
    optns$sizeTitle <- 12
  }
  if (!("sizeLabel" %in% names(optns))) {
    optns$sizeLabel <- 8
  }
  if (!("sizeLegend" %in% names(optns))) {
    optns$sizeLabel <- 8
  }
  if (!("showLabelX" %in% names(optns))) {
    optns$showLabelX <- TRUE # to show column names
  }
  if (!("showLabelY" %in% names(optns))) {
    optns$showLabelY <- TRUE # to show row names
  }
  if (!("showLegend" %in% names(optns))) {
    optns$showLegend <- TRUE # to show the legend on the top right
  }
  if (!("c1Name" %in% names(optns)) && optns$showLegend == TRUE) {
    optns$c1Name <- "correlation 1" # correlation data information (left)
  }
  if (!("c2Name" %in% names(optns)) && optns$showLegend == TRUE) {
    optns$c2Name <- "correlation 2" # correlation data information (right)
  }
  if (!("showScaleLegend" %in% names(optns))) {
    optns$showScaleLegend <- TRUE # to show the color scale
  }
  
  
  rescale <- function(x,
                      from = c(-1, 1),
                      to = c(0, 1)) {
    (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
  }
  
  c1 <- rescale(corList[[1]])
  c2 <- rescale(corList[[2]])
  
  col_names <- colnames(c1)
  row_names <- rownames(c1)
  rows <- nrow(c1)
  cols <- ncol(c1)
  
  half_circle <- function(x_center,
                          y_center,
                          r,
                          side = "left",
                          n = 100) {
    angles <- seq((1 / 2) * pi, (3 / 2) * pi, length.out = n)
    if (side == "right")
      angles <- angles + pi
    list(x = x_center + r * cos(angles), y = y_center + r * sin(angles))
  }
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    nrow = 4,
    ncol = 3,
    widths = unit.c(unit(0.1, "npc"), unit(0.7, "npc"), unit(0.2, "npc")),
    heights = unit.c(
      unit(0.1, "npc"),
      unit(0.1, "npc"),
      unit(0.7, "npc"),
      unit(0.1, "npc")
    )
  )))
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  grid.text(
    optns$plotTitle,
    y = 0.75,
    gp = gpar(fontsize = optns$sizeTitle, fontface = "bold")
  )
  popViewport()
  
  plot_matrix <- function(c1, c2, x_offset, y_offset, plot_type) {
    for (i in 1:rows) {
      for (j in 1:cols) {
        if (plot_type == "full" ||
            (plot_type == "upper" && j >= i) ||
            (plot_type == "lower" && j <= i)) {
          x <- (j - 0.5) / cols + x_offset
          y <- (rows - i + 0.5) / rows + y_offset
          
          grid.rect(
            x = x,
            y = y,
            width = 1 / cols,
            height = 1 / rows,
            gp = gpar(col = "black", fill = NA)
          )
          
          if(!is.na(c1[i,j])){
            fill1 <- rgb(colorRamp(c("blue", "white", "red"))(c1[i, j]) / 255)
            hc_left <- half_circle(x + 0.01 / cols, y, 0.45 / cols, "left")
            grid.polygon(
              x = hc_left$x,
              y = hc_left$y,
              gp = gpar(fill = fill1, col = "black")
            )
          }
          if(!is.na(c2[i,j])){
            fill2 <- rgb(colorRamp(c("blue", "white", "red"))(c2[i, j]) / 255)
            hc_right <- half_circle(x + 0.01 / cols, y, 0.45 / cols, "right")
            grid.polygon(
              x = hc_right$x,
              y = hc_right$y,
              gp = gpar(fill = fill2, col = "black")
            )
          }
        
        }
      }
    }
  }
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 3))
  if (optns$showLegend) {
    grid.rect(
      x = 0.5,
      y = 0.5,
      width = 0.5,
      height = 1,
      gp = gpar(col = "black", fill = NA)
    )
    hc_left <- half_circle(0.5, 0.5, 0.2, "left")
    grid.polygon(x = hc_left$x,
                 y = hc_left$y,
                 gp = gpar(fill = NA, col = "black"))
    grid.text(
      "1",
      x = 0.4,
      y = 0.5,
      gp = gpar(fontsize = optns$sizeLabel, fontface = "bold")
    )
    hc_right <- half_circle(0.5, 0.5, 0.2, "right")
    grid.polygon(x = hc_right$x,
                 y = hc_right$y,
                 gp = gpar(fill = NA, col = "black"))
    grid.text(
      "2",
      x = 0.6,
      y = 0.5,
      gp = gpar(fontsize = optns$sizeLabel, fontface = "bold")
    )
    popViewport()
    
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 3))
    grid.text(
      paste0("1: ", optns$c1Name),
      x = 0.4,
      y = 0.6,
      gp = gpar(fontsize = optns$sizeLabel, fontface = "bold")
    )
    grid.text(
      paste0("2: ", optns$c2Name),
      x = 0.4,
      y = 0.4,
      gp = gpar(fontsize = optns$sizeLabel, fontface = "bold")
    )
    
  }
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
  plot_matrix(c1,
              c2,
              x_offset = 0,
              y_offset = 0,
              plot_type = type)
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
  
  if (type == "full" | type == "lower" && optns$showLabelY) {
    for (i in 1:rows) {
      y1 <- (rows - i + 0.5) / rows
      grid.text(
        row_names[i],
        x = 0.8,
        y = y1,
        just = "right",
        gp = gpar(fontsize = optns$sizeLabel)
      )
    }
  }
  if (type == "upper" && optns$showLabelY) {
    for (i in 1:rows) {
      y1 <- (rows - i + 0.5) / rows
      grid.text(
        row_names[i],
        x = 8.8,
        y = y1,
        just = "right",
        gp = gpar(fontsize = optns$sizeLabel)
      )
    }
  }
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
  
  if (type == "full" | type == "upper" && optns$showLabelX) {
    for (j in 1:cols) {
      x1 <- (j - 0.5) / cols  # Center the label above each box
      grid.text(
        col_names[j],
        x = x1,
        y = 0.5,
        just = "center",
        rot = 90,
        gp = gpar(fontsize = optns$sizeLabel)
      )
    }
  }
  
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2))
  if (type == "lower" && optns$showLabelX) {
    for (j in 1:cols) {
      x1 <- (j - 0.5) / cols  # Center the label below each box
      grid.text(
        col_names[j],
        x = x1,
        y = 0.6,
        just = "center",
        rot = -90,
        gp = gpar(fontsize = optns$sizeLabel)
      )
    }
  }
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
  color_grad <- colorRampPalette(c("blue", "white", "red"))(100)
  legend_height <- 0.8
  legend_width <- 0.1
  x_start <- 0.5
  y_start <- 0.1
  
  for (i in 1:length(color_grad)) {
    grid.rect(
      x = x_start,
      y = y_start - (1 - i) * legend_height / length(color_grad),
      width = legend_width,
      height = legend_height / length(color_grad),
      gp = gpar(fill = color_grad[i], col = NA)
    )
  }
  
  ticks <- seq(-1, 1, length.out = 11)
  tick_labels <- formatC(ticks, format = "f", digits = 1)
  
  for (i in seq_along(ticks)) {
    grid.lines(
      x = c(x_start + legend_width, x_start + legend_width + 0.02),
      y = c(
        y_start - (1 - i) * legend_height / length(ticks),
        y_start - (1 - i) * legend_height / length(ticks)
      ),
      gp = gpar(col = "black")
    )
    grid.text(
      tick_labels[i],
      x = x_start + legend_width + 0.05,
      y = y_start - (1 - i) * legend_height / length(ticks),
      just = "left",
      gp = gpar(fontsize = 6)
    )
  }
  
  popViewport()
  
}



