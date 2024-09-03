split_corrplot2 <- function(corList, options = list(lable.x = TRUE, lable.y = TRUE, 
                                                   Title = "It can be a long title", 
                                                   Title_fontsize = 12, 
                                                   label_fontsize = 8, 
                                                   plot_type = "full")) {
  if (length(corList) != 2) {
    stop("corList must contain 2 correlation matrices as a list")
  }
  
  if (!options$plot_type %in% c("full", "upper", "lower")) {
    stop("plot_type must be one of 'full', 'upper', or 'lower'")
  }
  
  rescale <- function(x, from = c(-1, 1), to = c(0, 1)) {
    (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
  }
  
  c1 <- rescale(corList[[1]])
  c2 <- rescale(corList[[2]])
  
  col_names <- colnames(c1)
  row_names <- rownames(c1)
  rows <- nrow(c1)
  cols <- ncol(c1)
  
  half_circle <- function(x_center, y_center, r, side = "left", n = 100) {
    angles <- seq((1/2)*pi, (3/2)*pi, length.out = n)
    if (side == "right") angles <- angles + pi
    list(x = x_center + r * cos(angles), y = y_center + r * sin(angles))
  }
  
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 3, 
                                             widths = unit.c(unit(0.1, "npc"), unit(0.7, "npc"), unit(0.2, "npc")),
                                             heights = unit.c(unit(0.1, "npc"), unit(0.1, "npc"), unit(0.7, "npc"), unit(0.1, "npc")))))
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  grid.text(options$Title, y = 0.75, gp = gpar(fontsize = options$Title_fontsize, fontface = "bold"))
  popViewport()
  
  plot_matrix <- function(c1,c2, x_offset, y_offset, plot_type) {
    for (i in 1:rows) {
      for (j in 1:cols) {
        if (options$plot_type == "full" || 
            (options$plot_type == "upper" && j >= i) || 
            (options$plot_type == "lower" && j <= i)) {
          x <- (j - 0.5) / cols + x_offset
          y <- (rows - i + 0.5) / rows + y_offset
          fill1 <- rgb(colorRamp(c("blue", "white", "red"))(c1[i, j]) / 255)
          fill2 <- rgb(colorRamp(c("blue", "white", "red"))(c2[i, j]) / 255)
          
          grid.rect(x = x, y = y, width = 1 / cols, height = 1 / rows, gp = gpar(col = "black", fill = NA))
          
            hc_left <- half_circle(x + 0.01 / cols, y, 0.45 / cols, "left")
            grid.polygon(x = hc_left$x, y = hc_left$y, gp = gpar(fill = fill1, col = "black"))
            hc_right <- half_circle(x + 0.01 / cols, y, 0.45 / cols, "right")
            grid.polygon(x = hc_right$x, y = hc_right$y, gp = gpar(fill = fill2, col = "black"))
          
        }
      }
    }
  }
  
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 2))
  plot_matrix(c1,c2, x_offset = 0, y_offset = 0, plot_type = options$plot_type)
  popViewport()
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
  if (options$lable.y) {
    for (i in 1:rows) {
      y <- (rows - i + 0.5) / rows
      grid.text(row_names[i], x = -0.01, y = y, just = "right", gp = gpar(fontsize = options$label_fontsize))
    }
  }
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
  if (options$lable.x) {
    for (j in 1:cols) {
      x <- (j - 0.5) / cols
      grid.text(col_names[j], x = x, y = 1.08, just = "center", rot = 90, gp = gpar(fontsize = options$label_fontsize))
    }
  }
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
  color_grad <- colorRampPalette(c("blue", "white", "red"))(100)
  legend_height <- 0.8
  legend_width <- 0.1
  x_start <- 0.4
  y_start <- 0.1
  
  for (i in 1:length(color_grad)) {
    grid.rect(x = x_start, y = y_start - (1 - i) * legend_height / length(color_grad), 
              width = legend_width, height = legend_height / length(color_grad),
              gp = gpar(fill = color_grad[i], col = NA))
  }
  
  ticks <- seq(-1, 1, length.out = 11)
  tick_labels <- formatC(ticks, format = "f", digits = 1)
  
  for (i in seq_along(ticks)) {
    grid.lines(x = c(x_start + legend_width, x_start + legend_width + 0.02),
               y = c(y_start - (1 - i) * legend_height / length(ticks),
                     y_start - (1 - i) * legend_height / length(ticks)),
               gp = gpar(col = "black"))
    grid.text(tick_labels[i], x = x_start + legend_width + 0.05, 
              y = y_start - (1 - i) * legend_height / length(ticks), 
              just = "left", gp = gpar(fontsize = 6))
  }
  
  popViewport()
  popViewport()
}


split_corrplot2(corList = corList)


split_corrplot2(corList = corList,options = list(label.x = TRUE,label.y = TRUE,Title = "Test",Title_fontsize = 12,
                                                 label_fontsize = 8, plot_type = "full"))
