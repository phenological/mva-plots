gg_circle <- function(rx, ry, xc, yc, color = "black", fill = NA, linetype = "dashed",...) {
  x <- xc + rx * cos(seq(0, pi, length.out = 100))
  ymax <- yc + ry * sin(seq(0, pi, length.out = 100))
  ymin <- yc + ry * sin(seq(0, -pi, length.out = 100))
  annotate(
    "ribbon",
    x = x, ymin = ymin, ymax = ymax,
    color = color, fill = fill, linetype = linetype, ...
  )
}


