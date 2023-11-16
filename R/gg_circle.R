# gg_circle <- function(rx, ry, xc, yc, color = "black", fill = NA, linetype = "dashed",...) {
#   x <- xc + rx * cos(seq(0, pi, length.out = 100))
#   ymax <- yc + ry * sin(seq(0, pi, length.out = 100))
#   ymin <- yc + ry * sin(seq(0, -pi, length.out = 100))
#   annotate(
#     "ribbon",
#     x = x, ymin = ymin, ymax = ymax,
#     color = color, fill = fill, linetype = linetype, ...
#   )
# }


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

# #Hotelling's T2
#
# if(optns$ellipse == "hotellings"){
#
#   ##for the plot
#   temp <-
#     temp + gg_circle(
#       rx = sqrt(var(model$data$pcdf[i]) * hotFisN),
#       ry = sqrt(var(model$data$pcdf[j]) * hotFisN),
#       xc = 0,
#       yc = 0
#     )
#   ##for outliers
#   rx <- sqrt(var(model$data$pcdf[i]) * hotFisN)
#   ry <- sqrt(var(model$data$pcdf[j]) * hotFisN)
#
#   insideOut <- list((model$data$pcdf[i]^2)/(rx^2) + (model$data$pcdf[j]^2)/(ry^2))
#   idx <- which(insideOut[[1]] > 1)
#
#   outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]
#
#   new_list <- setNames(list(outlierIDX), placeHolder)
#   outliers <- append(outliers, new_list)
#
#   ##label outliers
#   if(optns$ellipse == "hotellings" & "outlierLabels" %in% names(optns)){
#     temp <-
#       temp + geom_text(
#         data = model$data$pcdf[idx, ],
#         aes(label = outlierID),
#         size = 2,
#         hjust = 0,
#         vjust = 0
#       )
#   }
#
#   ##########Hotelling's##############
#   if(optns$ellipse == "hotellings"){
#
#     n <- nrow(df)
#
#     hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)
#
#     ellipse <- gg_circle(rx = sqrt(var(df[,PCi]) * hotFisN),
#                          ry = sqrt(var(df[,PCj]) * hotFisN),
#                          xc = 0,
#                          yc = 0)
#     gl <-  labs(caption = bquote(paste("Dashed line: Hotelling's T"^{2} ~"ellipse ("* alpha *" ~ ", .(ci), ")")))
#
#     ##for outliers
#     rx <- sqrt(var(df[, PCi]) * hotFisN)
#     ry <- sqrt(var(df[, PCj]) * hotFisN)
#
#     insideOut <- list((df[,PCi]^2)/(rx^2) + (df[,PCj]^2)/(ry^2))
#     idx <- which(insideOut[[1]] > 1)
#   }
