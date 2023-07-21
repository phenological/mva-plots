##plots

#Permutation

#loadings

#scores

#AUC

#outliers?

# ellipseOptionsOnePlot <- function(model , Plot , i, j, optns = optns){
#
#   #option for CI % level
#   if("ci" %in% names(optns)) {
#     ci <- optns$ci
#   } else{ci <- 0.95}
#
#   #option for outlier labels on plot
#   if("outlierLabels" %in% names(optns)) {
#     model$data$pcdf$outlierID <- optns$outlierLabels
#   }
#
#   #ellipse options
#
#   if("ellipse" %in% names(optns)){
#
#
#
#     X <- as.matrix(model$data$pcdf[,c(i:j)])
#
#     # Sample size
#     n <- nrow(X)
#
#     hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)
#
#     outliers <- list()
#
#           #set up names for outlier list, eg PC1vPC2
#           placeHolder <- paste0("PC", i, "vPC", j)
#
#           temp <- Plot
#
#           #Hotelling's T2
#
#           if(optns$ellipse == "hotellings"){
#
#             ##for the plot
#             temp <-
#               temp + gg_circle(
#                 rx = sqrt(var(model$data$pcdf[i]) * hotFisN),
#                 ry = sqrt(var(model$data$pcdf[j]) * hotFisN),
#                 xc = 0,
#                 yc = 0
#               )
#             ##for outliers
#             rx <- sqrt(var(model$data$pcdf[i]) * hotFisN)
#             ry <- sqrt(var(model$data$pcdf[j]) * hotFisN)
#
#             insideOut <- list((model$data$pcdf[i]^2)/(rx^2) + (model$data$pcdf[j]^2)/(ry^2))
#             idx <- which(insideOut[[1]] > 1)
#
#             outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]
#
#             new_list <- setNames(list(outlierIDX), placeHolder)
#             outliers <- append(outliers, new_list)
#
#             ##label outliers
#             if(optns$ellipse == "hotellings" & "outlierLabels" %in% names(optns)){
#               temp <-
#                 temp + geom_text(
#                   data = model$data$pcdf[idx, ],
#                   aes(label = outlierID),
#                   size = 2,
#                   hjust = 0,
#                   vjust = 0
#                 )
#             }
#           }
#
#           #stat_ellipse with type t method
#
#           if(optns$ellipse == "T"){
#
#             ##for the plot
#             temp <-
#               temp + stat_ellipse(
#                 type = "t",
#                 geom = "polygon",
#                 fill = "gray",
#                 level = ci,
#                 alpha = .5,
#                 linetype = 2
#               )
#
#             ##for the outliers
#             build <- ggplot_build(temp)$data
#             points <- build[[3]]
#             ell <- build[[4]]
#
#             ## Find which points are inside the ellipse
#             rx <- (max(ell$x)-min(ell$x))/2
#             ry <- (max(ell$y)-min(ell$y))/2
#             h <- max(ell$x) - rx
#             k <- max(ell$y) - ry
#
#             insideOut <- list((((model$data$pcdf[i]-h)^2)/(rx^2)) + (((model$data$pcdf[j]-k)^2)/(ry^2)))
#             idx <- which(insideOut[[1]] > 1)
#             outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]
#
#             new_list <- setNames(list(outlierIDX), placeHolder)
#             outliers <- append(outliers, new_list)
#
#             ##label outliers
#             if(optns$ellipse == "T" & "outlierLabels" %in% names(optns) ){
#               temp <-
#                 temp + geom_text(
#                   data = model$data$pcdf[idx, ],
#                   aes(label = outlierID),
#                   size = 2,
#                   hjust = 0,
#                   vjust = 0
#                 )
#             }
#
#           }
#
#           #stat_elipse with norm method
#           if(optns$ellipse == "normal"){
#             ##for plot
#             temp <-
#               temp + stat_ellipse(
#                 type = "norm",
#                 geom = "polygon",
#                 fill = "gray",
#                 level = ci,
#                 alpha = .5,
#                 linetype = 2
#               )
#
#             ##for the outliers
#             build <- ggplot_build(temp)$data
#             points <- build[[3]]
#             ell <- build[[4]]
#
#             ## Find which points are inside the ellipse
#             rx <- (max(ell$x)-min(ell$x))/2
#             ry <- (max(ell$y)-min(ell$y))/2
#             h <- max(ell$x) - rx
#             k <- max(ell$y) - ry
#
#             insideOut <- list((((model$data$pcdf[i]-h)^2)/(rx^2)) + (((model$data$pcdf[j]-k)^2)/(ry^2)))
#             idx <- which(insideOut[[1]] > 1)
#             outlierIDX <- model$data$pcdf[idx,-1:-ncol(model$data$scores)]
#
#             new_list <- setNames(list(outlierIDX), placeHolder)
#             outliers <- append(outliers, new_list)
#
#             ##label outliers
#             if(optns$ellipse == "normal" & "outlierLabels" %in% names(optns)){
#               temp <-
#                 temp + geom_text(
#                   data = model$data$pcdf[idx, ],
#                   aes(label = outlierID),
#                   size = 2,
#                   hjust = 0,
#                   vjust = 0
#                 )
#             }
#           }
#
#           #stat_ellipse for separation
#
#           if(optns$ellipse == "color"){
#             temp <- temp +
#               stat_ellipse(aes(color = model$data$pcdf$colorCoding))
#           }
#           onePlot <- temp
#
#       }
#
#   if("ellipse" %in% names(optns)){
#     outliers <- outliers
#   }else{outliers <- list()}
#
#   output <- list(onePlot = onePlot,
#                  outliers = outliers
#   )
#
#   return(output)
# }
#
# #stat_ellipse for separation
#
# if(optns$ellipse == "color"){
#   temp <- temp +
#     stat_ellipse(aes(color = model$data$pcdf$colorCoding))
# }
#
ellipseOptions2 <- function(df, plot, optns){
  if("ellipse" %in% names(optns)){
    if(optns$ellipse == "color"){
      #colour
      ellipse <- stat_ellipse(aes(color = y1))
    }
    if(optns$ellipse == "hotellings"){
      #hotellings
      ci<-0.95
      n <- nrow(df)

      hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)


      ##for outliers
      rx <- sqrt(var(df$p1) * hotFisN)
      ry <- sqrt(var(df[,2]) * hotFisN)

      insideOut <- list((df$p1^2)/(rx^2) + (df[,2]^2)/(ry^2))
      idx <- which(insideOut[[1]] > 1)

      outlierIDX <- df[idx,-1:-ncol(df)]

      # new_list <- setNames(list(outlierIDX), placeHolder)
      # outliers <- append(outliers, new_list)
      ellipse <- gg_circle(rx = sqrt(var(df[,1]) * hotFisN),
                           ry = sqrt(var(df[,2]) * hotFisN),
                           xc = 0,
                           yc = 0)
      gl <-  labs(caption = paste0("Dashed line: Hotelling's T"^2 ~ "ellipse (" * alpha * "=",ci,")"))

      #   geom_text(data = df[idx, ], aes(label = p1), size = 2, hjust = 0, vjust = 0)
    }
    # if(ellipse == "t"){
    #   #norm or t
    #   ellipse <- stat_ellipse(type = "t",
    #                           geom = "polygon",
    #                           fill = "gray",
    #                           level = ci,
    #                           alpha = .5,
    #                           linetype = 2) +
    #     labs(caption = expression("Dashed line: t-distribution" ~ "ellipse (" * alpha * "=0.95)"))
    # }
    #
    # if(ellipse == "norm"){
    #   ellipse <- stat_ellipse(type = "norm",
    #                           geom = "polygon",
    #                           fill = "gray",
    #                           level = ci,
    #                           alpha = .5,
    #                           linetype = 2) +
    #     labs(caption = expression("Dashed line: normal distribution" ~ "ellipse (" * alpha * "=0.95)"))
    # }
  }else{ellipse <- labs(caption = expression("no ellipse"))
  gl <- labs()}

  plot <- plot + ellipse + gl
return(plot)
}









