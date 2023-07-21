##plots

#Permutation

#loadings

#scores

#AUC

#outliers?

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









