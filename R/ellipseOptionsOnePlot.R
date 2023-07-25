##plots

#Permutation

#loadings

#scores

#AUC

#outliers?

ellipseOptions2 <- function(df, PCi, PCj, plot, optns){
  if("ellipse" %in% names(optns)){
    if(optns$ellipse == "color"){
      #colour
      ellipse <- stat_ellipse(aes(color = df$y1))
      gl <- labs()
      idx <-list()
    }

    #option for CI % level
    if("ci" %in% names(optns)) {
      ci <- optns$ci
    } else{ci <- 0.95}

    ##########Hotelling's##############
    if(optns$ellipse == "hotellings"){

      n <- nrow(df)

      hotFisN <- (n - 1) * 2 * (n^2 - 1) / (n^2 * (n - 2)) * qf(ci, 2, n - 2)

      ellipse <- gg_circle(rx = sqrt(var(df[,PCi]) * hotFisN),
                           ry = sqrt(var(df[,PCj]) * hotFisN),
                           xc = 0,
                           yc = 0)
      gl <-  labs(caption = bquote(paste("Dashed line: Hotelling's T"^{2} ~"ellipse ("* alpha *" ~ ", .(ci), ")")))

      ##for outliers
      rx <- sqrt(var(df[, PCi]) * hotFisN)
      ry <- sqrt(var(df[, PCj]) * hotFisN)

      insideOut <- list((df[,PCi]^2)/(rx^2) + (df[,PCj]^2)/(ry^2))
      idx <- which(insideOut[[1]] > 1)
    }
    ##########T##############
    if(optns$ellipse == "T"){
      #norm or t
      ellipse <- stat_ellipse(type = "t",
                              geom = "polygon",
                              fill = "gray",
                              level = ci,
                              alpha = .5,
                              linetype = 2)
      gl <-  labs(caption = bquote(paste("Dashed line: t distribution" ~"ellipse ("* alpha *" ~ ", .(ci), ")")))
    }
    ###############normal#######
    if(optns$ellipse == "norm"){
      ellipse <- stat_ellipse(type = "norm",
                              geom = "polygon",
                              fill = "gray",
                              level = ci,
                              alpha = .5,
                              linetype = 2)
      gl <-  labs(caption = bquote(paste("Dashed line: normal distribution" ~"ellipse ("* alpha *" ~ ", .(ci), ")")))

    }
  }else{ellipse <- labs(caption = expression("no ellipse"))
  gl <- labs()}


  plot <- plot + ellipse + gl

  if(optns$ellipse == "T" | optns$ellipse =="norm"){

    ##for outliers
    build <- ggplot_build(plot)$data
    points <- build[[3]]
    ell <- build[[4]]

    ## Find which points are inside the ellipse
    rx <- (max(ell$x)-min(ell$x))/2
    ry <- (max(ell$y)-min(ell$y))/2
    h <- max(ell$x) - rx
    k <- max(ell$y) - ry

    insideOut <- list((((df[,PCi]-h)^2)/(rx^2)) + (((df[,PCj]-k)^2)/(ry^2)))
    idx <- which(insideOut[[1]] > 1)

  }
  output <- list(plot = plot,
                 outliers = idx)

return(output)
}









