#' matspec 
#'
#' Plot multiple spectra plot that can be interactive (default)
#'
#' @param X is a spectra matrix (colnames(X)<-ppm, and rownames(X)<-Annotation$sampleID is recommended to do so before plotting)
#' @param ppm must be numeric and same length as the number of column in X
#' @param roi region of interest if not defined default is set as c(0.0 , 9.5)
#' @param interactive default is True which allow you to zoom in and select which spectra to show
#' @return Interactive/non-interactive spectra plot
#' @importFrom dplyr
#' @importFrom ggplot2
#' @importFrom plotly
#' 
#' @examples 
#' nmr<-local(get(load("~/OneDrive - Murdoch University/datasets/Colchicin/DataElements/hims_colchicin_PLA_HIMr02_PROF.PLASMA.CPMG.daE")))
#' ppm<-as.numeric(nmr@varName)
#' X<-nmr@.Data
#' matspec(X[1:3,],ppm,roi = c(3.0,4.5),interactive = T)

matspec<-function (X, ppm, roi = c(0, 9.5), interactive = TRUE, ...) 
{
  if (is.null(ppm)) {
    ppm <- as.numeric(colnames(X))
  }else {
    if (length(ppm)!=ncol(X)) 
      stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.")
  }
  if (!missing(roi)){
    fi <- ppm >= roi[1] & ppm <= roi[2]
  }else{
    fi <- ppm >= 0 & ppm <= 9.5
  }
  if(interactive){
    df <- reshape2::melt(X[, fi])
    x <- list(title = "ppm", autorange = "reversed")
    y <- list(title = "Intensity")
    cols <- suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(10, 
                                                                       "Set2"))(nrow(X)))
    df$col <- rep(cols, length(which(fi==TRUE)))
    p <- suppressWarnings(plotly::plot_ly(data = df, x = ~Var2, y = ~value, 
                                  color = ~I(col), name = ~Var1, hovertemplate = "%{x} ppm<extra></extra>") %>% 
                            layout(xaxis = x, yaxis = y) %>%plotly::add_lines())
    return(p)
  }
  matplot(ppm[fi], t(X[, fi]), type = "l", xlim = rev(range(ppm[fi])), 
          xlab = "ppm", ylab = "Intensity",...)

}




















