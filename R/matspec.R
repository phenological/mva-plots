#' matspec
#'
#' Plot multiple spectra plot that can be interactive (default)
#'
#' @param X is a spectra matrix (colnames(X) <- ppm, and
#' rownames(X) <- Annotation$sampleID is recommended to do so before plotting)
#' @param ppm must be numeric and same length as the number of column in X
#' @param roi region of interest if not defined default is set as c(0.0 , 9.5)
#' @param interactive default is True which allow you to zoom in and select
#' which spectra to show
#' @param ... extra arguments.
#' @return Interactive/non-interactive spectra plot
#' @importFrom plotly plot_ly add_lines layout
#' @importFrom reshape2 melt
#'
#' @examples
#' # this cannot be we don't publish path to our data
#' # !nmr<-local(get(load("~/OneDrive - Murdoch University/datasets/Colchicin/
#' # DataElements/hims_colchicin_PLA_HIMr02_PROF.PLASMA.CPMG.daE")))
#' #ppm<-as.numeric(nmr@varName)
#' #X<-nmr@.Data
#' #matspec(X[1:3,],ppm,roi = c(3.0,4.5),interactive = T)
#' @export

matspec<-function (X, ppm, roi = c(0.5, 9.5), interactive = TRUE, ...)
{
####ppm####
#if ppm is not provided, create it using the column names of X
  if (is.null(ppm)) {
    ppm <- as.numeric(colnames(X))
  }else {
    if (length(ppm)!=ncol(X))
      stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.")
  }

####roi####
#if a roi is provided, use only values in ppm that fall within that range, otherwise set roi to 0.5 to 9.5 and do the same
  if (!missing(roi)){
    fi <- ppm >= roi[1] & ppm <= roi[2]
  }else{
    fi <- ppm >= 0 & ppm <= 9.5
  }

####interactive plot####
#for an interactive plot, use plot_ly
  if(interactive){
    df <- melt(X[, fi])
    x <- list(title = "ppm", autorange = "reversed")
    y <- list(title = "Intensity")
     # cols <- suppressWarnings(colorRampPalette(RColorBrewer::brewer.pal(10,
     #                                                                    "Set2"))(nrow(X)))

    cols <- suppressWarnings(colorRampPalette(colors = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"))(nrow(X)))
    df$col <- rep(cols, length(which(fi==TRUE)))

    #####processed spectra#####
    #if the spectra is processed the column names in the created df will be different, this will rectify that

    #rename variable as Var2 if variable is in df
    if("variable" %in% names(df)){
      df$variable <- as.numeric(df$variable)
      names(df)[names(df) == "variable"] <- "Var2"
    }

    #create Var1 if it doesn't already exist
    if(!("Var1" %in% names(df))){
      unique_values <- unique(df$col)

      # Create the 'Var1' column based on the pattern
      df$Var1 <- rep(1:length(unique_values), length.out = nrow(df))
    }

    #plotting
    p <- suppressWarnings(plotly::plot_ly(data = df,
                                          x = ~Var2,
                                          y = ~value,
                                          color = ~I(col),
                                          name = ~Var1,
                                          hovertemplate = "%{x} ppm<extra></extra>"))

    p <- suppressWarnings(layout(p = p ,
                                 xaxis = x,
                                 yaxis = y))
    p <- suppressWarnings(add_lines(p = p))

    return(p)
  } else {
    ####non-interactive plot####
    # We can re-sample to have 1 data point by pixel and get the result faster.
    figSizeInPx <- dev.size(units = "px")# width, height
    if(all(is.na(figSizeInPx))) {
      figSizeInPx <- c(600, 400)
    }
    # We can suppose that all the pixels are used for data points. That is not completely true, but it is a good guess
    pointsPerPixel <- ceiling(length(fi) / figSizeInPx[[1]])
    # Simple re-sampling function.
    for (i in 1:length(fi)) {
      if ((i %% pointsPerPixel) != 0) {
        fi[[i]] <- FALSE
      }
    }

    matplot(ppm[fi],
            t(X[, fi]),
            type = "l",
            xlim = rev(range(ppm[fi])),
            xlab = "ppm",
            ylab = "Intensity",
            ...)
  }
}











