#' specOverlay
#'
#' Plot multiple spectra plot that can be separated by groups, colour and line types (not interactive)
#'
#' @param X is a spectra matrix (colnames(X) <- ppm, and rownames(X)<-Annotation$sampleID is recommended to do so before plotting)
#' @param ppm must be numeric and same length as the number of column in X
#' @param roi region of interest if not defined default is set as c(-0.01 , 0.01)
#' @param optns An empty list the first list represent facet (require), the second list as color (optional) and the third as linshape (optional).
#' @param title A character for the title of the plot.
#' @param size A numeric of the size desired. Default size is 0.5.
#' @param alpha A numeric of the alpha desired. Default size is 0.7.
#' @return matspec non-interactive version separated by assigned column factor (i.e., Groups) and within the each plot that can have different color or linetype to differentiate further by sub-categories (i.e., timepoints, class)
#' @importFrom reshape2 melt
#' @importFrom scales breaks_pretty
#' @export


#@examples
# this cannot be we don't publish path to our data !nmr<-local(get(load(etc.
# ppm<-as.numeric(nmr@varName)
# X<-nmr@.Data
# an<-nmr@obsDescr[[1]]
# Y<-factor(c(rep("A",28),rep("B",28)))
# specOverlay(X,ppm,roi = c(3.0,4.5),alp = 0.3,optns = list(Y, factor(an$sampleType)))

specOverlay <- function(X, ppm = NULL, roi = c(-0.01, 0.01), alp = 0.7, size = 0.5, title = "", optns=list(),...) {
  
  if (is.null(ppm)) {
    ppm <- as.numeric(colnames(X))
  } else {
    if (length(ppm)!=ncol(X))
      stop("Non-matching dimensions X matrix and ppm vector or missing values in ppm.")
  }
  
  if (is.null(names(optns))) {
    cat("No facet, colour and linetype names given. See optn argument in ?specOverlay\n")
    names(optn) <- paste("optns", seq_len(length(optns)), sep = "")
  }
  
  le.arg <- paste(length(optns))
  if ("" %in% names(optns)) {
    idx <- which(names(optns) == "")
    names(optns)[idx] <- paste("optns", idx, sep = "")
  }
  names(optns) <- gsub(" ", ".", names(optns))
  idx <- which(ppm >= roi[1] & ppm <= roi[2])
  specs <- X[, idx]
  colnames(specs) <- paste("Idx", idx, sep = "_")
  # create dataframe for ggplot function
  df <- data.frame(do.call(cbind.data.frame, optns), ID = seq_len(nrow(specs)), alp,
                   specs)
  colnames(df)[seq_len(le.arg)] <- names(optns)
  df <- melt(df, id.vars = c("alp", "ID", names(optns)))
  df$variable <- ppm[as.numeric(gsub("Idx_", "", df$variable))]
  # initiate generic ggplot object
  g <- ggplot() + scale_x_reverse(breaks = seq(roi[1], roi[2], by = abs(diff(roi))/20),
                                  name = expression(delta ~ {
                                  }^1 * H ~ "(ppm)")) + scale_y_continuous(breaks = breaks_pretty(), name = "Intensity") +
    ggtitle(title) + facet_grid(as.formula(paste(names(optns)[1], "~ ."))) + theme_bw() +
    theme(axis.text = element_text(colour = "black"), axis.text.x = element_text(angle = 45,
                                                                                 hjust = 1))
  # add colour and line type
  switch(le.arg, `1` = {
    g <- g + geom_line(data = df, aes_string(x = "variable", y = "value", group = "ID"),
                       colour = "black", alpha = alp, size = size)
  }, `2` = {
    g <- g + geom_line(data = df, aes_string(x = "variable", y = "value", group = "ID",
                                             colour = names(optns)[2]), alpha = alp, size = size)
    # add multi-colour gradient if colour vector is not factor/char
    col.cat <- is.factor(optns[[2]]) | is.character(optns[[2]]) | is.logical(optns[[2]])
    if (!col.cat) {
      g <- g + scale_colour_gradientn(colors = matlab.like2(length(optns[[2]])))
    }
  }, `3` = {
    optns[[3]] <- factor(optns[[3]])
    g <- g + geom_line(data = df, aes_string(x = "variable", y = "value", group = "ID",
                                             colour = names(optns)[2], linetype = names(optns)[3]), alpha = alp, size = size)
    # add multi-colour gradient if colour vector is not factor/char
    col.cat <- is.factor(optns[[2]]) | is.character(optns[[2]]) | is.logical(optns[[2]])
    if (!col.cat) {
      g <- g + scale_colour_gradientn(colors = matlab.like2(length(optns[[2]])))
    }
  })
  return(g)
}

