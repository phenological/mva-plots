#' PlotLoadSpec
#'
#' Allow to plot loadings of PCA, PLS, OPLS,PLS-DA and OPLS-DA
#'
#' @param model are the output from : PCA(), prcomp(), oplsda() or ropls::opls()
#' @param PC is the number of components that you want to plot the loadings of
#' Default is 1 and only require to change when plotting PCA or PLS model
#' @param  roi is the region of interest (default is from 0.5 to 9.5 ppm)
#' @param type is the loading visualization types
#'         "Statistical reconstruction" calculate the covariance for the trace
#'         (y) and Pearson's correlation of the predictive (O)PLS scores with
#'         each ppm for the color.//
#'         "Backscaling" calculate the trace with predicted loadings multiplied
#'         by the standard deviation of X feature, color is weighted by the
#'         OPLS model.//
#'         for PCA it is always "Statistical reconstruction".
#'         but for PLS and OPLS "Backscaling" method is recommended.
#' @param X spectral data matrix used for the model.
#'        This is needed for PCA with prcomp package, or
#'        (O)PLS with type = "Statistical reconstruction".
#' @param ppm chemical shift scale. If NULL (default), it is read from the row names
#'        of the loading matrix. If the rows are unnamed, row indices are
#'        used instead with a warning.
#' @param Median only works for OPLS-DA model using mva.plot().
#'        Default is FALSE.
#'        When Median = True
#'        Median spectra of each groups will be added on the top of loading plot showing the same direction as score plot
#'
#' @param optns An empty list for additional options.
#' @return plot of the loadings
#' @examples
#' \dontrun{
#' data("NMR_1D_rat_urine_spectra")
#' #PCA
#' model <- PCA(data = X,
#'              rank. = 2)
#' #oplsda
#' model <- oplsda(X = Xsample,
#'                 Y = Ysample,
#'                 type = "OPLS")
#'
#' model_loadings_plot <- PlotLoadSpec(model = model,
#'                                     X = X,
#'                                     PC = 2)
#' }
#' @import scales
#' @import methods
#' @export

PlotLoadSpec<-function(model, PC = 1, roi = c(0.5,9.5), type = "Backscaled", X = NULL, ppm = NULL, Median = FALSE, optns = list()){
  #Global argument checks (model independent)
  if(!class(model)[1]%in%c("opls","list","prcomp")){
    stop("Check the Model class: must be prcomp,opls, or the results from PCA")
  }
  if(type != "Backscaled" & type !="Statistical reconstruction"){
    stop("Name for the visualization type must be 'Backscaled' or 'Statistical reconstruction'. ")
  }
  if(!is.numeric(roi) | length(roi) != 2){
    stop("Invalid roi: must be numeric vector of length 2")
  }

  continuousPalette<- c(
                        "#0000CC",
                        "#0000FF",
                        "#0055FF",
                        "#00AAFF",
                        "#00FFFF",
                        "#2BFFD5",
                        "#55FFAA",
                        "#80FF80",
                        "#AAFF55",
                        "#D4FF2B",
                        "#FFFF00",
                        "#FFAA00",
                        "#FF5500",
                        "#FF0000",
                        "#CC0000")


  if(class(model)[1] == "prcomp"){
    #Either read ppm from argument or get from variable names
    #If no variable names stored in the model, use ordinals with a warning
    if (is.null(ppm)){
      if (is.null(row.names(model$rotation))){
        warning("No variable names provided, row indices will be used. Did you forget to input a chemical shift scale (ppm)?")
      }
      x <- as.numeric(rownames(data.frame(-model$rotation, check.names = F)))
    }  else{
      x <- as.numeric(ppm)
      #Check x length is consistent with data
      if (length(x) != nrow(model$rotation)) stop("Chemical shift scale length does not match the input model")
    } 
    #Check succsessful cast as.numeric
    if (any(is.na(x))) stop("Non-numeric chemical shift scale (ppm)")
    #Filter to roi
    idx <- which(x>roi[1] & x<roi[2])
    #Check that roi exists in the model
    if (length(idx) == 0) stop("No model for the given roi")
    #Ensure PC exists in the model
    if(ncol(model$x)< PC){
      stop("PC selected is larger than the dimension of the model")
    }
    x <- x[idx]
    method = "PCA"
    if(is.null(X)){
      stop("please define X which is a spectra data matrix used for the PCA model using prcomp")
    }
    cc <- abs(cor(X[,idx],model$x[,PC]))
    cv <- cov(X[,idx],model$x[,PC])
    raCol <- c(0, max(cc))
    df <- data.frame(x = x,y = cv,col = cc)
  }
  if(is(model)[1] == "list" & length(model) == 2){  # need to change this, for now, PCA() using library(prcomp) return list of 2
    if (is.null(ppm)){
      if (is.null(row.names(model$data$loadings))){
        warning("Chemical shift values were not provided, neither explicitely (ppm) nor as stored variable names. Row indices will be used instead")
      }
      x <- as.numeric(rownames(as.data.frame(-model$data$loadings, check.names = F)))
    }  else{
      x <- as.numeric(ppm)
      #Check x length is consistent with data
      if (length(x) != nrow(model$data$loadings)) stop("Chemical shift scale length does not match the input model")
    } 
    #Check succsessful cast as.numeric
    if (any(is.na(x))) stop("Non-numeric chemical shift scale (ppm)")
    #Filter to roi
    idx <- which(x>roi[1] & x<roi[2])
    #Check that roi exists in the model
    if (length(idx) == 0) stop("No model for the given roi")
    #Ensure PC exists in the model
    if(ncol(model$data$scores)< PC){
      stop("PC selected is larger than the dimension of the model")
    }
    x <- x[idx]
    method = "PCA"
    cc <- abs(cor(model$data$rawData[,idx],model$data$scores[,PC]))
    cv <- cov(model$data$rawData[,idx],model$data$scores[,PC])
    raCol <- c(0, max(cc))
    df <- data.frame(x = x, y = cv, col = cc)
  }
  if(is(model)[1] == "opls"){
    method <- model@typeC
    if (is.null(ppm)){
      if (is.null(row.names(model@loadingMN))){
        warning("No variable names provided, row indices will be used. Did you forget to input a chemical shift scale (ppm)?")
      }
      x <- as.numeric(rownames(data.frame(-model@loadingMN, check.names = F)))
    }  else{
      x <- as.numeric(ppm)
      #Check x length is consistent with data
      if (length(x) != nrow(model@loadingMN)) stop("Chemical shift scale length does not match the input model")
    }
    #Check succsessful cast as.numeric
    if (any(is.na(x))) stop("Non-numeric chemical shift scale (ppm)")
    #Filter to roi
    idx <- which(x>roi[1] & x<roi[2])
    #Check that roi exists in the model
    if (length(idx) == 0) stop("No model for the given roi")
    #Ensure PC exists in the model
    if(ncol(model@loadingMN)< PC){
      stop("PC selected is larger than the dimension of the model")
    }
    if(type == "Backscaled"){
      ldng <- model@loadingMN[,PC]
      m <- abs(ldng)
      #The color min-max needs to be computed before filtering, else the scale
      #re-adjust to the roi
      col <- (m - min(m))/(max(m) - min(m))
      cen <- model@xSdVn[idx]
      y <- ldng[idx]*cen
      x <- x[idx]
      m <- m[idx]
      col <- col[idx]
      # names(y)[1] <- "y"
      df <- data.frame(x = x, y = y, col = col)
      raCol = NULL
    }
    if(type == "Statistical reconstruction"){
      if(is.null(X)){
        stop("please defined X which is a spectra data matrix used for the PCA model using prcomp")
      }
      if(grepl("O",method)){ # for opls use scores
        df <- data.frame(model@orthoScoreMN, check.names = F)
      }else{ # for pls use orthogonal
        df <- data.frame(model@scoreMN, check.names = F)
      }
      df <- df[idx,PC]
      x <- x[idx]
      cc <- abs(cor(X[,idx],df))
      cv <- cov(X[,idx],df)
      raCol <- c(0, max(cc))
      df <- data.frame(x = x, y = cv, col = cc)
    }
  }
  if(method == "PCA"){
    p1 <- ggplot(df,
                 aes(x = x,
                     y = y,
                     color = col)) +
        geom_line() +
        scale_x_reverse(breaks = breaks_pretty(n = 15)) +
        scale_colour_gradientn(colors = continuousPalette,
                               limits = raCol,
                               name = expression("|p"["sc"] * "|")) +
        labs(title = method,
             subtitle = "Statistical reconstruction",
             caption = paste0("PC: ", PC, " loadings"),
             x = expression(delta ~ {}^1 * H ~ (ppm)),
             y = "") +
        theme_bw()
  }else{
    if( is(model)[1] == "opls" &  model@typeC=="OPLS-DA" & Median =="TRUE"){
      spec_df <- data.frame(t(do.call("rbind",model@suppLs$x)))
      if (is.null(ppm))  x <- as.numeric(gsub("X","",colnames(spec_df))) else x <- ppm
      # y_groups<-unique(model@suppLs$y)
      data.frame(y = model@suppLs$y, model@scoreMN) -> y_groups
      y_groups$p1[which(y_groups$p1>0)] <- 1
      y_groups$p1[which(y_groups$p1<0)] <- -1
      y_groups<-unique(y_groups)
      idx =  which(x>roi[1] & x<roi[2])
      median_spectra_df<-list()
      for (i in 1:nrow(y_groups)) {
        if (y_groups$p1[i] > 0) {
          median_spectra_df[["pos"]] <-
            apply(spec_df[which(model@suppLs$y == y_groups$y[i]), idx], 2, median)
        } else{
          median_spectra_df[["neg"]] <-
            -apply(spec_df[which(model@suppLs$y == y_groups$y[i]), idx], 2, median)
        }
      }
      txt = paste0(as.character(y_groups$y[1])," = ", ifelse(y_groups$p1[1]>0,"Pos","Neg")," , ",
                  as.character(y_groups$y[2])," = ", ifelse(y_groups$p1[2]>0,"Pos","Neg"))
      df <- cbind(df, t(do.call("rbind",median_spectra_df)))
      coef =floor(c(max(abs(df$neg)),max(df$pos))[which.max(c(max(abs(df$neg)),max(df$pos)))]/max(abs(df$y)))
      p1 <- ggplot(df,
                   aes(x = x,
                       y = y,
                       color = col)) +
        geom_line() +
        geom_line(aes(y = pos / coef), color = "grey30", linetype = 1) +
        geom_line(aes(y = neg / coef), color = "grey30", linetype = 1) +
        scale_x_reverse(breaks = breaks_pretty(n = 15)) +
        scale_y_continuous(# Features of the first axis
          name = "",
          # Add a second axis and specify its features
          sec.axis = sec_axis( ~ . * coef, name = "Median")) +
        scale_colour_gradientn(
          colors = continuousPalette,
          na.value = "grey50",
          limits = raCol,
          name = expression("|p"["sc"] * "|")
        ) +
        labs(
          title = method,
          subtitle = paste0(type,"\n",txt),
          caption = paste0("component: ", PC, " loadings"),
          x = expression(delta ~ {
          } ^ 1 * H ~ (ppm))
        ) +
        theme_bw()
    }else{
      p1 <- ggplot(df,
                   aes(x = x,
                       y = y,
                       color = col)) +
          geom_line() +
          scale_x_reverse(breaks = breaks_pretty(n = 15)) +
          scale_colour_gradientn(colors = continuousPalette,
                                 na.value = "grey50",
                                 limits = raCol,
                                 name = expression("|p"["sc"] * "|")) +
          labs(title = method,
               subtitle = type,
               caption = paste0("component: ", PC, " loadings"),
               x = expression(delta ~ {}^1 * H ~ (ppm)),
               y = "") +
          theme_bw()
    }


  }

  return(p1)

}
