#' PlotLoadSpec
#'
#' Allow to plot loadings of PCA, PLS, OPLS,PLS-DA and OPLS-DA
#'
#' @param model are the output from :
#'                            PCA()
#'                            prcomp()
#'                            ropls::opls()
#' @param PC is the number of components that you want to plot the loadings of
#' Default is 1 and only require to change when plotting PCA or PLS model
#' @param  roi is the region of interest (default is from 0.5 to 9.5 ppm)
#' @param type is the loading visualization types
#'         "Statistical reconstruction" calculate the covariance for the trace (y) and Pearson's correlation of the predictive (O)PLS scores with each ppm for the color.//
#'         "Backscaling" calculate the trace with predicted loadings multiplied by the standard deviation of X feature, color is weighted by the OPLS model.//
#'         for PCA it is always "Statistical reconstruction".
#'         but for PLS and OPLS "Backscaling" method is recommended
#' @param X spectral data matrix used for the model.
#'        This is needed for PCA with prcomp package, or (O)PLS with type = "Statistical reconstruction".
#' @return plot of the loadings
#' @import scales
#' @examples
#' # PCA model,the loading will always use type = "Statistical reconstruction".
#' #PCA_pca_model <- PCA(NOESY, annotation = ANN$COVID)  # PCA
#' #prcomp_pca_model<-prcomp(NOESY,scale. = T,rank. = 2) # PCA
#'
#' #Y <- ANN$COVID
#' #Y<-as.numeric(gsub("preCOVID",1,gsub("COVID-pos",2,Y))) for (pls-da)
#' #Y1<-as.factor(ANN$COVID)  # for OPLS-DA
#' #ropls_pls_model<-ropls::opls(NOESY,y = Y)
#' #ropls_plsda_model<-ropls::opls(NOESY,y = ANN$COVID)
#' #ropls_opls_model<-ropls::opls(NOESY,y = Y,predI = 1, orthoI = NA)
#' #ropls_oplsda_model<-ropls::opls(NOESY,y = Y1,predI = 1, orthoI = NA)
#'
#' #PlotLoadSpec(model = prcomp_pca_model)
#' @export

PlotLoadSpec<-function(model,PC = 1,roi = c(0.5,9.5),type = "Backscaled",X = NULL,option = list()){

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


  if(class(model)[1]=="prcomp"){
    df <- data.frame(-model$rotation)
    res<-summary(model)
    res<-round(c(res$importance['Proportion of Variance',1:ncol(df)])*100,1)
    x<-as.numeric(rownames(df))
    idx<-which(x>roi[1] & x<roi[2])
    if(ncol(df)< PC){
      stop("PC selected is larger than the dimension of the model")
    }
    df<-df[idx,PC]
    x<-x[idx]
    method = "PCA"
    if(is.null(X)){
      stop("please defined X which is a spectra data matrix used for the PCA model using prcomp")
    }
    cc <- abs(cor(X[,idx],model$x[,PC]))
    cv<-cov(X[,idx],model$x[,PC])
    raCol <- c(0, max(cc))
    df<-data.frame(x = x,y = cv,col = cc)

  }
  if(is(model)[1]=="list" & length(model)==2){  # need to change this, for now, PCA() using library(prcomp) return list of 2
    res<-model$data$pcSum$`Proportion of Variance`
    df<-as.data.frame(-model$data$loadings)
    x<-as.numeric(rownames(df))
    idx<-which(x>roi[1] & x<roi[2])
    # cen<-data.frame(model$data$center[idx])
    if(ncol(df)< PC){
      stop("PC selected is larger than the dimension of the model")
    }
    df<-df[idx,PC]
    x<-x[idx]
    method = "PCA"
    cc <- abs(cor(model$data$rawData[,idx],model$data$scores[,PC]))
    cv<-cov(model$data$rawData[,idx],model$data$scores[,PC])
    raCol <- c(0, max(cc))
    df<-data.frame(x = x,y = cv,col = cc)
  }
  if(is(model)[1]=="opls"){
    method<-model@typeC
    res<-model@summaryDF
    if(type=="Backscaled"){
      df<-data.frame(-model@loadingMN)
      x<-as.numeric(rownames(df))
      idx<-which(x>roi[1] & x<roi[2])
      if(ncol(df)< PC){
        stop("PC selected is larger than the dimension of the model")
      }
      cen<--data.frame(model@xSdVn[idx])
      df<-df[idx,PC]
      x<-x[idx]
      m<-abs(df)
      y<-df*cen
      names(y)[1]<-"y"
      col<-(m - min(m))/(max(m) - min(m))
      df<-data.frame(x = x,y = y,col = col)
      raCol = NULL
    }
    if(type=="Statistical reconstruction"){
      if(is.null(X)){
        stop("please defined X which is a spectra data matrix used for the PCA model using prcomp")
      }

      if(grepl("O",method)){ # for opls use scores
        df<-data.frame(model@orthoScoreMN)
      }else{ # for pls use orthogonal
        df<-data.frame(model@scoreMN)
      }
      x<-as.numeric(rownames(-model@loadingMN))
      idx<-which(x>roi[1] & x<roi[2])
      if(ncol(df)< PC){
        stop("PC selected is larger than the dimension of the model")
      }
      df<-df[,PC]
      x<-x[idx]
      cc <- abs(cor(X[,idx],df))
      cv<-cov(X[,idx],df)
      raCol <- c(0, max(cc))
      df<-data.frame(x = x,y = cv,col = cc)
    }
    if(type != "Backscaled" & type !="Statistical reconstruction"){
      stop("Name for the visualization type must be 'Backscaled' or 'Statistical reconstruction'. ")
    }
  }
  if(!class(model)[1]%in%c("opls","list","prcomp")){
    stop("Check the Model class: must be prcomp,opls, or the results from PCA")
  }

  if(method=="PCA"){
    p1<-ggplot(df,aes(x = x, y = y,color = col))+geom_line() +
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
    p1<-ggplot(df,
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

  return(p1)

}
