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
#' @return plot of the loadings
#' @import scales
#' @import colorRamps

#' @examples
#' PCA_pca_model<-PCA(NOESY,annotation = ANN$COVID)  # PCA
#' prcomp_pca_model<-prcomp(NOESY,scale. = T,rank. = 2) # PCA
#'
#' Y<-ANN$COVID
#' Y<-as.numeric(gsub("preCOVID",1,gsub("COVID-pos",2,Y))) for (pls-da)
#' Y1<-as.factor(ANN$COVID)  # for OPLS-DA
#' ropls_pls_model<-ropls::opls(NOESY,y = Y)
#' ropls_plsda_model<-ropls::opls(NOESY,y = ANN$COVID)
#' ropls_opls_model<-ropls::opls(NOESY,y = Y,predI = 1, orthoI = NA)
#' ropls_oplsda_model<-ropls::opls(NOESY,y = Y1,predI = 1, orthoI = NA)
#'
#' PlotLoadSpec(model = prcomp_pca_model)

PlotLoadSpec<-function(model,PC = 1,roi = c(0.5,9.5),option = list()){
  if(class(model)[1]=="prcomp"){
    df <- data.frame(-model$rotation)
    res<-summary(model)
    res<-round(c(res$importance['Proportion of Variance',1:ncol(df)])*100,1)
    x<-as.numeric(rownames(df))
    idx<-which(x>roi[1] & x<roi[2])
    df<-df[idx,]
    x<-x[idx]
    cen<-data.frame(model$center[idx])
    method = "PCA"
  }
  if(class(model)[1]=="list" & names(model)[1]=="data"){  # need to change this, for now, PCA() using library(prcomp) return list of 2
    res<-model$data$pcSum$`Proportion of Variance`
    df<-as.data.frame(-model$data$loadings)
    x<-as.numeric(rownames(df))
    idx<-which(x>roi[1] & x<roi[2])
    cen<-data.frame(model$data$center[idx])
    df<-df[idx,]
    x<-x[idx]
    method = "PCA"
  }
  if(class(model)[1]=="opls"){
    res<-model@summaryDF
    df<-data.frame(-model@loadingMN)
    x<-as.numeric(rownames(df))
    idx<-which(x>roi[1] & x<roi[2])
    df<-df[idx,]
    x<-x[idx]
    cen<--data.frame(model@weightMN[idx])
    method<-model@typeC
  }
  if(!class(model)[1]%in%c("opls","list","prcomp")){
    stop("Check the Model class: must be prcomp,opls, or the results from PCA")
  }
  if(PC<= ncol(df)){
    m<-abs(df[,PC])
    y<-df[,PC]*cen[,PC]
    col<-(m - min(m))/(max(m) - min(m))
    df<-data.frame(x = x,y = y,col = col)
  } else{
    stop("PC selected is larger than the dimension of the model")
  }
  
  if(method=="PCA"){
    p1<-ggplot(df,aes(x = x, y = y,color = col))+geom_line()+
      scale_x_reverse(breaks = scales::breaks_pretty(n = 15))+
      scale_colour_gradientn(colors = colorRamps::matlab.like2(10),
                             name = expression("|p"["sc"] * "|")) +
      labs(title = method,
           caption = paste0("PC: ", PC, " loadings"),
           x = expression(delta ~ {}^1 * H ~ (ppm)),
           y = "") +
      theme_bw()
  }else{
    p1<-ggplot(df,aes(x = x, y = y,color = col))+geom_line()+
      scale_x_reverse(breaks = scales::breaks_pretty(n = 15))+
      scale_colour_gradientn(colors = colorRamps::matlab.like2(10),
                             name = expression("|p"["sc"] * "|")) +
      labs(title = method,
           caption = paste0("component: ", PC, " loadings"),
           x = expression(delta ~ {}^1 * H ~ (ppm)),
           y = "") +
      theme_bw()
  }
  
  return(p1)
  
}