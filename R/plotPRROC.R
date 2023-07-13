#' PR and ROC curve 
#'
#' Allow models from of PCA, PLS, OPLS,PLS-DA and OPLS-DA, or individual input of x and y
#'
#' @param model are the output from :
#'                            ropls::opls()
#'                            
#' @param x,y are two vectors that 
#' @return PRROC_plot 
#' plot: 
#' 
#' @import PRROC
#' @examples
#' Y<-as.numeric(gsub("preCOVID",1,gsub("COVID-pos",2,Y))) for (pls-da)
#' Y1<-as.factor(ANN$COVID)  # for OPLS-DA
#' ropls_pls_model<-ropls::opls(NOESY,y = Y)
#' ropls_plsda_model<-ropls::opls(NOESY,y = ANN$COVID)
#' ropls_opls_model<-ropls::opls(NOESY,y = Y,predI = 1, orthoI = NA)
#' ropls_oplsda_model<-ropls::opls(NOESY,y = Y1,predI = 1, orthoI = NA)
#' 
#' t<-plotPRROC(ropls_oplsda_model,caption = TRUE)
#'
#'plot(t[[1]])
#'
#'plot(t$plot)


library(PRROC)

plotPRROC<-function(model = NULL,x = NULL,y = NULL,PC = c(1,2),col = c("red","black"),caption = FALSE){
  if(exists("model")){ 
    if(class(model)[1]=="opls"){
      method<-model@typeC
      Y = as.numeric(factor(model@suppLs$y))
      x<--model@scoreMN[which(Y==PC[1])]
      y<--model@scoreMN[which(Y==PC[2])]  
    }else{
      stop("model is not from ropls function, Try x and y input instead")
    }
  }
  if(!exists("model")){
    if(exists("x") &exists("y")){
      x<-x
      y<-y
    }else{
      stop("Please specifiy ropls model or x and y input")
    }
  }
  roc<-roc.curve(scores.class0 = x, scores.class1 = y, curve = TRUE)
  pr<-pr.curve(scores.class0 = x, scores.class1 = y, curve = TRUE)
  ROC<-data.frame(roc$curve[,1:2])
  PR<-data.frame(rev(pr$curve[,1]),rev(pr$curve[,2])) # reverse the order
  names(ROC)<-names(PR)<-c("x","y")
  PR$y<-abs(1-PR$y) # change the scale for y axis 
  p1<-ggplot(ROC, aes(x, y)) + 
    geom_line(color = col[1]) +
    geom_line(data = PR, color = col[2])+
    theme_bw()+geom_abline(intercept = 0,linetype = "dashed")+scale_y_continuous(name =  "Sensitivity",breaks = seq(0,1,0.2),sec.axis = sec_axis(~abs(.-1),name = "Precision",breaks = seq(0,1,0.2)))+
    scale_x_continuous(name = "Recall",breaks = seq(0,1,0.2),sec.axis = dup_axis(name = "FPR",breaks = seq(0,1,0.2)))+coord_fixed(ratio = 1)
  if(caption == TRUE){
    text<-paste0("AUC ROC: ",round(roc$auc,2),"\n","AUC PR: ",round(pr$auc.integral,2))
    p1<-p1+annotate("text", x = 0.5, y = 0.5, label = text, angle=45)
  }
  if(caption == FALSE){
    text<-paste0("AUC ROC: ",round(roc$auc,2),", AUC PR: ",round(pr$auc.integral,2))
    p1<-p1+labs(caption = text)
  }
  plot(p1)
  
  PRROC_plot<-list(plot = p1,PR = pr,ROC =roc)
  return(PRROC_plot)
  
}


