#' PR and ROC curve
#'
#' Precision Recall (PR) and Receiver Operator Curves. Should be applied to
#' classifications problems, for example PLS-DA or O-PLS-DA.You can supply a
#' model (from ropls or mva plots if PLS-DA or OPLS-DA) or individual input of
#' x and y.
#'
#' @param model model produced by ropls opls() or mva-plots oplsda().
#' @param x,y are two vectors that contain elements for a class each. Class 1
#' and class 2 for x and y, respectively, are the default. If more that 2
#' classes are present, consider the One-vs-Rest scheme, which compares each
#' class against all the others (assumed as one) or the One-vs-One scheme, which
#' compares every unique pairwise combination of classes. For the One-vs-One,
#' you can specify different combinations of classes as x and y.
#' @param col colors of the ROC and PR curves, supplied as name (eg "red") or
#' hexadecimal or color. Default it col = c("red", "black").
#' @param caption Boolean for AU ROC and AUC PR. If TRUE, they appear in the
#' center of the graph, if FALSE they appear as a caption at the botom of the
#' graph.
#' @param PC A concatenation of the principal components to use, default is c(1, 2)
#' @return PRROC_plot
#' @import PRROC
#' @examples
#' data("mtcars")
#' Y<-as.factor(mtcars$vs)  # for DA
#'
#' #PLS-DA
#' plsda_model<-oplsda(X = mtcars[,1:5], Y = Y, type = "PLS")
#' ropls_plsda_model<-ropls::opls(x = mtcars[,1:5], y = Y, predI = NA, orthoI = 0)
#'
#' #O-PLS-DA
#' oplsda_model<-oplsda(X = mtcars[,1:5], Y = Y, type = "OPLS")
#' ropls_oplsda_model<-ropls::opls(x = mtcars[,1:5], y = Y, predI = 1, orthoI = NA)
#'
#' t<-plotPRROC(oplsda_model, caption = FALSE)
#'
#' #x and y
#' plotPRROC(x = oplsda_model@scoreMN[which(Y == 1)], y = oplsda_model@scoreMN[which(Y == 2)])
#'@export

plotPRROC<-function(model = NULL, x = NULL, y = NULL, PC = c(1,2), col = c("red","black"), caption = FALSE){

 #model supplied
  if(!is.null(model)){

    if(is(model)[1] == "opls"){

      #should only accept DA models
      if(grepl("DA", model@typeC) == FALSE){
        stop("Error: Your model is not a descriminant analysis and not appropriate for PRROC. Consider using RMSE, MAE or other regression metrics")
      }

      if(model@typeC == "PLS-DA"){
        if(length(model@ySdVn)>2){
          warning("Your model has more than 2 classes. Consider the One-vs-Rest, One-vs-One or another scheme, the first two classes are the default.")
        }

      }
      method <- model@typeC
      Y = as.numeric(factor(model@suppLs$y))
      x<--model@scoreMN[which(Y==PC[1])]
      y<--model@scoreMN[which(Y==PC[2])]
    }else{
      stop("model is not from ropls or oplsda function, Try x and y input instead")
    }
  }

  #model not supplied
  if(is.null(model)){
    if(exists("x") & exists("y")){
      x <- x
      y <- y
    }else{
      stop("Please specifiy ropls model or x and y input")
    }
  }

####Computation####
  #Compute the area under the receiver operating characteristics (ROC) curve (sensitivity vs FPR).
  roc <- roc.curve(scores.class0 = x, scores.class1 = y, curve = TRUE)

  #Compute the area under the precision-recall (PR) curve (precision vs recall).
  pr <- pr.curve(scores.class0 = x, scores.class1 = y, curve = TRUE)

  #make data frame
  ROC <- data.frame(roc$curve[,1:2])
  PR <- data.frame(rev(pr$curve[,1]), rev(pr$curve[,2])) # reverse the order
  names(ROC) <- names(PR) <-c ("x","y")
  PR$y <- abs(1-PR$y) # change the scale for y axis

####Plot####
  p1 <- ggplot(ROC,
              aes(x, y)) +
      geom_line(color = col[1]) +
      geom_line(data = PR,
                color = col[2]) +
      theme_bw() +
      geom_abline(intercept = 0,
                  linetype = "dashed") +
      scale_y_continuous(name =  "Sensitivity",
                         breaks = seq(0,1,0.2),
                         sec.axis = sec_axis(~abs(.-1),
                                             name = "Precision",
                                             breaks = seq(0,1,0.2))) +
      scale_x_continuous(name = "Recall",
                         breaks = seq(0,1,0.2),
                         sec.axis = dup_axis(name = "FPR",
                                             breaks = seq(0,1,0.2))) +
      coord_fixed(ratio = 1)

####Caption####
  if(caption == TRUE){
    text <- paste0("AUC ROC: ",round(roc$auc,2),"\n","AUC PR: ",round(pr$auc.integral,2))
    p1 <- p1 +
          annotate("text",
                   x = 0.5,
                   y = 0.5,
                   label = text,
                   angle = 45)
  }
  if(caption == FALSE){
    text <- paste0("AUC ROC: ",round(roc$auc,2),", AUC PR: ",round(pr$auc.integral,2))
    p1 <- p1 +
          labs(caption = text)
  }
  plot(p1)

  PRROC_plot <- list(plot = p1,
                     PR = pr,
                     ROC =roc)
  invisible(PRROC_plot)

}


