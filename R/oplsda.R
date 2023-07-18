#' oplsda.
#'
#' Function that uses ropls::opls to perform O-PLS-DA modelling. Please see the ropls::opls help for further details.
#'
#' @param X Numerical data for the model. Matrix or data frame accepted. NA's accepted since NIPALs is applied.
#' @param Y Response to be modeled. Vector, matrix or data frame of either numeric (O-PLS) or character/factor (O-PLS-DA) response.
#' @param type Specify the type of analysis "PLS" or "OPLS". Discriminant analysis will automatically occur when character/factor is supplied to Y.
#' @param optns An empty list for additional modelling options.
#' @param predI Default NA (ropls will automatically set the number) for PLS, default 1 for OPLS. If \code{type} is PLS, you may set your own in the \code{optns} list.
#' @param orthoI Default NA (ropls will automatically set the number) for OPLS, default 0 for PLS. If \code{type} is OPLS, you may set your own in the \code{optns} list.
#' @param crossvalI Number of folds in k-fold cross-validation. Default is 7. You may set your own in the \code{optns} list.
#' @param log10L Logical for if the \code{X} should be log10 transformed. Default is FALSE, can be changed to TRUE in the \code{optns} list.
#' @param permI Number of permutations applied for model validation. Default is 2, can be changed in \code{optns} list. Please note, if subset in \code{optns} is changed from NULL, permutations aren't calculate.
#' @param scaleC Default is "standard". You can change this in the \code{optns} list. Choose from "standard" for mean-centering and unit variance scaling, "none" for no scaling or centering, "center" for mean centering only, "pareto" for mean-centering and Pareto scaling.
#' @param subset Default is NULL. You can change this in the \code{optns} list. Set as vector of observation indices to be used for training set or "odd" for equal train and test subsets. Equal subsetting will take into account the classes proportions as training sets should be representative.
#' @return The ropls object. Please refer to the ropls::opls help for details.
#' @examples
#' data(mtcars)
#' a <- oplsda(X = mtcars[,1:7], Y = [,8], type = "PLS", optns = list(permI = 50))
#' a <- oplsda(X = mtcars[,1:7], Y = [,8], type = "OPLS", optns = list(permI = 50))

oplsda <- function(X, Y, type, optns=list()){

#orthoI
 if(type == "PLS"){
   orthoI <- 0
 } else if(type == "OPLS"){
   orthoI <- NA
 }

  if (type == "OPLS" && "orthoI" %in% names(optns)){
   orthoI <- optns$orthoI
 }

#predI
  if(type == "PLS"){
    predI <- NA
  } else if(type == "OPLS"){
    predI <- 1
  }

  if (type == "PLS" && "predI" %in% names(optns)){
    predI <- optns$predI
  }

  #crossvalI
    if("crossvalI" %in% names(optns)){
        crossvalI <- optns$crossvalI
    } else {crossvalI <- 7}

  #scaleC
    if("scaleC" %in% names(optns)){
      scaleC <- optns$scaleC
    } else {scaleC <- "standard"}

  #log10L
    if("log10L" %in% names(optns)){
     log10L <- optns$log10L
    }else {log10L <- FALSE}
  #subset
    if("subset" %in% names(optns)){
      subset <- optns$subset
    } else {subset <- NULL}

#permI (default 20 for single response models and 0 ortherwise)
if("permI" %in% names(optns)){
  permI <- optns$permI
} else {permI <- 20}

  object <- ropls::opls(x = X,
                        y = Y,
                        predI = predI,
                        orthoI = orthoI,
                        crossvalI = crossvalI,
                        scaleC = scaleC,
                        log10L = log10L,
                        subset = subset,
                        permI = permI)

return(object)
}

