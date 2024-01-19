#' oplsda.
#'
#' Function that uses ropls::opls to perform O-PLS-DA modelling. Please see the
#' ropls::opls help for further details.
#'
#' @param X Numerical data for the model. Matrix or data frame accepted. NA's
#' accepted since NIPALs is applied.
#' @param Y Response to be modeled. Vector, matrix or data frame of either
#' numeric (O-PLS) or character/factor (O-PLS-DA) response.
#' @param type Specify the type of analysis "PLS" or "OPLS". Discriminant
#' analysis will automatically occur when character/factor is supplied to Y.
#' @param optns A list for additional options:
#'   \itemize{
#'    \item{predI}{Default NA (ropls will automatically set the number) for PLS,
#'    default 1 for OPLS. If \code{type} is PLS, you may set your own.}
#'    \item{orthoI}{Default NA (ropls will automatically set the number) for
#'    OPLS, default 0 for PLS. If \code{type} is OPLS, you may set your own.}
#'    \item{crossvalI}{Number of folds in k-fold cross-validation. Default is 7.
#'    You may set your own.}
#'    \item{log10L}{Logical for if the \code{X} should be log10 transformed.
#'    Default is FALSE, can be changed to TRUE.}
#'    \item{permI}{Number of permutations applied for model validation. Default
#'    is 2. Please note, if subset in \code{optns} is changed from NULL,
#'    permutations aren't calculate.}
#'    \item{scaleC}{Default is "standard". Choose from "standard" for
#'    mean-centering and unit variance scaling, "none" for no scaling or
#'    centering, "center" for mean centering only, "pareto" for mean-centering
#'    and Pareto scaling.}
#'    \item{subset}{Default is NULL. Set as vector of observation indices to be
#'    used for training set or "odd" for equal train and test subsets. Equal
#'    subsetting will take into account the classes proportions as training sets
#'    should be representative.}
#'   }
#'
#' @return The ropls object. Please refer to the ropls::opls help for details.
#' @examples
#' data(mtcars)
#' a <- oplsda(X = mtcars[,1:7], Y = mtcars[,8], type = "PLS", optns = list(permI = 50))
#' a <- oplsda(X = mtcars[,1:7], Y = mtcars[,8], type = "OPLS", optns = list(permI = 50))
#' @import BiocManager
#' @import ggplot2
#' @export

oplsda <- function(X, Y, type, optns=list()){

#orthoI
 if(type == "PLS"){
   orthoI <- 0
 } else if(type == "OPLS"){
   orthoI <- NA
 }

#if the number of orthogonal components is specified
  if (type == "OPLS" && "orthoI" %in% names(optns)){
    orthoI <- optns$orthoI
  }

#DA or not
  if(is(Y)[1] == "character"){
    typeY <- "DA"
  }else{typeY <- "notDA"}

  if(is(Y)[1] == "factor"){
    typeY <- "DA"
  }else{typeY <- "notDA"}


####if it is DA####
  if(typeY == "DA"){
    #handle if the number of levels is not 2
    levels<- length(table(as.factor(Y)))

    #if there are more than 2 levels
    if(levels > 2){
      oldNames<- (names(table(as.factor(Y))))

      #remove any levels with nothing in them
      Y <- as.factor(Y)
      Y <- as.character(Y)
      Y <- as.factor(Y)
      newlevels<- length(table(Y))
      newNames<- (names(table(Y)))

      #####if opls-da####
      #not allowed more than 2 groups
      if(type == "OPLS"){
        #if there are still more than 2 levels
        if(newlevels >2){
          stop("Error: You have more than 2 levels in your Y. OPLS-DA requires exactly 2 levels.")
        }

        #if there are now only two levels, print what the old levels were and what the new levels are
        if(newlevels == 2){
          warning(paste0("Your levels in Y have been changed from ", levels, " to 2. Old levels are ", paste(oldNames, collapse = ", "), ". Your new levels are ", paste(newNames, collapse = ", ") ))
        }
      }

      ####if pls-da####
      if(type == "PLS"){
      #allowed more than 2 groups
      #if there are now less levels than before, print what the old levels were and what the new levels are
      if(!(newlevels == levels)){
        warning(paste0("Your levels in Y have been changed from ", levels, " to ", newlevels,". Old levels are ", paste(oldNames, collapse = ", "), ". Your new levels are ", paste(newNames, collapse = ", ") ))
      }
}
    }

    #if there are less than 2 levels
    if(levels < 2){
      stop("Error: You have less than 2 levels in your Y. DA requires at least 2 levels.")
    }
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


#make sure X is a data frame for other functions
  if(!(is(X)[1] == "data.frame")){
    X <- as.data.frame(X)
  }

  model <- ropls::opls(x = X,
                        y = Y,
                        predI = predI,
                        orthoI = orthoI,
                        crossvalI = crossvalI,
                        scaleC = scaleC,
                        log10L = log10L,
                        subset = subset,
                        permI = permI,
                        fig.pdfC = "none")


  model@suppLs[["x"]] <-append(x = data.frame(), values = X)

invisible(model)
}

