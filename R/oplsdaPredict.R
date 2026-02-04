#' oplsdaPredict
#'
#' Function that uses ropls::opls model to make predictions on a new data set.
#' Only the first opls predicitve and orthogonal component used currently.
#'
#' @param model oplsda model (or ropls built model).
#' @param newdata Matrix or data frame of the same X variables as the model
#' (O-PLS-DA).
#' @param optns An empty list for confusion matrix addition.
#'    \describe{
#'     \item{real}{The "real" classifications for the newdata as a factor. If
#'      supplied, a confusion matrix will be calculated. Only available for DA.}
#'      \item{orthoI}{If you have an OPLS with more than one orthogonal
#'      component, you can specify how many you would like used in the
#'      predictive calculation. If not specified, all available orthogonal
#'      components will be used.}
#'      }
#'
#' @returns The prediction model including the predictive scores and orthogonal
#' scores.
#' @importFrom caret confusionMatrix
#' @examples
#' data(mtcars)
#' a <- oplsda(X = mtcars[,1:5],
#'             Y = mtcars[,8],
#'             type = "OPLS",
#'             optns = list(permI = 50))
#'
#' jitterCol <- c("mpg", "cyl", "disp", "hp", "drat")
#'
#' egData <- mtcars
#'
#' egData[, jitterCol] <- lapply(X = egData[, jitterCol],
#'                               FUN = function(x) jitter(x, factor = 0.1))
#'
#' b <- oplsdaPredict(model = a,
#'                    newdata = egData[,1:5])
#' @export

oplsdaPredict <- function (model, newdata, optns=list()){
  #center and scale new data
  xteMN <- scale(newdata, model@xMeanVn, model@xSdVn)
#check there is an orthogonal component
  if(model@summaryDF[, "ort"] > 0) {
    #allow user to specify how many orthogonal components should be removed.
    if("orthoI" %in% names(optns)){
      components <- optns$orthoI
    }else{ components <- model@summaryDF[, "ort"]}

    #can only use the first orthogonal component hence model@summaryDF[,1] not model@summaryDF[,"ort"] in for statement
    for(noN in 1:components) {
      if(model@suppLs[["naxL"]]) {
        #make empty matrix
        xtoMN <- matrix(0, nrow = nrow(xteMN), ncol = 1)
        for(i in 1:nrow(xtoMN)) {

        #ensure pick rows without NAs
        comVl <- complete.cases(xteMN[i, ])

        #calculate
        xtoMN[i, ] <- crossprod(xteMN[i, comVl], model@orthoWeightMN[comVl, noN]) / drop(crossprod(model@orthoWeightMN[comVl, noN]))
        }
      } else{
        #Do a loop where xteMN gets regenerated after removing each orthogonal component (xtoMN). Below the loop already exists and doesn't need modification. If noN is 1 then only 1st ortho component is used.
        #calculate the orthogonal scores
        xtoMN <- xteMN %*% model@orthoWeightMN[, noN]
        #take out the orthogonal components from newdata X
        xteMN <- xteMN - tcrossprod(xtoMN, model@orthoLoadingMN[, noN])
      }
    }
    #calculate the predictive scores, can only use the first predictive component since that's all there is for and OPLS, hence model@weightMN[,1] not model@sweightMN[,noN]
    t_pred <- xteMN %*% model@weightMN[, 1]
  }

#if there's no orthogonal component
  if(!(model@summaryDF[, "ort"] > 0)){

    t_pred <- matrix(NA, nrow = nrow(xteMN), ncol = model@summaryDF[, "pre"])

    #calculate the predictive scores
    for(noN in 1:model@summaryDF[, "pre"]){
      t_pred[, noN] <- xteMN %*% model@weightMN[, noN]
      xtoMN <- NULL
    }
  }


#use the changed newdata X that only had predictive comp to calculate Y prediction
  if(model@suppLs[["naxL"]]) {
    yTesScaMN <- matrix(0,
                        nrow = nrow(xteMN),
                        ncol = ncol(model@coefficientMN),
                        dimnames = list(rownames(xteMN),
                                        colnames(model@coefficientMN)))

    for(j in 1:ncol(yTesScaMN))
      for(i in 1:nrow(yTesScaMN)) {
        comVl <- complete.cases(xteMN[i, ])
        yTesScaMN[i, j] <- crossprod(xteMN[i, comVl], model@coefficientMN[comVl, j])
      }
  } else
    yTesScaMN <- xteMN %*% model@coefficientMN
    yTesMN <- scale(scale(yTesScaMN,
                          FALSE,
                          1 / model@ySdVn),
                    -model@yMeanVn,
                    FALSE)
    attr(yTesMN, "scaled:center") <- NULL
    attr(yTesMN, "scaled:scale") <- NULL


#if Y is a factor, convert predicted for new data to the same format

  if(is.factor(model@suppLs[["y"]])) {

    yTestMCN <- model@suppLs[[".char2numF"]](yTesMN,
                                             c2nL = FALSE)
    predMCNFcVcn <- as.character(yTestMCN)
    names(predMCNFcVcn) <- rownames(newdata)
    predMCNFcVcn <- factor(predMCNFcVcn, levels = levels(model@suppLs[["y"]]))

  } else if(is.vector(model@suppLs[["y"]])) {

    if(is.character(model@suppLs[["y"]])) {

      yTestMCN <- model@suppLs[[".char2numF"]](yTesMN, c2nL = FALSE)
      predMCNFcVcn <- as.character(yTestMCN)
      names(predMCNFcVcn) <- rownames(newdata)

    } else {

      predMCNFcVcn <- as.numeric(yTesMN)
      names(predMCNFcVcn) <- rownames(newdata)

    }

  } else if (is.matrix(model@suppLs[["y"]])) {

    if (mode(model@suppLs[["y"]]) == "character") {
      predMCNFcVcn  <- model@suppLs[[".char2numF"]](yTesMN, c2nL = FALSE)
    } else
      predMCNFcVcn <- yTesMN
      rownames(predMCNFcVcn) <- rownames(newdata)

  }

#confusion matrix for DA or multi-class models, A Confusion matrix is an N x N matrix used for evaluating the performance of a classification model, where N is the total number of target classes.
    if("real" %in% names(optns)){

      if(is(optns$real)[1] == "factor"| is(optns$real)[1] == "character"){
        conf <- confusionMatrix(data = as.factor(predMCNFcVcn), reference = as.factor(optns$real))
      } else{conf <- list()
             warning("Your Y is not a factor or factorizable character, therefore no confusion matrix is supplied")}
    } else (conf <- list())

prediction <- list(orthoScoreMN = xtoMN,
                   predScoreMN = t_pred,
                   predY = predMCNFcVcn,
                   confusionMatrix = conf)

sp <- plotScores(model)@suppLs$ScoresPlot
#WARNING: this will break if you ever change the order of layers in plotScores
sp <- sp + geom_point(data=data.frame(Pred = prediction$predScoreMN
                                      ,Ortho = prediction$orthoScoreMN)
                      ,aes(x=Pred,y=Ortho),shape=1,size=sp$layers[[1]]$aes_params$size,color="black"
                      )
print(sp)
invisible(prediction)
}

##confusion matrix
#caret::confusionMatrix(iris$Species, sample(iris$Species))
# model@suppLs[["yMCN"]]
# model@suppLs[["yPreMN"]]

# if("real" %in% names(optns)){
#   caret::confusionMatrix(data = predMCNFcVcn, reference = optns$real)
# }


#caret::confusionMatrix(data = lot2[["predY"]], reference = lot@suppLs[["y"]])


