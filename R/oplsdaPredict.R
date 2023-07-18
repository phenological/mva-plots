#' oplsdaPredict
#'
#' Function that uses ropls::opls model to make predictions on a new data set.
#'
#' @param object oplsda model (or ropls built model).
#' @param newdata Matrix or data frame of the same X variables as the model object (O-PLS-DA).
#' @returns The prediction object including the predictive scores and orthogonal scores.
#' @examples
#' data(mtcars)
#' a <- oplsda(X = mtcars[,1:7], Y = [,8], type = "PLS", optns = list(permI = 50))
#' b <- oplsdaPredict(object = a, Y = [,8], newdata = )

oplsdaPredict <- function (object, newdata){
  #center and scale new data
  xteMN <- scale(newdata, object@xMeanVn, object@xSdVn)

  if(object@summaryDF[, "ort"] > 0) {

    for(noN in 1:object@summaryDF[, "ort"]) {
      if(object@suppLs[["naxL"]]) {
        #make empty matrix
        xtoMN <- matrix(0, nrow = nrow(xteMN), ncol = 1)
        for(i in 1:nrow(xtoMN)) {

          #ensure pick rows without NAs
          comVl <- complete.cases(xteMN[i, ])

          #calculate
          xtoMN[i, ] <- crossprod(xteMN[i, comVl], object@orthoWeightMN[comVl, noN]) / drop(crossprod(object@orthoWeightMN[comVl, noN]))
        }
      } else

      xtoMN <- xteMN %*% object@orthoWeightMN[, noN]
      t_pred <- xteMN %*% object@weightMN[, noN]
      xteMN <- xteMN - tcrossprod(xtoMN, object@orthoLoadingMN[, noN])
}

  }

  if(object@suppLs[["naxL"]]) {
    yTesScaMN <- matrix(0, nrow = nrow(xteMN), ncol = ncol(object@coefficientMN),
                        dimnames = list(rownames(xteMN), colnames(object@coefficientMN)))
    for(j in 1:ncol(yTesScaMN))
      for(i in 1:nrow(yTesScaMN)) {
        comVl <- complete.cases(xteMN[i, ])
        yTesScaMN[i, j] <- crossprod(xteMN[i, comVl], object@coefficientMN[comVl, j])
      }
  } else
    yTesScaMN <- xteMN %*% object@coefficientMN
  yTesMN <- scale(scale(yTesScaMN,
                        FALSE,
                        1 / object@ySdVn),
                  -object@yMeanVn,
                  FALSE)
  attr(yTesMN, "scaled:center") <- NULL
  attr(yTesMN, "scaled:scale") <- NULL

  if(is.factor(fitted(object))) {

    yTestMCN <- object@suppLs[[".char2numF"]](yTesMN,
                                              c2nL = FALSE)
    predMCNFcVcn <- as.character(yTestMCN)
    names(predMCNFcVcn) <- rownames(newdata)
    predMCNFcVcn <- factor(predMCNFcVcn, levels = levels(object@suppLs[["y"]]))

  } else if(is.vector(fitted(object))) {

    if(is.character(fitted(object))) {

      yTestMCN <- object@suppLs[[".char2numF"]](yTesMN,
                                                c2nL = FALSE)
      predMCNFcVcn <- as.character(yTestMCN)
      names(predMCNFcVcn) <- rownames(newdata)

    } else {

      predMCNFcVcn <- as.numeric(yTesMN)
      names(predMCNFcVcn) <- rownames(newdata)

    }

  } else if (is.matrix(fitted(object))) {

    if (mode(fitted(object)) == "character") {
      predMCNFcVcn  <- object@suppLs[[".char2numF"]](yTesMN, c2nL = FALSE)
    } else
      predMCNFcVcn <- yTesMN

    rownames(predMCNFcVcn) <- rownames(newdata)

  }

prediction <- list(orthoScoreMN = xtoMN,
                                       predScoreMN = t_pred,
                                       predY = predMCNFcVcn)
  invisible(prediction)
}

