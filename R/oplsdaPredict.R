#' oplsdaPredict
#'
#' Function that uses ropls::opls model to make predictions on a new data set. Only the first opls predicitve and orthogonal component used currently.
#'
#' @param model oplsda model (or ropls built model).
#' @param newdata Matrix or data frame of the same X variables as the model (O-PLS-DA).
#' @param optns An empty list for confusion matrix addition.
#' @param real A parameter for the \code{optns} list. The "real" clasifications for the newdata as a factor. If supplied, a confusion matrix will be calculated.
#' @returns The prediction model including the predictive scores and orthogonal scores.
#' @importFrom caret confusionMatrix
#' @examples
#' #data(mtcars)
#' #a <- oplsda(X = mtcars[,1:7], Y = mtcars[,8], type = "PLS", optns = list(permI = 50))
#' #b <- oplsdaPredict(model = a, newdata = mtcars[,1:7], optns = list(real = as.factor()))
#' @export

oplsdaPredict <- function (model, newdata, optns=list()){

  #center and scale new data
  xteMN <- scale(newdata, model@xMeanVn, model@xSdVn)

#check there is an orthogonal component
  if(model@summaryDF[, "ort"] > 0) {

    #can only use the first orthogonal component hence model@summaryDF[,1] not model@summaryDF[,"ort"] in for statement
    for(noN in 1:model@summaryDF[, 1]) {
      if(model@suppLs[["naxL"]]) {

        #make empty matrix
        xtoMN <- matrix(0, nrow = nrow(xteMN), ncol = 1)
        for(i in 1:nrow(xtoMN)) {

        #ensure pick rows without NAs
        comVl <- complete.cases(xteMN[i, ])

        #calculate
        xtoMN[i, ] <- crossprod(xteMN[i, comVl], model@orthoWeightMN[comVl, noN]) / drop(crossprod(model@orthoWeightMN[comVl, noN]))
        }
      } else

        #calculate the orthogonal scores
        xtoMN <- xteMN %*% model@orthoWeightMN[, noN]
        #calculate the predictive scores
        t_pred <- xteMN %*% model@weightMN[, noN]
        #take out the orthogonal components from newdata X
        xteMN <- xteMN - tcrossprod(xtoMN, model@orthoLoadingMN[, noN])
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

    if("real" %in% names(optns)){
      conf <- confusionMatrix(data = predMCNFcVcn, reference = optns$real)
    } else (conf <-list())

prediction <- list(orthoScoreMN = xtoMN,
                   predScoreMN = t_pred,
                   predY = predMCNFcVcn,
                   confusionMatrix = conf)

invisible(prediction)
}

##confusion matrix
# caret::confusionMatrix(iris$Species, sample(iris$Species))
# model@suppLs[["yMCN"]]
# model@suppLs[["yPreMN"]]

# if("real" %in% names(optns)){
#   caret::confusionMatrix(data = predMCNFcVcn, reference = optns$real)
# }


#caret::confusionMatrix(data = lot2[["predY"]], reference = lot@suppLs[["y"]])












# function (opls_model, newdata, idx_scale = NULL)
# {
#   if (!"OPLS_metabom8" %in% is(opls_model)) {
#     stop("Model input does not belong to class OPLS_Torben!")
#     return(NULL)
#   }
#   if (length(unique(opls_model@Y$ori)) != 2) {
#     stop("Predictions implemented only for regression or 2-class outcomes.")
#     return(NULL)
#   }
#   if (is.null(ncol(newdata))) {
#     X <- rbind(newdata)
#   }
#   else {
#     X <- newdata
#   }
#   if (length(opls_model@X_mean) != ncol(newdata)) {
#     stop("Newdata argument does not match training data.")
#   }
#   if (!is.null(idx_scale)) {
#     map_scale <- c(none = 0L, UV = 1L)
#     map_scale[match(opls_model@Parameters$scale, names(map_scale))]
#     sc_res <- .scaleMatRcpp(X,
#                             idx_scale - 1,
#                             center = opls_model@Parameters$center,
#                             scale_type = map_scale[match(opls_model@Parameters$scale,
#                                                          names(map_scale))])
#     X <- sc_res$X_prep
#   }
#   else {
#     if (all(!is.null(opls_model@X_mean) & !is.na(opls_model@X_mean)) &&
#         all(!is.null(opls_model@X_sd) & !is.na(opls_model@X_sd))) {
#       Xmc <- sweep(X, 2, opls_model@X_mean, FUN = "-")
#       X <- sweep(Xmc, 2, opls_model@X_sd, FUN = "/")
#     }
#   }
#   e_new_orth <- X
#   n_pcOorth = opls_model@nPC - 1
#   t_orth <- matrix(NA, nrow = nrow(X), ncol = n_pcOorth)
#   for (i in seq_len(n_pcOorth)) {
#     t_orth[, i] <- e_new_orth %*% t(t(opls_model@w_orth[i,]))/drop(crossprod(t(t(opls_model@w_orth[i, ]))))
#     e_new_orth <- e_new_orth - (cbind(t_orth[, i]) %*% t(opls_model@p_orth[i, ]))
#   }
#
#   if ((n_pcOorth) > 1) {
#     pc.orth <- pca(t_orth, pc = 1, scale = "UV")
#     t_orth_pca <- pc.orth@t[, 1]
#   }
#   else {
#     t_orth_pca <- NULL
#   }
#
#   t_pred <- e_new_orth %*% (opls_model@w_pred)
#   betas <- opls_model@betas_pred
#   q_h <- opls_model@Qpc
#   res <- matrix(NA, nrow = nrow(X), ncol = ncol(opls_model@t_pred))
#   for (i in seq_len(ncol(opls_model@t_pred))) {
#     opts <- t(cbind(betas[i]) %*% t_pred[, i]) %*% rbind(q_h[,
#                                                              i])
#     res[, i] <- apply(opts, 1, sum)
#   }
#   totalPrediction <- apply(res, 1, sum)
#   Y_predicted <- (totalPrediction * opls_model@Y_sd) + opls_model@Y_mean
#   if (opls_model@type == "DA") {
#     cs <- table(opls_model@Y$ori, opls_model@Y$dummy)
#     levs <- data.frame(Original = rownames(cs), Numeric = as.numeric(colnames(cs)),
#                        stringsAsFactors = FALSE, row.names = NULL)
#     Y_predicted <- levs$Original[apply(vapply(levs$Numeric,
#                                               function(x, y = Y_predicted) {
#                                                 abs(x - y)
#                                               }, Y_predicted), 1, which.min)]
#   }
#   out <- list(Y_predicted = Y_predicted, t_pred = t_pred,
#               t_orth = t_orth, t_orth_pca = t_orth_pca)
#   return(out)
# }
