#' Principal Component Calculation.
#'
#' The prcomp function is applied and uses the singular value decomposition (SVD) method. The maximum number of principal components calculated is 8.
#'
#' @param data A data frame of the values for the principal component calculation.
#' @param annotation A data frame of the accompanying metadata.
#' @param center A logical value indicating whether the variables should be shifted to be zero centered. The default is TRUE. Alternately, a vector of length equal the number of columns of \code{data} can be supplied. The value is passed to scale.
#' @param scale A logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is TRUE. Alternatively, a vector of length equal the number of columns of \code{data} can be supplied.
#' @return A list of objects used in further calculations.
#' @param results A list with class prcomp, for details of this list see the prcomp documentation.
#' @param pcSum A data frame of the summary information (Standard deviation, Proportion of Variance and Cumulative Proportion) for the principal component calculation.
#' @param scores The scores. The value of the rotated data (which has been centered and scaled) data multiplied by the loadings matrix. The cov(scores) is the diagonal matrix diag(sdev^2). For the formula method, napredict() is applied to handle the treatment of values omitted by the na.action.
#' @param loadings Variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#' @param sdev The standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).
#' @param center The centering used.
#' @param scale The scaling used
#' @examples


#calculate Principal Components using prcomp

pcResults <- function(data, annotation, center = TRUE, scale. = TRUE) {

  results <- prcomp(data, center = center, scale. = scale.)

  pcSum <- rownames_to_column(as.data.frame(t(summary(results)[["importance"]]))) %>%
            mutate(across(where(is.double), ~.x*100))

  scores <- results[["x"]]

  loadings <- results[["rotation"]]

  sdev <- results[["sdev"]]

  center <- results[["center"]]

  scale <- results[["scale"]]

  pcdf<- cbind(as.data.frame(scores), annotation)

  return(list(scores = scores,
              loadings = loadings,
              sdev = sdev,
              center = center,
              scale = scale,
              pcSum = pcSum,
              pcdf = pcdf))
}
