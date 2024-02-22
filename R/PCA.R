#' Principal Component Calculation.
#'
#' The prcomp function is applied and uses the singular value decomposition
#' (SVD) method. The maximum number of principal components calculated is 8.
#'
#' @param data A data frame or matrix of the numeric values for the principal
#' component calculation.
#' @param plot To stop the generation of summary plots, set to FALSE. Default
#' is TRUE.
#' @param center A logical value indicating whether the variables should be
#' shifted to be zero centered. The default is TRUE. Alternately, a vector of
#' length equal the number of columns of \code{data} can be supplied. The value
#' is passed to scale.
#' @param scale. A logical value indicating whether the variables should be
#' scaled to have unit variance before the analysis takes place. The default is
#' TRUE. Alternatively, a vector of length equal the number of columns of
#' \code{data} can be supplied.
#' @param rank The number of components calculated. Default is 5.
#' @param optns A list for additional options:
#'   \itemize{
#'    \item{cutoff}{The maximum percentage of cumulative variance you wish to
#'    explain. The default is 99 percent. for example, optns = list(cutoff = 90) to
#'    set a 90 percent cutoff. Set rank. = NULL if using this.}
#'   }
#'
#' @return A list containing:\tabular{ll}{
#'    \code{results} \tab A list with class prcomp, for details of this list see the
#'                        prcomp documentation. \cr
#'    \tab \cr
#'    \code{pcSum} \tab A data frame of the summary information (Standard deviation,
#'    Proportion of Variance and Cumulative Proportion) for the principal component
#'    calculation. \cr
#'    \tab \cr
#'    \code{scores} \tab The scores. The value of the rotated data (which has been
#'    centered and scaled) data multiplied by the loadings matrix. The cov(scores)
#'    is the diagonal matrix diag(sdev^2). For the formula method, napredict() is
#'    applied to handle the treatment of values omitted by the na.action. \cr
#'    \tab \cr
#'    \code{loadings} \tab Variable loadings (i.e., a matrix whose columns contain the
#'    eigenvectors). \cr
#'    \tab \cr
#'    \code{sdev} \tab The standard deviations of the principal components (i.e., the
#'    square roots of the eigenvalues of the covariance/correlation matrix, though
#'    the calculation is actually done with the singular values of the data matrix). \cr
#'    \tab \cr
#'    \code{center} The centering used. \cr
#'    \tab \cr
#'    \code{scale} The scaling used. \cr
#'    \tab \cr
#'    \code{threshold} The number of principal components needed to explain the
#'    amount of cumulative variance specified (or the default of 99(in %)). \cr
#' }
#' @examples
#' data(iris)
#' a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
#' @import stats
#' @import ggplot2
#' @export

#calculate Principal Components using prcomp

PCA <- function(data, center = TRUE, scale. = TRUE, rank = 5, plot = TRUE, optns=list()) {

  requireNamespace("ggplot2", quietly = TRUE)
  results <- prcomp(data,
                    rank = rank,
                    center = center,
                    scale. = scale.)

  rank <- ncol(results[["rotation"]])

  pcSum <- as.data.frame(t(summary(results)[["importance"]]), check.names = F)
  pcSum <- pcSum[1:rank,]*100
  pcSum[, "Principal Component"] <- rownames(pcSum)
  pcSum[, "Principal Component"] <- factor(pcSum$`Principal Component`, levels = pcSum$`Principal Component`)

  scores <- results[["x"]]

  loadings <- results[["rotation"]]

  sdev <- results[["sdev"]]

  center <- results[["center"]]

  scale <- results[["scale"]]

  pcdf <- cbind(as.data.frame(scores, check.names = F))

  rawData <- data

  dataSC<- as.data.frame(scale(rawData, scale=TRUE, center=T), check.names = F)

 if("cutoff" %in% optns){
   cutoff = optns$cutoff
 }else{cutoff = 99}

  t <- length(which(pcSum$`Cumulative Proportion` < cutoff))

if (plot == TRUE) {
  #Make cumulative variance plot

  cumulativeVariance <- ggplot(data = pcSum,
                               aes(x = `Principal Component`,
                                   y = `Cumulative Proportion`)) +
                        geom_point(colour = "blue") +
                        geom_line(group = 1,
                                  colour = "blue") +
                        ggtitle("Cumulative Variance") +
                        xlab("PC") +
                        ylab("Cumulative Variance Explained (%)")

  #Make Screeplot

  screeplot <- ggplot(data = pcSum,
                      aes(x = `Principal Component`,
                          y = (`Proportion of Variance`))) +
                geom_point(colour = "red") +
                geom_line(group = 1,
                          colour = "red") +
                ggtitle("Scree Plot") +
                xlab("PC") +
                ylab("Proportion of Variance (%)")

  #combined scree and cumulative variance plot
  combinedScreeCumulative <- ggplot(pcSum)  +
                              geom_bar(aes(x = `Principal Component`,
                                           y = `Proportion of Variance`),
                                           stat = "identity", fill = "grey20",
                                           color = "black",
                                           alpha = 0.4) +
                              geom_line(aes(x = `Principal Component`,
                                            y = `Cumulative Proportion`),
                                            stat = "identity",
                                            color = "orange2",
                                            linewidth = 2, group = 1) +
                              labs(title = "Screeplot and Cumulative Variance",
                                  x = "PC",
                                  y = "Cumulative Variance (%)") +
                              scale_y_continuous(sec.axis = sec_axis(~.*0.5, name = "Proportion of Variance (%)")) +
                              theme(axis.title.y = element_text(color = "orange3"),
                                    axis.title.y.right = element_text(color = "gray30"))

  plots = list(combinedScreeCumulative = combinedScreeCumulative,
               screeplot = screeplot,
               cumulativeVariance = cumulativeVariance)
  print(combinedScreeCumulative)
} else{plots = list()}

  data<-list(rawData = rawData,
             dataSC = dataSC,
             scores = scores,
             loadings = loadings,
             sdev = sdev,
             center = center,
             scale = scale,
             pcSum = pcSum,
             pcdf = pcdf,
             threshold = t)

  invisible(list(data = data,
                 plots = plots))
}
