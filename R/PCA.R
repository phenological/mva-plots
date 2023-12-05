#' Principal Component Calculation.
#'
#' The prcomp function is applied and uses the singular value decomposition (SVD) method. The maximum number of principal components calculated is 8.
#'
#' @param data A data frame or matrix of the numeric values for the principal component calculation.
#' @param plot To stop the generation of summary plots, set to FALSE. Default is TRUE.
#' @param center A logical value indicating whether the variables should be shifted to be zero centered. The default is TRUE. Alternately, a vector of length equal the number of columns of \code{data} can be supplied. The value is passed to scale.
#' @param scale A logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is TRUE. Alternatively, a vector of length equal the number of columns of \code{data} can be supplied.
#' @param cutoff A parameter for the \code{optns} list. The maximum percentage of cumulative variance you wish to explain. The default is 99%. for example, optns=list(cutoff= 90) to set a 90% cutoff. Set rank. = NULL if using this.
#' @return A list of objects used in further calculations.
#' @param results A list with class prcomp, for details of this list see the prcomp documentation.
#' @param pcSum A data frame of the summary information (Standard deviation, Proportion of Variance and Cumulative Proportion) for the principal component calculation.
#' @param scores The scores. The value of the rotated data (which has been centered and scaled) data multiplied by the loadings matrix. The cov(scores) is the diagonal matrix diag(sdev^2). For the formula method, napredict() is applied to handle the treatment of values omitted by the na.action.
#' @param loadings Variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#' @param sdev The standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).
#' @param center The centering used.
#' @param scale The scaling used.
#' @param threshold The number of principal components needed to explain the amount of cumulative variance specified (or the default of 99%).
#' @examples
#' #data(iris)
#' #a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
#' @export

#calculate Principal Components using prcomp

PCA <- function(data, center = TRUE, scale. = TRUE, rank = 5, plot = TRUE, optns=list()) {

  library(ggplot2)
  results <- prcomp(data,
                    rank = rank,
                    center = center,
                    scale. = scale.)

  rank <- ncol(results[["rotation"]])

  pcSum <- (as.data.frame(t(summary(results)[["importance"]])))
  pcSum <- pcSum[1:rank,]*100
  pcSum[,"Principal Component"] <- rownames(pcSum)
  pcSum[, "Principal Component"] <- factor(pcSum$`Principal Component`, levels = pcSum$`Principal Component`)

  scores <- results[["x"]]

  loadings <- results[["rotation"]]

  sdev <- results[["sdev"]]

  center <- results[["center"]]

  scale <- results[["scale"]]

  pcdf <- cbind(as.data.frame(scores))

  rawData <- data

  dataSC<- as.data.frame(scale(rawData, scale=TRUE, center=T))

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
