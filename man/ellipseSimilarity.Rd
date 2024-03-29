% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ellipseSimilarity.R
\name{ellipseSimilarity}
\alias{ellipseSimilarity}
\title{Ellipse Similarity}
\usage{
ellipseSimilarity(ps, type = "jaccard")
}
\arguments{
\item{ps}{an object containing (or is) a plotScores graph. Can be an oplsda
object or a PCA object (singular and grid both accepted).}

\item{type}{a character, either "jaccard" for the jaccard index or "coefficient"
for the overlap coefficient. The default is "jaccard". The jaccard index is the
size of the intersection of set A and set B (i.e. the number of common elements)
over the size of the union of set A and set B. The overlap coefficient (or
Szymkiewicz–Simpson coefficient), is the size of the intersection of set A and
set B over the size of the smaller set between A and B. This function also
provides case the intersection over the size of the larger set. The overlap
coefficient is often proffered when the sizes of the sets differ.}
}
\value{
a list with ellipse similarity for each graph
}
\description{
Either the Jaccard index or overlap coefficient. The closer the result is to
one, the more overlap there is between the ellipses. For more than two ellipses,
there will be a pairwise comparison.
}
\examples{
# oplsda object
data(mtcars)
pls <- oplsda(X = mtcars[,1:11],
              Y = as.factor(mtcars$vs),
              type = "OPLS",
              optns = list())
plsps <- plotScores(model = pls,
                    optns = list(ellipse = "color"))
result <- ellipseSimilarity(ps = plsps, type = "jaccard")

#PCA object (grid)
pca <- PCA(data = mtcars[,1:7],
          rank = 3,
          plot = FALSE)
psg<- plotScores(model = pca, optns = list(color = as.factor(mtcars$vs),
                                           ellipse ="color"))
ellipseSimilarity(ps = psg, type = "jaccard")


}
