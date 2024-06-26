% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrogram.R
\name{dendrogram}
\alias{dendrogram}
\title{Dendrogram}
\usage{
dendrogram(ps, p = 2, breaks = 1)
}
\arguments{
\item{ps}{An object from the plotScores function.}

\item{p}{number of principal components to include. 2 will include PC1 and PC2,
3 will include PC1, PC2 and PC3.}

\item{breaks}{Number of longest edges to add breaks for easier visualization.
Default is 1. Providing 2 will add breaks the the top two longest edges.}
}
\value{
A phylogenic tree with pvalues at each merge based on the number of
principal components provided. Each leaf will have the same name and coloring
as the plot score graph.
}
\description{
Dendrogram of group separation of plotscores color assigned groups.
}
\examples{

pca <- PCA(data = iris[,1:4])
ps <- plotScores(model = pca,
                optns = list(color = iris$Species,
                             ellipse = "color"))
dendrogram(ps = ps, p = 2, breaks = 2)

}
