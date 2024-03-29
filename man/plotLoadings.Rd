% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotLoadings.R
\name{plotLoadings}
\alias{plotLoadings}
\title{Plot loadings.}
\usage{
plotLoadings(model, flat = FALSE, optns = list())
}
\arguments{
\item{model}{A PCA, oplsda or ropls object.}

\item{flat}{A logical for a flat O-PLS(DA) loadings plot. Only applicable to
oplsda models with an orthogonal component. Default is FALSE.}

\item{optns}{An empty list for addtional options:
\itemize{
\item{plotTitle}{A character for the title of the grid.}
\item{theme}{Personalize the plot theme you would like applied as you
would using theme() in ggplot. Example set
theme = theme(legend.position = "left", text=element_text(size=5)) in optns.}
\item{thresh}{A numeric for the number of PCAs to display in the grid.
The default is calculated in the PCA function.}
}}
}
\value{
The model list appended with the grid of loadings under plots.
}
\description{
Grid of the loadings plots using GGally::ggpairs up to a threshold number.
}
\examples{
data(iris)
a <- PCA(data = iris[,1:4], center = TRUE, scale. = TRUE)
b <- plotLoadings(model = a, optns = list(gridTitle = "Iris Dataset PC Loadings", thresh = 3))
}
