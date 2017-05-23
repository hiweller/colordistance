% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_visualization.R
\name{plotClusters}
\alias{plotClusters}
\title{Plot clusters in 3D color space}
\usage{
plotClusters(clusterList, hsv = F, p = "all", pausing = T)
}
\arguments{
\item{clusterList}{A list of identically sized dataframes with 4 columns each
(R, G, B, Pct or H, S, V, Pct) as output by \code{extractClusters} or
\code{getHistList}.}

\item{hsv}{Logical. Should HSV axes be used instead of RGB?}

\item{p}{Numeric vector of indices for which elements to plot; otherwise each
set of clusters is plotted in succession.}

\item{pausing}{Logical. Should the function pause and wait for user keystroke
before plotting the next plot?}
}
\value{
A 3D \code{\link[plotly]{plot_ly}} plot of cluster sizes in the
  specified colorspace for each cluster dataframe provided.
}
\description{
Interactive, 3D \code{\link[plotly]{plot_ly}} plots of cluster sizes and
colors for each image in a list of cluster dataframes in order to visualize
cluster output. Requires a plotly account.
}
\examples{
clusterList <- getHistList("Heliconius/", plotting=F)
plotClusters(clusterList, p=c(1:3, 7:8))

clusterListHSV <- getHistList("Heliconius/", hsv=T, plotting=F)
plotClusters(clusterListHSV, p=c(1:3, 7:8), hsv=T, pausing=F)
}