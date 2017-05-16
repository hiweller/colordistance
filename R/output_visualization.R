#' Plot clusters in 3D color space
#'
#' Interactive, 3D \code{\link[plotly]{plot_ly}} plots of cluster sizes and
#' colors for each image in a list of cluster dataframes in order to visualize
#' cluster output. Requires a plotly account.
#'
#' @param clusterList A list of identically sized dataframes with 4 columns each
#'   (R, G, B, Pct or H, S, V, Pct) as output by \code{extractClusters} or
#'   \code{getHistList}.
#' @param hsv Logical. Should HSV axes be used instead of RGB?
#' @param p Numeric vector of indices for which elements to plot; otherwise each
#'   set of clusters is plotted in succession.
#' @param pausing Logical. Should the function pause and wait for user keystroke
#'   before plotting the next plot?
#'
#' @return A 3D \code{\link[plotly]{plot_ly}} plot of cluster sizes in the
#'   specified colorspace for each cluster dataframe provided.
#'
#' @examples
#' clusterList <- getHistList("Heliconius/", plotting=F)
#' plotClusters(clusterList, p=c(1:3, 7:8))
#'
#' clusterListHSV <- getHistList("Heliconius/", hsv=T, plotting=F)
#' plotClusters(clusterListHSV, p=c(1:3, 7:8), hsv=T, pausing=F)
#' @export
plotClusters <- function(clusterList, hsv=F, p="all", pausing=T) {

  # Set plotting parameters
  if (hsv) {
    scene <- list(xaxis=list(title="Hue", range=c(0, 1)), yaxis=list(title="Saturation", range=c(0, 1)), zaxis=list(title="Value", range=c(0, 1)), camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  } else {
    scene <- list(xaxis=list(title="Red", linecolor=plotly::toRGB("red"), linewidth=6, range=c(0, 1)), yaxis=list(title="Green", linecolor=plotly::toRGB("green"), linewidth=6, range=c(0, 1)), zaxis=list(title="Blue", linecolor=plotly::toRGB("blue"), linewidth=6, range=c(0, 1)), camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  }

  # If p is all just create vector for cycling through every element
  if (p[1]=="all") {
    p <- c(1:length(clusterList))
  }

  if (is.data.frame(clusterList)) {
    temp <- vector("list", 1)
    temp[[1]] <- clusterList
    clusterList <- temp
    pausing <- FALSE
    p <- 1
  }

  # Plot clusters for each cluster element
  if (is.numeric(p)) {
    for (i in p) {
      if (hsv) {
        colExp <- apply(clusterList[[i]], 1, function(x) hsv(x[1], x[2], x[3]))
        } else {
          colExp <- apply(clusterList[[i]], 1, function(x) rgb(x[1], x[2], x[3]))
      }
      library(magrittr, quietly=T)
      pl <- plotly::plot_ly(clusterList[[i]], x=~clusterList[[i]][, 1], y=~clusterList[[i]][, 2], z=~clusterList[[i]][, 3], size=~clusterList[[i]][, 4], color=~clusterList[[i]][, 4]) %>% plotly::add_markers(color=I(colExp), size=~clusterList[[i]][, 4], sizes=c(10, 5000)) %>% plotly::layout(scene=scene, title=names(clusterList)[i])
      print(pl)
      if (pausing) {
        pause()
        }
    }
  } else {
    stop("'p' must be either a numeric vector of indices for plotting or set to 'all' to plot all sets in cluster list")
    }

}

#' Plot several different cluster sets together
#'
#' Plots cluster sets from several different dataframes on a single plot for
#' easy comparison. Each cluster plotted is colored according to its actual
#' color, and labeled according to the image from which it originated.
#'
#' @param clusterList A list of identically sized dataframes with 4 columns each
#'   (R, G, B, Pct or H, S, V, Pct) as output by \code{extractClusters} or
#'   \code{getHistList}.
#' @param hsv Logical. Should HSV axes be used instead of RGB?
#' @param p Numeric vector of indices for which elements to plot; otherwise all
#'   of the cluster sets provided will be plotted together.
#' @param title Optional title for the plot.
#'
#' @return A single \code{\link{plotly}{plot_ly}} plot of every cluster in a
#'   list of cluster sets. Each cluster is colored by cluster color,
#'   proportional to cluster size, and labeled according to the image from which
#'   it originated.
#'
#' @examples
#' clusterList <- getHistList("Heliconius/", plotting=F)
#' plotClustersMulti(clusterList, p=c(1:4), title="Orange and black Heliconius")
#' plotClustersMulti(clusterList, p=c(5:8), title="Black, yellow, and red Heliconius")
#'
#' clusterListHSV <- getHistList("Heliconius/", hsv=T, plotting=F)
#' plotClustersMulti(clusterListHSV, p=c(1:3, 7:8), hsv=T)
#' @export
plotClustersMulti <- function(clusterList, hsv=F, p="all", title="") {

  # Set plotting parameters
  if (hsv) {
    scene <- list(xaxis=list(title="Hue", range=c(0, 1)), yaxis=list(title="Saturation", range=c(0, 1)), zaxis=list(title="Value", range=c(0, 1)), camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  } else {
    scene <- list(xaxis=list(title="Red", linecolor=plotly::toRGB("red"), linewidth=6, range=c(0, 1)), yaxis=list(title="Green", linecolor=plotly::toRGB("green"), linewidth=6, range=c(0, 1)), zaxis=list(title="Blue", linecolor=plotly::toRGB("blue"), linewidth=6, range=c(0, 1)), camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  }

  # If p is set to "all" then use entire list
  if (p[1]=="all") {
    p <- names(clusterList)
  }

  # Make new list of just specified values and flatten it
  newList <- vector("list", length(p))

  for (i in 1:length(p)) {
    newList[[i]] <- clusterList[[i]]
    names(newList)[i] <- p[i]
  }

  newDF <- newList[[1]]
  newDF$Image <- rep(names(newList)[1], dim(newList[[1]])[1])

  # Add an "image" ID column for plotting
  for (i in 2:length(newList)) {
    tempDF <- newList[[i]]
    tempDF$Image <- rep(names(newList)[i], dim(newList[[i]])[1])
    newDF <- rbind(newDF, tempDF)
  }

  if (hsv) {
    colExp <- apply(newDF[, 1:3], 1, function(x) hsv(x[1], x[2], x[3]))
    } else {
      colExp <- apply(newDF[, 1:3], 1, function(x) rgb(x[1], x[2], x[3]))
  }
  library(magrittr, quietly = T)
  pl <- plotly::plot_ly(newDF, x = ~newDF[, 1], y = ~newDF[, 2], z = ~newDF[, 3], size=~Pct, text = ~paste("Image: ", Image)) %>%
    plotly::add_markers(color=I(colExp), size=~Pct, sizes=c(10, 5000)) %>%
    plotly::layout(scene = scene, title=title)
  print(pl)
}

#' Plot a heatmap of a distance matrix
#'
#' Plots a heatmap of a symmetrical distance matrix in order to visualize
#' similarity/dissimilarity in scores. Values are clustered by similarity using
#' \code{\link[stats]{hclust}}.
#' @param clusterList_or_matrixObject Either a list of identically sized
#'   dataframes with 4 columns each (R, G, B, Pct or H, S, V, Pct) as output by
#'   \code{\link{extractClusters}} or \code{\link{getHistList}}, or a symmetrical distance matrix as output by \code{\link{getColorDistanceMatrix}}.
#' @param main Title for heatmap plot.
#' @param col Color scale for heatmap from low to high. Default is \code{colorRampPalette(c("royalblue4", "ghostwhite", "goldenrod2"))(n=299)}, where yellow is more dissimilar and blue is more similar.
#'
#' @return Heatmap representation of distance matrix.
#'
#' @examples
#' clusterList <- getHistList("Heliconius/")
#' CDM <- getColorDistanceMatrix(clusterList, plotting=F)
#' heatmapColorDistance(CDM, main="Heliconius color similarity")
#' heatmapColorDistance(clusterList, col=colorRampPalette(c("red", "cyan", "blue"))(n=299))
#' @export
heatmapColorDistance <- function(clusterList_or_matrixObject, main=NULL, col="default") {

  # Shorter handle
  obj <- clusterList_or_matrixObject

  # If a clusterList was provided, get the distance matrix; if it"s a matrix, just make the heatmap; if neither, throw an error
  if (is.list(obj)) {
    obj <- getColorDistanceMatrix(obj)
  } else if (!is.matrix(obj)) {
    stop("Argument is not a list (extractClusters or getHistList object) or a distance matrix (getColorDistanceMatrix object)")
  }

  # The default heatmap colors are genuinely offensive to the eyes so make the default something more palatable; otherwise use user-provided vector
  if (col=="default") {
    col <- colorRampPalette(c("royalblue4", "ghostwhite", "goldenrod2"))(n=299)
  }

  # Convert to "dist" object for hclust method and plot heatmap
  clust <- as.dist(obj)
  heatmap(obj, symm=TRUE, col=col, Rowv=as.dendrogram(hclust(clust)), main=main)

}
