#' Plot clusters in 3D color space
#'
#' Interactive, 3D \code{\link[plotly]{plot_ly}} plots of cluster sizes and
#' colors for each image in a list of cluster dataframes in order to visualize
#' cluster output.
#'
#' @param cluster.list A list of identically sized dataframes with 4 columns each
#'   (R, G, B, Pct or H, S, V, Pct) as output by \code{extractClusters} or
#'   \code{getHistList}.
#' @param color.space The color space (\code{"rgb"}, \code{"hsv"}, or
#'   \code{"lab"}) in which to plot pixels.
#' @param ref.white The reference white passed to
#'   \code{\link{convertColorSpace}}; must be specified if using
#'   \code{color.space = "lab"}.
#' @param to Display color space of image if clustering in CIE Lab space,
#'   probably either "sRGB" or "Apple RGB", depending on your computer.
#' @param p Numeric vector of indices for which elements to plot; otherwise each
#'   set of clusters is plotted in succession.
#' @param pausing Logical. Should the function pause and wait for user keystroke
#'   before plotting the next plot?
#' 
#'
#' @return A 3D \code{\link[plotly]{plot_ly}} plot of cluster sizes in the
#'   specified colorspace for each cluster dataframe provided.
#'
#' @examples
#' \dontrun{
#' # Takes >10 seconds
#' cluster.list <- colordistance::getHistList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), plotting=FALSE,
#' lower=rep(0.8, 3), upper=rep(1, 3))
#'
#' colordistance::plotClusters(cluster.list, p=c(1:3, 7:8), pausing=FALSE)
#'
#' clusterListHSV <- colordistance::getHistList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), hsv=TRUE,
#' plotting=FALSE, lower=rep(0.8, 3), upper=rep(1, 3))
#'
#' colordistance::plotClusters(clusterListHSV, p=c(1:3, 7:8), hsv=TRUE,
#' pausing=FALSE)
#' }
#' @export
plotClusters <- function(cluster.list, color.space = "rgb",
                         p = "all", pausing = TRUE, 
                         ref.white, to = "sRGB") {
  
  color.space <- tolower(color.space)
  # Set plotting parameters
  if (color.space == "hsv") {
    scene <- list(xaxis = list(title = "Hue", range = c(0, 1)),
                  yaxis = list(title = "Saturation", range = c(0, 1)),
                  zaxis = list(title = "Value", range = c(0, 1)), 
                  camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  } else if (color.space == "lab") {
    scene <- list(xaxis = list(title = "Luminance", 
                               range = c(0, 100)),
                  yaxis = list(title = "a (green-red)", 
                               range = c(-127, 128)),
                  zaxis = list(title = "b (blue-yellow)", 
                               range = c(-127, 128)))
  } else {
    scene <- list(xaxis = list(title = "Red", 
                             linecolor = plotly::toRGB("red"),
                             linewidth = 6, range = c(0, 1)),
                  yaxis = list(title = "Green", 
                               linecolor = plotly::toRGB("green"),
                               linewidth = 6,
                               range = c(0, 1)), 
                  zaxis = list(title = "Blue", 
                               linecolor = plotly::toRGB("blue"),
                               linewidth = 6, range = c(0, 1)),
                  camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  }

  # If p is all just create vector for cycling through every element
  if (p[1] == "all") {
    p <- c(1:length(cluster.list))
  }

  if (is.data.frame(cluster.list)) {
    temp <- vector("list", 1)
    temp[[1]] <- cluster.list
    cluster.list <- temp
    p <- 1
  }

  if (length(p) == 1) {
    pausing <- FALSE
  }
  # Plot clusters for each cluster element
  if (is.numeric(p)) {
    for (i in p) {
      if (color.space == "hsv") {
        colExp <- apply(cluster.list[[i]], 1, function(x) hsv(x[1],
                                                             x[2],
                                                             x[3]))
        } else {
          if (color.space == "lab") {
            temp <- cluster.list
            cluster.list <- lapply(cluster.list,
              function(i) suppressMessages(convertColorSpace(i,
              from = "Lab", to = "sRGB", sample.size = "all", 
              from.ref.white = ref.white)))
            colExp <- apply(cluster.list[[i]], 1, function(x) rgb(x[1],
                                                                  x[2],
                                                                  x[3]))
            cluster.list <- temp
          } else {
            colExp <- apply(cluster.list[[i]], 1, function(x) rgb(x[1],
                                                                  x[2],
                                                                  x[3]))
          }
      }

      pl <- plotly::plot_ly(cluster.list[[i]], 
                            x = ~cluster.list[[i]][, 1], 
                            y = ~cluster.list[[i]][, 2], 
                            z = ~cluster.list[[i]][, 3], 
                            size = ~cluster.list[[i]][, 4], 
                            color = ~cluster.list[[i]][, 4])
      pl <- plotly::add_markers(pl, color = I(colExp), 
                                size = ~cluster.list[[i]][, 4], 
                                sizes = c(10, 5000))
      pl <- plotly::layout(pl, scene = scene, 
                           title = names(cluster.list)[i])

      print(pl)

      if (pausing & i < tail(p, 1)) {
        pause()
        }
    }
  } else {
    stop("'p' must be either a numeric vector of indices for plotting",
         " or set to 'all' to plot all sets in cluster list")
    }

}

#' Plot several different cluster sets together
#'
#' Plots cluster sets from several different dataframes on a single plot for
#' easy comparison.
#'
#' @param cluster.list A list of identically sized dataframes with 4 columns each
#'   (R, G, B, Pct or H, S, V, Pct) as output by \code{extractClusters} or
#'   \code{getHistList}.
#' @param p Numeric vector of indices for which elements to plot; otherwise all
#'   of the cluster sets provided will be plotted together.
#' @param color.space The color space (\code{"rgb"}, \code{"hsv"}, or
#'   \code{"lab"}) in which to plot pixels.
#' @param ref.white The reference white passed to
#'   \code{\link{convertColorSpace}}; must be specified if using
#'   \code{color.space = "lab"}.
#' @param to Display color space of image if clustering in CIE Lab space,
#'   probably either "sRGB" or "Apple RGB", depending on your computer.
#' @param title Optional title for the plot.
#'
#' @return A single \code{\link[plotly]{plot_ly}} plot of every cluster in a
#'   list of cluster sets. Each cluster is colored by cluster color,
#'   proportional to cluster size, and labeled according to the image from which
#'   it originated.
#'
#' @note Each cluster plotted is colored according to its actual color, and
#' labeled according to the image from which it originated.
#'
#' @examples
#'
#' \dontrun{
#' # Takes >10 seconds
#' cluster.list <- colordistance::getHistList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), plotting=FALSE,
#' lower=rep(0.8, 3), upper=rep(1, 3))
#'
#' colordistance::plotClustersMulti(cluster.list, p=c(1:4), title="Orange and
#' black Heliconius")
#'
#' colordistance::plotClustersMulti(cluster.list, p=c(5:8), title="Black, yellow,
#' and red Heliconius")
#'
#' clusterListHSV <- colordistance::getHistList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), hsv=TRUE,
#' plotting=FALSE, lower=rep(0.8, 3), upper=rep(1, 3))
#'
#' colordistance::plotClustersMulti(clusterListHSV, p=c(1:3, 7:8), hsv=TRUE)
#' }
#' @export
plotClustersMulti <- function(cluster.list, color.space = "rgb", 
                              p = "all", title = "",
                              ref.white, to = "sRGB") {

  color.space <- tolower(color.space)
  # Set plotting parameters
  if (color.space == "hsv") {
    scene <- list(xaxis = list(title = "Hue", range = c(0, 1)),
                  yaxis = list(title = "Saturation", range = c(0, 1)),
                  zaxis = list(title = "Value", range = c(0, 1)), 
                  camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  } else if (color.space == "lab") {
    scene <- list(xaxis = list(title = "Luminance", 
                               range = c(0, 100)),
                  yaxis = list(title = "a (green-red)", 
                               range = c(-127, 128)),
                  zaxis = list(title = "b (blue-yellow)", 
                               range = c(-127, 128)))
  } else {
    scene <- list(xaxis = list(title = "Red", 
                               linecolor = plotly::toRGB("red"),
                               linewidth = 6, range = c(0, 1)),
                  yaxis = list(title = "Green", 
                               linecolor = plotly::toRGB("green"),
                               linewidth = 6,
                               range = c(0, 1)), 
                  zaxis = list(title = "Blue", 
                               linecolor = plotly::toRGB("blue"),
                               linewidth = 6, range = c(0, 1)),
                  camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))
  }
  
  # If p is set to "all" then use entire list
  if (p[1] == "all") {
    p <- c(1:length(cluster.list))
  }

  # Make new list of just specified values and flatten it
  newList <- vector("list", length(p))

  for (i in c(1:length(p))) {
    newList[[i]] <- cluster.list[[p[i]]]
    names(newList)[i] <- names(cluster.list)[p[i]]
  }

  newDF <- newList[[1]]
  newDF$Image <- rep(names(newList)[1], dim(newList[[1]])[1])

  # Add an "image" ID column for plotting
  for (i in 2:length(newList)) {
    tempDF <- newList[[i]]
    tempDF$Image <- rep(names(newList)[i], dim(newList[[i]])[1])
    newDF <- rbind(newDF, tempDF)
  }

  if (color.space == "hsv") {
    colExp <- apply(newDF[, 1:3], 1, function(x) hsv(x[1], x[2], x[3]))
  } else if (color.space == "lab") {
      colDF <- suppressMessages(convertColorSpace(newDF[, 1:3],
                 from = "Lab", to = "sRGB", sample.size = "all",
                 from.ref.white = ref.white))
      colExp <- apply(colDF, 1, function(x) rgb(x[1], x[2], x[3]))
  } else {
    colExp <- apply(newDF[, 1:3], 1, function(x) rgb(x[1], x[2], x[3]))
    }
  
  pl <- plotly::plot_ly(newDF, x = ~newDF[, 1],
                        y = ~newDF[, 2],
                        z = ~newDF[, 3],
                        size = ~Pct, text = ~paste("Image: ", Image))
  pl <- plotly::add_markers(pl, color = I(colExp), 
                            size = ~Pct, sizes = c(10, 5000))
  pl <- plotly::layout(pl, scene = scene, title = title)
  print(pl)
}

#' Plot a heatmap of a distance matrix
#'
#' Plots a heatmap of a symmetrical distance matrix in order to visualize
#' similarity/dissimilarity in scores. Values are clustered by similarity using
#' \code{\link[stats]{hclust}}.
#'
#' @param clusterList_or_matrixObject Either a list of identically sized
#'   dataframes with 4 columns each (R, G, B, Pct or H, S, V, Pct) as output by
#'   \code{\link{extractClusters}} or \code{\link{getHistList}}, or a
#'   symmetrical distance matrix as output by
#'   \code{\link{getColorDistanceMatrix}}.
#' @param main Title for heatmap plot.
#' @param col Color scale for heatmap from low to high. Default is
#'   \code{colorRampPalette(c("royalblue4", "ghostwhite",
#'   "goldenrod2"))(n=299)}, where yellow is more dissimilar and blue is more
#'   similar.
#' @param margins Margins for column and row labels.
#' @param ... Additional arguments passed on to \code{\link[gplots]{heatmap.2}}.
#'
#' @return Heatmap representation of distance matrix.
#'
#' @examples
#' \dontrun{
#' # Takes a few seconds to run
#' cluster.list <- colordistance::getHistList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), lower=rep(0.8, 3),
#' upper=rep(1, 3))
#'
#' CDM <- colordistance::getColorDistanceMatrix(cluster.list, plotting=FALSE)
#'
#' colordistance::heatmapColorDistance(CDM, main="Heliconius color similarity")
#' colordistance::heatmapColorDistance(cluster.list,
#' col=colorRampPalette(c("red", "cyan", "blue"))(n=299))
#' }
#'
#' @export
heatmapColorDistance <- function(clusterList_or_matrixObject, 
                                 main=NULL, col="default", 
                                 margins=c(6, 8), ...) {

  # Shorter handle
  obj <- clusterList_or_matrixObject

  # If a cluster.list was provided, get the distance matrix; if it"s a matrix,
  # just make the heatmap; if neither, throw an error
  if (is.list(obj)) {
    obj <- getColorDistanceMatrix(obj)
  } else if (!is.matrix(obj)) {
    stop("Argument is not a list (extractClusters or getHistList object)",
         " or a distance matrix (getColorDistanceMatrix object)")
  }

  # The default heatmap colors are genuinely offensive to the eyes so make the
  # default something more palatable; otherwise use user-provided vector
  if (col[1] == "default") {
    col <- colorRampPalette(c("royalblue4", 
                              "ghostwhite", 
                              "goldenrod2"))(n = 299)
  }

  # Convert to "dist" object for hclust method and plot heatmap
  clust <- as.dist(obj)
  gplots::heatmap.2(obj, symm = TRUE, col = col, 
                    Rowv = as.dendrogram(hclust(clust)), main = main,
                    trace = "none", density.info = "none", 
                    key.xlab = "Color distance score", key.title = NA,
                    keysize = 1, revC = T, srtCol = 35, na.color = "grey",
                    margins = margins, offsetRow = 0, offsetCol = 0, ...)

}


#' Color histogram of binned image
#'
#' Plots a color histogram from a dataframe as returned by
#' \code{\link{getImageHist}}, \code{\link{getHistList}}, or
#' \code{\link{extractClusters}}. Bars are colored according to the color of the
#' bin.
#'
#' @param histogram A single dataframe or a list of dataframes as returned by
#'   \code{\link{getImageHist}}, \code{\link{getHistList}}, or
#'   \code{\link{extractClusters}}. First three columns must be color
#'   coordinates and fourth column must be cluster size.
#' @param pausing Logical. Pause and wait for keystroke before plotting the next
#'   histogram?
#' @param hsv Logical. Should provided color coordinates be interpreted as HSV?
#'   If \code{FALSE}, RGB is assumed.
#' @param main Title for plot. If \code{"default"}, the name of the cluster
#'   histogram is used.
#' @param ... Optional arguments passed to the \code{\link[graphics]{barplot}} function.
#'
#' @examples
#' color_df <- as.data.frame(matrix(rep(seq(0, 1, length.out=3), 3), nrow=3,
#' ncol=3))
#'
#' color_df$Pct <- c(0.2, 0.5, 0.3)
#'
#' colordistance::plotHist(color_df, main="Example plot")
#' @export
plotHist <- function(histogram, pausing = TRUE, 
                     hsv = FALSE, main = "default", ...) {
  if (is.null(dim(histogram))) {

    for (i in 1:length(histogram)) {
      clusters <- histogram[[i]]
      if (hsv) {
        colExp <- apply(clusters, 1, function(x) hsv(h = x[1],
                                                     s = x[2],
                                                     v = x[3]))
      } else {
        colExp <- apply(clusters, 1, function(x) rgb(red = x[1],
                                                     green = x[2],
                                                     blue = x[3]))
      }

      if (main == "default") {
        main <- names(histogram)[i]
        }
      barplot(as.vector(clusters[, 4]), col = colExp, main = main, ...)

      if (pausing & i < length(histogram)) {
        pause()
      }
    }
  } else if (is.matrix(histogram) | is.data.frame(histogram)) {

    clusters <- histogram

    if (hsv) {
      colExp <- apply(clusters, 1, function(x) hsv(h = x[1],
                                                   s = x[2],
                                                   v = x[3]))
    } else {
      colExp <- apply(clusters, 1, function(x) rgb(red = x[1],
                                                   green = x[2],
                                                   blue = x[3]))
    }
    barplot(as.vector(clusters[, 4]), col = colExp, ...)
  }
}
