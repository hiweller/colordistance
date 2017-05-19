# This section gets the award for most headaches incurred during production. Congratulations to me.

#' Chi-square distance between vectors
#'
#' Computes the chi-squared distance between each element of a pair of vectors
#' which must be of the same length. Good for comparing color histograms if you
#' don't want to weight by color similarity. Probably hugely redundant; alas.
#'
#' @param a Numeric vector.
#' @param b Numeric vector; must be the same length as a.
#'
#' @return Chi-squared distance, \eqn{(a - b)^2/(a + b)}, between vectors a and
#'   b. If one or both elements are NA/NaN, contribution is counted as a 0.
#' @examples
#' chisqDistance(rnorm(10), rnorm(10))
chisqDistance <- function(a, b) {
  # If vectors aren"t same length throw an error
  if (length(a) != length(b)) {
    stop("Vectors must be of the same length")
    }

  # Otherwise calculate chi-squared distance between each pair of elements in the vectors and return the sum
  v <- sapply(c(1:length(a)), function(x) (a[x] - b[x])^2/(a[x] + b[x]))
  v[is.nan(v)] <- 0
  return(sum(v))
}

#'  Sum of Euclidean distances between color clusters
#'
#'  Calculates the Euclidean distance between each pair of points in two
#'  dataframes as returned by extractClusters or getImageHist and returns the
#'  sum of the distances.
#'
#' @param T1 Dataframe (especially a dataframe as returned by
#'   \code{extractClusters()} or \code{getImageHist()}, but first three columns
#'   must be coordinates).
#' @param T2 Another dataframe like T1.
#'
#' @return Sum of Euclidean distances between each pair of points (rows) in the
#'   provided dataframes.
#' @examples
#' colorDistance(clusterList[[1]], clusterList[[2]])
colorDistance <- function(T1, T2) {
  return(sum(sapply(1:dim(T1)[1], function(x) dist(rbind(T1[x, 1:3], T2[x, 1:3])))))
}

#'  Earth mover's distance between two sets of color clusters
#'
#'  Calculates the
#'  \href{http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/RUBNER/emd.htm}{Earth
#'  mover's distance} (briefly, the amount of work required to move the data
#'  from one distribution to resemble the other distribution, or the amount of
#'  "dirt" you have to shovel weighted by how far you have to shovel it).
#'  Accounts for both color disparity and size disparity. Recommended unless
#'  \code{binAvg} is off for histogram generation.
#'
#' @param T1 Dataframe (especially a dataframe as returned by
#'   \code{link{extractClusters}} or \code{\link{getImageHist}}, but first three columns
#'   must be coordinates).
#' @param T2 Another dataframe like T1.
#'
#' @return Earth mover's distance between the two dataframes (metric of overall
#'   bin similarity for a pair of 3-dimensional histograms).
#'
#' @examples
#' EMDistance(clusterList[[1]], clusterList[[2]])
EMDistance <- function(T1, T2) {
  T1 <- as.matrix(cbind(T1[, 4], T1[, 1:3]))
  T2 <- as.matrix(cbind(T2[, 4], T2[, 1:3]))
  return(emdist::emd(T1, T2))
}


#'  Distance between color clusters with user-specified color/size weights
#'
#'  Distance metric with optional user input for specifying how much the bin
#'  size similarity and color similarity should be weighted when pairing
#'  clusters from different color cluster sets. (For example, if
#'  \code{sizeWeight} = 1 and \code{colorWeight} = 0, two clusters of identical
#'  color but different sizes would not be compared). Use with caution.
#'
#' @param T1 Dataframe (especially a dataframe as returned by
#'   \code{extractClusters} or \code{getImageHist}, but first three columns
#'   must be coordinates).
#' @param T2 Another dataframe like T1.
#' @param ordering Logical. Should clusters by paired in order to minimize
#'   overall distance scores or evaluated in the order given?
#' @param sizeWeight Weight of size similarity in determining overall score and
#'   ordering (if ordering=T).
#' @param colorWeight Weight of color similarity in determining overall score
#'   and ordering (if ordering=T). Color and size weights do not necessarily
#'   have to sum to 1.
#' @return Similarity score based on size and color similarity of each pair of
#'   points in provided dataframes.
#'
#' @examples
#' weightedPairsDistance(clusterList[[1]], clusterList[[2]])
weightedPairsDistance <- function(T1, T2, ordering=F, sizeWeight=0.5, colorWeight=0.5) {
  if (ordering) {
    require(spatstat)
    require(clue)
    im1 <- spatstat::pp3(T1[, 1], T1[, 2], T1[, 3], box3(c(0, 1)))
    im2 <- spatstat::pp3(T2[, 1], T2[, 2], T2[, 3], box3(c(0, 1)))
    colDist <- spatstat::crossdist(im1, im2) / sqrt(3)
    sizeDist <- spatstat::crossdist(spatstat::ppx(T1[, 4]), ppx(T2[, 4])) / t(sapply(c(1:dim(T1)[1]), function(x) T1[x, 4] + T2[, 4]))
    sizeDist[is.nan(sizeDist)] <- 0

    pairMatrix <- sizeWeight*sizeDist + colorWeight*colDist
    pairs <- clue::solve_LSAP(pairMatrix)
    return(sum(pairMatrix[cbind(seq_along(pairs), pairs)]))
  } else {
    # If ordering is FALSE, take only distances between each ordered bin - compare 1 to 1, 2 to 2, etc, regardless of color or size similarity
    colDist <- sum(sapply(c(1:dim(T1)[1]), function(x) dist(rbind(T1[x, 1:3], T2[x, 1:3])) / sqrt(3)))
    sizeDist <- chisqDistance(T1[, 4], T2[, 4])
    return(colDist*colorWeight + sizeDist*sizeWeight)
  }
}

#'  Distance matrix for a list of color cluster sets
#'
#'  Calculates a distance matrix for a list of color cluster sets as returned by
#'  \code{\link{extractClusters}} or \code{\link{getHistList}} based on the
#'  specified distance metric. Each cell represents the distance between a pair
#'  of color cluster sets as measured using either chi-squared distance (cluster
#'  size only), earth mover's distance (size and color), weighted pairs (size
#'  and color with user-specified weights for each), or color distance
#'  (Euclidean distance between clusters as 3-dimensional - RGB or HSV - color
#'  coordinates). Earth mover's distance is recommended unless \code{binAvg} is
#'  set to false during cluster list generation (in which case all paired bins
#'  will have the same colors across datasets), in which case chi-squared is
#'  recommended. Weighted pairs or color distance may be appropriate depending
#'  on the question, but generally give poorer results.
#'
#' @param clusterList A list of identically sized dataframes with 4 columns each
#'   (R, G, B, Pct or H, S, V, Pct) as output by \code{extractClusters} or
#'   \code{getHistList}.
#' @param method One of four possible comparison methods for calculating the
#'   color distances: \code{"emd"} (uses \code{\link{EMDistance}}, recommended),
#'   \code{"chisq"} (uses \code{\link{chisqDistance}}), \code{"color.dist"}
#'   (uses \code{\link{colorDistance}}; not appropriate if binAvg=F), or
#'   \code{"weighted.pairs"} (\code{\link{weightedPairsDistance}}).
#' @param sizeWeight Same as in \code{\link{weightedPairsDistance}}.
#' @param colorWeight Same as in \code{\link{weightedPairsDistance}}.
#' @param ordering Logical if not left as "default". Should the color clusters
#'   in the list be reordered to minimize the distances between the pairs? If
#'   left as default, ordering depends on distance method: "emd" and "chisq" do
#'   not order clusters ("emd" orders on a case-by-case in the
#'   \code{\link{EMDistance}} function itself and reordering by size similarity
#'   would make chi-squared meaningless); "color.dist" and "weighted.pairs" use
#'   ordering. To override defaults, set to either \code{T} (for ordering) or
#'   \code{F} (for no ordering).
#' @param plotting Logical. Should a heatmap of the distance matrix be displayed
#'   once the function finishes running?
#'
#' @return A distance matrix of image distance scores (the scales vary depending
#'   on the distance metric chosen, but for all four methods, higher scores =
#'   more different).
#'
#' @examples
#' # Default values - recommended!
#' getColorDistanceMatrix(clusterList)
#'
#' # Without plotting
#' getColorDistanceMatrix(clusterList, plotting=F)
#'
#' # Use chi-squared instead
#' getColorDistanceMatrix(clusterList, method="chisq")
#'
#' # Override ordering (throws a warning if you're trying to do this with chisq!)
#' getColorDistanceMatrix(clusterList, method="chisq", ordering=TRUE)
#'
#' # Specify high size weight/low color weight for weighted pairs
#' getColorDistanceMatrix(clusterList, method="weighted.pairs", colorWeight=0.1, sizeWeight=0.9)
#'
#' getColorDistanceMatrix(clusterList, method="color.dist", ordering=T)
#' @export
getColorDistanceMatrix <- function(clusterList, method="emd", ordering="default", sizeWeight=0.5, colorWeight=0.5, plotting=T) {

  # First redefine the name of this thing for brevity
  obj <- clusterList

  # If provided object is indeed a list then keep going
  if (is.list(obj)) {

    # If ordering is default, do no reordering for "emd" or "chisq" methods, create an ordered list for "color.dist", or just set ordering to T for weighted.pairs method
    # If ordering is true then just order clusters; if false set ordering to false
    if (ordering=="default") {
      if (method=="color.dist") {
        obj <- orderClusters(obj)
      } else if (method=="weighted.pairs") {
        ordering <- TRUE
        }
    } else if (isTRUE(ordering)) {
      obj <- orderClusters(obj)
      } else {
        ordering <- FALSE
    }

    # Empty distance matrix with row/column names matching list
    distMat <- matrix(data=NA, nrow=length(obj), ncol=length(obj))
    rownames(distMat) <- names(obj)
    colnames(distMat) <- names(obj)

    # Matrix will be symmetrical - save time by calculating only unique pairs (we"ll reflect the values later)
    pairs <- which(lower.tri(distMat, diag=F), arr.ind=T)

    # Use specified method
    # In each case, for every pair defined above, calculate distance metric and insert into distance matrix cell
    if (method=="emd") {
      distMat[pairs] <- apply(pairs, 1, function(x) distMat[x[1], x[2]] <- colordistance::EMDistance(obj[[x[1]]], obj[[x[2]]]))
    } else if (method=="chisq") {

      # Throw a warning if reordering with just chisq method
      if (isTRUE(ordering)) {
        warning("Setting ordering = T for 'chisq' method not recommended (comparisons may not be valid)")
        }

      distMat[pairs] <- apply(pairs, 1, function(x) distMat[x[1], x[2]] <- colordistance::chisqDistance(obj[[x[1]]][, 4], obj[[x[2]]][, 4]))

    } else if (method=="color.dist") {

      distMat[pairs] <- apply(pairs, 1, function(x) distMat[x[1], x[2]] <- colordistance::colorDistance(obj[[x[1]]], obj[[x[2]]]))

    } else if (method=="weighted.pairs") {

      distMat[pairs] <- apply(pairs, 1, function(x) distMat[x[1], x[2]] <- colordistance::weightedPairsDistance(obj[[x[1]]], obj[[x[2]]], ordering=ordering, colorWeight=colorWeight, sizeWeight=sizeWeight))

    } else {
      stop("Comparison method must be one of 'emd', 'chisq', 'color.dist' or 'weighted.pairs'")
      }
  } else {
    stop("Must provide either an extractClusters object or a getHistList object")
    }

  distMat[upper.tri(distMat)] <- t(distMat)[upper.tri(distMat)]

  if (plotting) {
    colordistance::heatmapColorDistance(distMat)
  }

  return(distMat)

}

#' Order color clusters to minimize overall color distance between pairs
#'
#' Reorders clusters to minimize color distance using the
#' \href{https://en.wikipedia.org/wiki/Hungarian_algorithm}{Hungarian algorithm}
#' as implemented by \code{\link[clue]{solve_LSAP}}. Briefly: Euclidean
#' distances between every possible pair of clusters across two dataframes are
#' calculated, and pairs of clusters are chosen in order to minimize the total
#' sum of color distances between the cluster pairs (i.e. A1-B1, A2-B2, etc).
#' For example, if dataframe A has a black cluster, a white cluster, and a blue
#' cluster, in that order, and dataframe B has a white cluster, a blue cluster,
#' and a grey cluster, in that order, the final pairs might be A1-B3 (black and
#' grey), A2-B2 (blue and blue), and A3-B1 (white and white). Rows are reordered
#' so that paired rows are in the same row index (in the example, dataframe B
#' would be reshuffled to go grey, blue, white instead of white, grey, blue).
#'
#' @param extractClustersObject A list of color clusters such as those returned
#'   by \code{\link{extractClusters}} or \code{\link{getHistList}}. List must
#'   contain identically sized dataframes with color coordinates (R, G, B or H,
#'   S, V) as the first three columns.
#'
#' @return A list with identical data to the input list, but with rows in each
#'   dataframe reordered to minimize color distances per cluster pair.
#'
#' @examples
#' orderClusters(extractClusters(getKMeansList("Heliconius/")))
orderClusters <- function(extractClustersObject) {

  endList <- extractClustersObject

  # R trucks in 0-1 scales for pixel intensity rather than 0-255
  # Set bounds to c(0, 1) unless converted
  if (max(extractClustersObject[[1]][, 1:3]) <= 1) {
    box <- c(0, 1)
  } else {
    box <- c(0, 255)
  }

  # Use pp3 function from spatstat package to create 3D point pattern objects for use with solve_LSAP
  imgA <- spatstat::pp3(extractClustersObject[[1]][, 1],
              extractClustersObject[[1]][, 2],
              extractClustersObject[[1]][, 3],
              spatstat::box3(box))

  # Order each subsequent dataframe by the clusters in the first element
  for (i in 2:length(extractClustersObject)) {
    imgB <- spatstat::pp3(extractClustersObject[[i]][, 1],
                extractClustersObject[[i]][, 2],
                extractClustersObject[[i]][, 3],
                spatstat::box3(box))

    # Get distance matrix between the two
    distMatrix <- spatstat::crossdist(imgA, imgB)

    # Use solve_LSAP from clue package to get the minimum sum of distances
    # between point pairs (implements Hungarian Algorithm)
    orders <- clue::solve_LSAP(distMatrix, maximum = FALSE)

    # Reorder the points and insert into list
    endList[[i]] <- extractClustersObject[[i]][orders, ]
  }

  return(endList)

}

#'  Normalize pixel RGB ratios
#'
#'  Converts clusters from raw channel intensity to their fraction of the
#'  intensity for that cluster For example, a bright yellow (R=1, G=1, B=0) and
#'  a darker yellow (R=0.8, G=0.8, B=0) both have 50\% red, 50\% green, and 0\%
#'  blue, so their normalized values would be equivalent. This is a useful
#'  option if your images have a lot of variation in lighting, but obviously
#'  comes at the cost of reducing variation (if darker and lighter colors are
#'  meaningful sources of variation in the dataset).  A similar but less harsh
#'  alternative would be to use HSV rather than RGB for pixel binning and color
#'  similarity clustering by setting \code{hsv=T} in clustering functions and
#'  specifying a low number of 'value' bins (e.g. \code{bins=c(8, 8, 2)}).
#'
#' @param extractClustersObject A list of color clusters such as those returned
#'   by \code{\link{extractClusters}} or \code{\link{getHistList}}. List must
#'   contain identically sized dataframes with color coordinates (R, G, B or H,
#'   S, V) as the first three columns.
#'
#' @return A list of the same size and structure as the input list, but with the
#'   cluster normalized as described.
#'
#' @examples
#' normalizeRGB(extractClusters(getKMeansList("Heliconius/")))
normalizeRGB <- function(extractClustersObject) {
  # For every dataframe in the list, divide each RGB cluster by the total for that row
  return(lapply(extractClustersObject, function(x) x <- cbind(t(apply(x, 1, function(y) y[1:3] / sum(y[1:3]))), Pct=x[, 4])))
}
