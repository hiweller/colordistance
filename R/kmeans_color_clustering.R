#' Fit pixels to clusters using KMeans clustering
#'
#' Uses \href{https://en.wikipedia.org/wiki/K-means_clustering}{KMeans
#' clustering} to determine color clusters that minimize the sum of distances
#' between pixels and their assigned clusters. Useful for parsing common color
#' motifs in an object.
#'
#' @param path Path to an image (JPG or PNG).
#' @param n Number of KMeans clusters to fit. Unlike \code{\link{getImageHist}},
#'   this represents the actual final number of bins, rather than the number of
#'   breaks in each channel.
#' @param sampleSize Number of pixels to be randomly sampled from filtered pixel array for performing fit. If set to \code{FALSE}, all pixels are fit, but this can be time-consuming, especially for large images.
#' @param plotting Logical. Should the results of the KMeans fit (original image + histogram of colors and bin sizes) be plotted?
#' @param lower RGB or HSV triplet specifying the lower bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]).
#' @param upper RGB or HSV triplet specifying the upper bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]). Determining these bounds may
#'   take some trial and error, but the following bounds may work for certain
#'   common background colors:
#' \itemize{
#' \item Black: lower=c(0, 0, 0); upper=c(0.1, 0.1, 0.1)
#' \item White: lower=c(0.8, 0.8, 0.8); upper=c(1, 1, 1)
#' \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1, 0.24)
#' \item Blue: lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1)
#' }
#' If no background filtering is needed, set bounds to some non-numeric value
#' (\code{NULL}, \code{FALSE}, \code{"off"}, etc); any non-numeric value is
#' interpreted as \code{NULL}.
#' @param iter.max Inherited from \code{\link[stats]{kmeans}}. The maximum number of iterations allowed.
#' @param nstart Inherited from \code{\link[stats]{kmeans}}. How many random sets should be chosen?
#' @param returnClust Logical. Should clusters be returned? If \code{FALSE}, results are plotted but not returned.
#'
#' @return A \code{\link[stats]{kmeans}} fit object.
#'
#' @examples
#' getKMeanColors("Heliconius/Heliconius_01.png", n=15, returnClust=F)
#' @export
getKMeanColors <- function(path, n=10, sampleSize=20000, plotting=T, lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), iter.max=50, nstart=5, returnClust=T) {
  # Load image, store original image and reshaped pixel matrix separately
  imload <- loadImage(path, lower=lower, upper=upper)
  img <- imload$original.rgb
  pix <- imload$filtered.rgb.2d

  # If sampleSize is a valid number, randomly select that number of pixels for fitting
  if (is.numeric(sampleSize)) {
    if (sampleSize <= dim(pix)[1]) {
      # If sample size is smaller than number of pixels in image, randomly select subset
      pixSample <- pix[sample(nrow(pix), sampleSize), ]
    } else {
      # If the image is smaller, just use all of them and throw an alert
      pixSample <- pix[sample(nrow(pix), nrow(pix)), ]
      message(paste(path, "has fewer than", sampleSize, "pixels; using entire image"))
    }
  } else {
    pixSample <- pix
    message("Performing fit on all pixels (slow for large images).")
  }

  # Perform KMeans fit on pixels
  suppressWarnings(kmeansFit <- kmeans(pixSample, n, iter.max=iter.max, nstart=nstart))

  # If plotting is on, plot original image + color histogram
  if (plotting) {
    # Plot both original image and kmeans in the same window and then return to original parameters
    currentPar <- par()

    par(mfrow=c(2, 1), mar=rep(1, 4) + 0.1)
    asp <- dim(img)[1]/dim(img)[2]
    plot(0:1, 0:1, type="n", axes=F, asp=asp)
    title(tail(strsplit(path, "/")[[1]], 1))
    rasterImage(img, 0, 0, 1, 1)

    centers <- kmeansFit$centers
    rgbExp <- apply(centers, 1, function(x) rgb(x[1], x[2], x[3]))

    counts <- table(kmeansFit$cluster, rep("", length(kmeansFit$cluster)))
    orders <- rev(order(counts[, 1]))
    counts[, 1] <- counts[orders, ]
    rgbExp <- rgbExp[orders]
    barplot(counts, col=rgbExp, axes=F, space=0, border=NA, horiz=T, asp=5000)
    title(paste("KMeans color clusters (", n, " clusters)", sep=""), line=-1)

    par(mfrow=currentPar$mfrow, mar=currentPar$mar)
  }

  if (returnClust) {
    return(kmeansFit)
    }
}

#' Get KMeans clusters for every image in a set
#'
#' Performs \code{\link{getKMeanColors}} on every image in a set of images and returns a list of kmeans fit objects, where each dataframe contains the RGB coordinates of the clusters and the percentage of pixels in the image assigned to that cluster.
#'
#' @param images A character vector of directories, image paths, or a combination of both. Takes either absolute or relative filepaths.
#' @param n Number of KMeans clusters to fit. Unlike \code{\link{getImageHist}},
#'   this represents the actual final number of bins, rather than the number of
#'   breaks in each channel.
#' @param sampleSize Number of pixels to be randomly sampled from filtered pixel
#'   array for performing fit. If set to \code{FALSE}, all pixels are fit, but
#'   this can be time-consuming, especially for large images.
#' @param plotting Logical. Should the results of the KMeans fit (original image
#'   + histogram of colors and bin sizes) be plotted for each image?
#' @param lower RGB or HSV triplet specifying the lower bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]).
#' @param upper RGB or HSV triplet specifying the upper bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]). Determining these bounds may
#'   take some trial and error, but the following bounds may work for certain
#'   common background colors:
#' \itemize{
#' \item Black: lower=c(0, 0, 0); upper=c(0.1, 0.1, 0.1)
#' \item White: lower=c(0.8, 0.8, 0.8); upper=c(1, 1, 1)
#' \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1, 0.24)
#' \item Blue: lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1)
#' }
#' If no background filtering is needed, set bounds to some non-numeric value
#' (\code{NULL}, \code{FALSE}, \code{"off"}, etc); any non-numeric value is
#' interpreted as \code{NULL}.
#' @param iter.max Inherited from \code{\link[stats]{kmeans}}. The maximum
#'   number of iterations allowed.
#' @param nstart Inherited from \code{\link[stats]{kmeans}}. How many random
#'   sets should be chosen?
#' @param imgType Logical. Should the image extension (.PNG or .JPG) be retained
#'   in the list names?
#'
#' @return A list of kmeans fit objects, where the list element names are the
#'   original image names.
#'
#' @examples
#' getKMeansList(c("Heliconius/", "Chaetodontidae/Chaetodon_01.png"),
#' plotting=T, sampleSize=10000, n=20)
#' @export
getKMeansList <- function(images, bins=10, sampleSize=20000, plotting=F, lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), iter.max=50, nstart=5, imgType=F) {
  # If argument isn"t a string/vector of strings, throw an error
  if (!is.character(images)) {
    stop("'images' argument must be a string (folder containing the images), a vector of strings (paths to individual images), or a combination of both")
    }

  imPaths <- c()

  # Extract image paths from any folders
  if (length(which(dir.exists(images)))>=1) {
    imPaths <- unlist(sapply(images[dir.exists(images)], getImagePaths), use.names=F)
  }

  # For any paths that aren"t folders, append to imPaths if they are existing image paths
  # ok this is confusing so to unpack: images[!dir.exists(images)] are all paths that are not directories; then from there we take only ones for which file.exists=TRUE, so we"re taking any paths that are not folders but which do exist
  imPaths <- c(imPaths, images[!dir.exists(images)][file.exists(images[!dir.exists(images)])])

  # grab only valid image types (jpegs and pngs)
  imPaths <- imPaths[grep(x=imPaths, pattern="[.][jpg.|jpeg.|png.]", ignore.case=T)]

  # Now that we have our list of valid images, get kmeans fit objects for each one
  endList <- vector("list", length(imPaths))

  # Use a progress bar
  pb <- txtProgressBar(min=0, max=length(imPaths), style=3)
  for (i in 1:length(imPaths)) {
    endList[[i]] <- getKMeanColors(imPaths[i], n=bins, sampleSize=sampleSize, plotting=plotting, lower=lower, upper=upper, iter.max=iter.max, nstart=nstart, returnClust=T)
    setTxtProgressBar(pb, i)
  }

  # Get just image names (not entire filepath) as labels for list
  listNames <- sapply(imPaths, function(x) tail(strsplit(x, "/")[[1]], 1))

  # Unless the imgType flag = T, drop the file extension from the labels
  if (!imgType) {
    listNames <- sapply(listNames, function(x) head(strsplit(x, "[.]")[[1]], 1))
  }

  names(endList) <- listNames

  # And give it back!
  return(endList)
}

#' Extract cluster values and sizes from kmeans fit objects
#'
#' Extract a list of dataframes with the same format as those returned by \code{\link{getHistList}}, where each dataframe has 3 color attributes (R, G, B or H, S, V) and a size attribute (Pct) for every cluster. Names are inherited from the list passed to the function.
#'
#' @param getKMeansListObject A list of \code{\link[stats]{kmeans}} fit objects (especially as returned by getKMeansList).
#' @param ordering Logical. Should clusters by reordered by color similarity? If \code{TRUE}, the Hungarian algorithm via \code{\link[clue]{solve_LSAP}} is applied to find the minimum sum of Euclidean distances between color pairs for every pair of cluster objects and colors are reordered accordingly.
#' @param normalize Logical. Should each cluster be normalized to show R:G:B or H:S:V ratios rather than absolute values? Can be helpful for inconsistent lighting, but reduces variation. See \code{\link{normalizeRGB}}.
#'
#' @return A list of dataframes (same length as input list), each with 4 columns: R, G, B (or H, S, V) and Pct (cluster size), with one row per cluster.
#'
#' @examples
#' extractClusters(getKMeansList("Heliconius/"))
#' @export
extractClusters <- function(getKMeansListObject, ordering=T, normalize=F) {

  if (class(getKMeansListObject)!="kmeans") {
    # Extract cluster size and centers
    endList <- lapply(getKMeansListObject, function(x) data.frame(R=x$centers[, 1], G=x$centers[, 2], B=x$centers[, 3], Pct=x$size/sum(x$size)))

    # Retain names
    names(endList) <- names(getKMeansListObject)

    # Reorder clusters if ordering=TRUE
    if (ordering) {
      endList <- orderClusters(endList)
    }
  } else {
    endList <- data.frame(R=getKMeansListObject$centers[, 1], G=getKMeansListObject$centers[, 2], B=getKMeansListObject$centers[, 3], Pct=getKMeansListObject$size/sum(getKMeansListObject$size))
  }

  # Same with normalization
  if (normalize) {
    endList <- normalizeRGB(endList)
  }

  return(endList)

}
