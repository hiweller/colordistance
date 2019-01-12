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
#' @param sample.size Number of pixels to be randomly sampled from filtered pixel
#'   array for performing fit. If set to \code{FALSE}, all pixels are fit, but
#'   this can be time-consuming, especially for large images.
#' @param plotting Logical. Should the results of the KMeans fit (original image
#'   + histogram of colors and bin sizes) be plotted?
#' @param lower RGB triplet specifying the lower bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]).
#' @param upper RGB triplet specifying the upper bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]). Determining these bounds may
#'   take some trial and error, but the following bounds may work for certain
#'   common background colors: \itemize{ \item Black: lower=c(0, 0, 0);
#'   upper=c(0.1, 0.1, 0.1) \item White: lower=c(0.8, 0.8, 0.8); upper=c(1, 1,
#'   1) \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1, 0.24) \item Blue:
#'   lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1) } If no background filtering is
#'   needed, set bounds to some non-numeric value (\code{NULL}, \code{FALSE},
#'   \code{"off"}, etc); any non-numeric value is interpreted as \code{NULL}.
#' @param iter.max Inherited from \code{\link[stats]{kmeans}}. The maximum
#'   number of iterations allowed.
#' @param nstart Inherited from \code{\link[stats]{kmeans}}. How many random
#'   sets should be chosen?
#' @param return.clust Logical. Should clusters be returned? If \code{FALSE},
#'   results are plotted but not returned.
#' @param color.space The color space (\code{"rgb"}, \code{"hsv"}, or
#'   \code{"lab"}) in which to cluster pixels.
#' @param from  Display color space of image if clustering in CIE Lab space,
#'   probably either "sRGB" or "Apple RGB", depending on your computer.
#' @param ref.white The reference white passed to
#'   \code{\link{convertColorSpace}}; must be specified if using CIE
#'   Lab space. See \link{convertColorSpace}.
#'
#' @return A \code{\link[stats]{kmeans}} fit object.
#'
#' @examples
#' colordistance::getKMeanColors(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"), n=3,
#' return.clust=FALSE, lower=rep(0.8, 3), upper=rep(1, 3))
#' @export
getKMeanColors <- function(path, n = 10, sample.size = 20000,
                           plotting = TRUE, lower = c(0, 0.55, 0),
                           upper = c(0.24, 1, 0.24), iter.max = 50,
                           nstart = 5, return.clust = TRUE,
                           color.space = "rgb", from = "sRGB", 
                           ref.white) {
  
  color.space <- tolower(color.space)
  
  # Load image, store original image and reshaped pixel matrix separately
  imload <- loadImage(path, lower = lower, upper = upper, hsv = TRUE)
  
  # Use HSV pixels if specified, otherwise stick with RGB
  if (color.space == "hsv") {
    pix <- imload$filtered.hsv.2d
  } else {
    pix <- imload$filtered.rgb.2d
  }
  img <- imload$original.rgb

  # If sample.size is a valid number, randomly select that number of pixels for
  # fitting
  if (is.numeric(sample.size)) {
    if (sample.size <= dim(pix)[1]) {
      # If sample size is smaller than number of pixels in image, randomly
      # select subset
      pix.sample <- pix[sample(nrow(pix), sample.size), ]
    } else {
      # If the image is smaller, just use all of them and throw an alert
      pix.sample <- pix[sample(nrow(pix), nrow(pix)), ]
      message(paste(path, "has fewer than", sample.size, 
                    "pixels; using entire image"))
    }
  } else {
    pix.sample <- pix
    message("Performing fit on all pixels (slow for large images).")
  }
  
  if (color.space == "lab") {
    pix.sample <- convertColorSpace(pix.sample, from = from, 
                             sample.size = "all",
                             from.ref.white = ref.white)
  }

  # Perform KMeans fit on pixels
  suppressWarnings(kmeans.fit <- kmeans(pix.sample, n,
                   iter.max = iter.max, nstart = nstart))

  # If plotting is on, plot original image + color histogram
  if (plotting) {
    # Plot both original image and kmeans in the same window and then return to
    # original parameters
    current.par <- par()

    par(mfrow = c(2, 1), mar = rep(1, 4) + 0.1)
    asp <- dim(img)[1] / dim(img)[2]
    plot(0:1, 0:1, type = "n", axes = FALSE, asp = asp, xlab = "",
         ylab ="")
    title(tail(strsplit(path, "/")[[1]], 1))
    rasterImage(img, 0, 0, 1, 1)

    centers <- kmeans.fit$centers
    if (color.space == "lab") {
      centers <- suppressMessages(convertColorSpace(centers[, 1:3], 
                                   from = "Lab", to = from, 
                                   to.ref.white = ref.white))
    } 
    
    if (color.space == "hsv") {
      rgb.exp <- apply(centers, 1, function(x) hsv(x[1], x[2], x[3]))
    } else {
      rgb.exp <- apply(centers, 1, function(x) rgb(x[1], x[2], x[3]))
    }

    counts <- table(kmeans.fit$cluster, rep("", length(kmeans.fit$cluster)))
    orders <- rev(order(counts[, 1]))
    counts[, 1] <- counts[orders, ]
    rgb.exp <- rgb.exp[orders]
    barplot(counts, col = rgb.exp, axes = FALSE, space = 0,
            border = NA, horiz = T)
    title(paste("KMeans color clusters (", n, " clusters)",
                sep = ""), line = 0)

    par(mfrow = current.par$mfrow, mar = current.par$mar)
  }

  if (return.clust) {
    return(kmeans.fit)
    }
}

#' Get KMeans clusters for every image in a set
#'
#' Performs \code{\link{getKMeanColors}} on every image in a set of images and
#' returns a list of kmeans fit objects, where each dataframe contains the RGB
#' coordinates of the clusters and the percentage of pixels in the image
#' assigned to that cluster.
#'
#' @param images A character vector of directories, image paths, or a
#'   combination of both. Takes either absolute or relative filepaths.
#' @param bins Number of KMeans clusters to fit. Unlike \code{\link{getImageHist}},
#'   this represents the actual final number of bins, rather than the number of
#'   breaks in each channel.
#' @param sample.size Number of pixels to be randomly sampled from filtered pixel
#'   array for performing fit. If set to \code{FALSE}, all pixels are fit, but
#'   this can be time-consuming, especially for large images.
#' @param plotting Logical. Should the results of the KMeans fit (original image
#'   + histogram of colors and bin sizes) be plotted for each image?
#' @param lower RGB triplet specifying the lower bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]).
#' @param upper RGB triplet specifying the upper bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]). Determining these bounds may
#'   take some trial and error, but the following bounds may work for certain
#'   common background colors: \itemize{ \item Black: lower=c(0, 0, 0);
#'   upper=c(0.1, 0.1, 0.1) \item White: lower=c(0.8, 0.8, 0.8); upper=c(1, 1,
#'   1) \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1, 0.24) \item Blue:
#'   lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1) } If no background filtering is
#'   needed, set bounds to some non-numeric value (\code{NULL}, \code{FALSE},
#'   \code{"off"}, etc); any non-numeric value is interpreted as \code{NULL}.
#' @param iter.max Inherited from \code{\link[stats]{kmeans}}. The maximum
#'   number of iterations allowed.
#' @param nstart Inherited from \code{\link[stats]{kmeans}}. How many random
#'   sets should be chosen?
#' @param img.type Logical. Should the image extension (.PNG or .JPG) be retained
#'   in the list names?
#' @param color.space The color space (\code{"rgb"}, \code{"hsv"}, or
#'   \code{"lab"}) in which to cluster pixels.
#' @param from  Original color space of images if clustering in CIE Lab space,
#'   probably either "sRGB" or "Apple RGB", depending on your computer.
#' @param ref.white The reference white passed to
#'   \code{\link{convertColorSpace}}; must be specified if using CIE
#'   Lab space. See \link{convertColorSpace}.
#'
#' @return A list of kmeans fit objects, where the list element names are the
#'   original image names.
#'
#' @examples
#' \dontrun{
#' # Takes a few seconds to run
#' kmeans_list <- colordistance::getKMeansList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), bins=3,
#' lower=rep(0.8, 3), upper=rep(1, 3), plotting=TRUE)
#' }
#' @export
getKMeansList <- function(images, bins = 10, sample.size = 20000,
                          plotting = FALSE, lower = c(0, 0.55, 0),
                          upper = c(0.24, 1, 0.24), iter.max = 50,
                          nstart = 5, img.type = FALSE,
                          color.space = "rgb", from = "sRGB",
                          ref.white) {
  # If argument isn't a string/vector of strings, throw an error
  if (!is.character(images)) {
    stop("'images' argument must be a string (folder containing the",
         " images), a vector of strings (paths to individual images),",
         " or a combination of both")
    }

  im.paths <- c()

  # Extract image paths from any folders
  if (length(which(dir.exists(images))) >= 1) {
    im.paths <- unlist(sapply(images[dir.exists(images)],
                              getImagePaths), use.names = FALSE)
  }

  # For any paths that aren't folders, append to im.paths if they are existing
  # image paths
  # ok this is confusing so to unpack: images[!dir.exists(images)] are all paths
  # that are not directories; then from there we take only ones for which
  # file.exists=TRUE, so we're taking any paths that are not folders but which
  # do exist
  im.paths <- c(im.paths, 
        images[!dir.exists(images)][file.exists(images[!dir.exists(images)])])

  # grab only valid image types (jpegs and pngs)
  im.paths <- im.paths[grep(x = im.paths, 
                            pattern = "[.][jpg.|jpeg.|png.]",
                            ignore.case = TRUE)]

  # Now that we have our list of valid images, get kmeans fit objects for each
  # one
  end.list <- vector("list", length(im.paths))

  # Use a progress bar
  pb <- txtProgressBar(min = 0, max = length(im.paths), style = 3)
  for (i in 1:length(im.paths)) {
    end.list[[i]] <- suppressMessages(getKMeanColors(im.paths[i], 
                                    n = bins, sample.size = sample.size, 
                                    plotting = plotting,
                                    lower = lower, upper = upper,
                                    iter.max = iter.max, nstart = nstart, 
                                    color.space = color.space,
                                    return.clust = TRUE, from = from,
                                    ref.white = ref.white))
    setTxtProgressBar(pb, i)
  }

  # Get just image names (not entire filepath) as labels for list
  listNames <- basename(im.paths)

  # Unless the img.type flag = T, drop the file extension from the labels
  if (!img.type) {
    listNames <- sapply(listNames, function(x) strsplit(x, "[.]")[[1]][1])
  }

  names(end.list) <- listNames

  # And give it back!
  return(end.list)
}

#' Extract cluster values and sizes from kmeans fit objects
#'
#' Extract a list of dataframes with the same format as those returned by
#' \code{\link{getHistList}}, where each dataframe has 3 color attributes (R, G,
#' B or H, S, V) and a size attribute (Pct) for every cluster.
#'
#' @param getKMeansListObject A list of \code{\link[stats]{kmeans}} fit objects
#'   (especially as returned by getKMeansList).
#' @param ordering Logical. Should clusters by reordered by color similarity? If
#'   \code{TRUE}, the Hungarian algorithm via \code{\link[clue]{solve_LSAP}} is
#'   applied to find the minimum sum of Euclidean distances between color pairs
#'   for every pair of cluster objects and colors are reordered accordingly.
#' @param normalize Logical. Should each cluster be normalized to show R:G:B or
#'   H:S:V ratios rather than absolute values? Can be helpful for inconsistent
#'   lighting, but reduces variation. See \code{\link{normalizeRGB}}.
#'
#' @return A list of dataframes (same length as input list), each with 4
#'   columns: R, G, B (or H, S, V) and Pct (cluster size), with one row per
#'   cluster.
#'
#' @note Names are inherited from the list passed to the function.
#'
#' @examples
#' clusterList <- colordistance::getKMeansList(system.file("extdata",
#' "Heliconius/Heliconius_A", package="colordistance"), bins=3)
#'
#' colordistance::extractClusters(clusterList)
#' @export
extractClusters <- function(getKMeansListObject, 
                            ordering=TRUE, normalize=FALSE) {

  if (class(getKMeansListObject) != "kmeans") {
    # Extract cluster size and centers
    end.list <- lapply(getKMeansListObject, 
                       function(x) data.frame(R = x$centers[, 1], 
                                              G = x$centers[, 2], 
                                              B = x$centers[, 3], 
                                     Pct = x$size / sum(x$size)))

    # Retain names
    names(end.list) <- names(getKMeansListObject)

    # Reorder clusters if ordering=TRUE
    if (ordering) {
      end.list <- orderClusters(end.list)
    }
  } else {
    end.list <- data.frame(R = getKMeansListObject$centers[, 1], 
                           G = getKMeansListObject$centers[, 2], 
                           B = getKMeansListObject$centers[, 3], 
                Pct = getKMeansListObject$size / sum(getKMeansListObject$size))
  }

  # Same with normalization
  if (normalize) {
    end.list <- normalizeRGB(end.list)
  }

  return(end.list)

}
