#' Generate a 3D histogram based on color distribution in an image
#'
#' Computes a histogram in either RGB or HSV colorspace by sorting pixels into a
#' specified number of bins.
#'
#' @param image Path to a valid image (PNG or JPG) or a \code{\link{loadImage}}
#'   object.
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. Bins=3 will result in 3^3 = 27 bins; bins=c(2, 2, 3) will
#'   result in 2*2*3=12 bins (2 red, 2 green, 3 blue), etc.
#' @param bin.avg Logical. Should the returned color clusters be the average of
#'   the pixels in that bin (bin.avg=\code{TRUE}) or the center of the bin
#'   (\code{FALSE})? If a bin is empty, the center of the bin is returned as the
#'   cluster color regardless.
#' @param defaultClusters Optional dataframe of default color clusters to be
#'   returned when a bin is empty. If \code{NULL}, the geometric centers of the
#'   bins are used.
#' @param lower RGB or HSV triplet specifying the lower bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]).
#' @param upper RGB or HSV triplet specifying the upper bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]). Determining these bounds may
#'   take some trial and error, but the following bounds may work for certain
#'   common background colors: \itemize{ \item Black: lower=c(0, 0, 0);
#'   upper=c(0.1, 0.1, 0.1) \item White: lower=c(0.8, 0.8, 0.8); upper=c(1, 1,
#'   1) \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1, 0.24) \item Blue:
#'   lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1) } If no background filtering is
#'   needed, set bounds to some non-numeric value (\code{NULL}, \code{FALSE},
#'   \code{"off"}, etc); any non-numeric value is interpreted as \code{NULL}.
#' @param alpha.channel Logical. If available, should alpha channel transparency be
#'   used to mask background? See \code{\link{removeBackground}} for more
#'   details.
#' @param as.vec Logical. Should the bin sizes just be returned as a vector?
#'   Much faster if only using \code{\link{chisqDistance}} for comparison metric.
#' @param norm.pix Logical. Should RGB or HSV cluster values be normalized using
#'   \code{\link{normalizeRGB}}?
#' @param plotting Logical. Should a histogram of the bin colors and sizes be
#'   plotted?
#' @param hsv Logical. Should HSV be used instead of RGB?
#' @param title String for what to title the plots if plotting is on; defaults
#'   to the image name.
#' @param bounds Upper and lower limits for the channels; R reads in images with
#'   intensities on a 0-1 scale, but 0-255 is common.
#' @param ... Optional arguments passed to the \code{\link[graphics]{barplot}} function.
#'
#' @return A vector or dataframe (depending on whether \code{as.vec=T}) of bin
#'   sizes and color values.
#'
#' @details If you choose 2 bins for each color channel, then each of R, G, and
#' B will be divided into 2 bins each, for a total of 2^3 = 8 bins.
#'
#' Once all pixels have been binned, the function will return either the size of
#' each bin, either in number of pixels or fraction of total pixels, and the
#' color of each bin, either as the geometric center of the bin or as the
#' average color of all pixels assigned to it.
#'
#' For example, if you input an image of a red square and used 8 bins, all red
#' pixels (RGB triplet of [1, 0, 0]) would be assigned to the bin with R bounds
#' (0.5, 1], G bounds [0, 0.5) and B bounds [0, 0.5). The average color of the
#' bin would be [0.75, 0.25, 0.25], but the average color of the pixels assigned
#' to that bin would be [1, 0, 0]. The latter option is obviously more
#' informative, but takes longer (about 1.5-2x longer depending on the images).
#'
#' @examples
#' # generate HSV histogram for a single image
#' colordistance::getImageHist(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"),
#' upper=rep(1, 3), lower=rep(0.8, 3), bins=c(8, 3, 3), hsv=TRUE, plotting=TRUE)
#'
#' # generate RGB histogram
#' colordistance::getImageHist(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"),
#' upper=rep(1, 3), lower=rep(0.8, 3), bins=2)
#' @export
getImageHist <- function(image, bins = 3, bin.avg = TRUE,
                         defaultClusters = NULL, lower = c(0, 0.55, 0),
                         upper = c(0.24, 1, 0.24), as.vec = FALSE,
                         alpha.channel = TRUE,
                         norm.pix = FALSE, plotting = TRUE, hsv = FALSE,
                         title = "path", bounds = c(0, 1), ...) {
  
  message("RGB and HSV are device-dependent,", 
          " perceptually non-uniform color spaces.",
          " See 'Color spaces' vignette for more information.\n")

  # If filepath was provided, check to make sure it exists or throw an error
  if (is.character(image)) {
    if (file.exists(image)) {
      image <- loadImage(image, lower = lower, upper = upper, hsv = hsv,
                         alpha.channel = alpha.channel)
    }
  } else if (!is.list(image)) {
    stop("'image' must be either a path to the image", 
         " or a list object returned by loadImage")
    }

  # If hsv=T, use hsv column
  if (hsv) {
    if (exists("filtered.hsv.2d", where = image)) {
      pix <- image$filtered.hsv.2d
    } else {
      stop("'image' does not have a valid HSV element")
      }
  } else {
    pix <- image$filtered.rgb.2d
    }

  # Create vector of bins
  if (length(bins) == 1 | length(bins) == 3) {
    if (length(bins) == 1) {
      message(paste("\nUsing ", bins, "^3 = ", 
                    paste(bins ^ 3), " total bins", sep = ""))
      bins <- rep(bins, 3)
    } else {
      message(paste("\nUsing ", bins[1], "*", bins[2], "*", bins[3],
                    " = ", bins[1] * bins[2] * bins[3], " bins", sep = ""))
    }
    breaks <- lapply(bins + 1, 
                function(x) seq(bounds[1], bounds[2], length = x))
  } else {
    stop("Bins must be a numeric vector of length 1 or length 3")
    }

  # Bin all the channels
  binnedImage <- data.frame(
    R = cut(pix[, 1], breaks = breaks[[1]], include.lowest = T, labels = F),
    G = cut(pix[, 2], breaks = breaks[[2]], include.lowest = T, labels = F),
    B = cut(pix[, 3], breaks = breaks[[3]], include.lowest = T, labels = F)
  )

  # Unless defaultClusters dataframe is provided, just make a dataframe where
  # each default cluster color is the center of the bin
  # I know this line is confusing
  # Don't worry about that
    if (is.null(defaultClusters)) {
        defaultClusters <- as.matrix(expand.grid(lapply(breaks,
                           function(i) sapply(c(1:(length(i) - 1)),
                           function(j) mean(c(i[j], i[j + 1]))))))

        # Set clusters as defaultClusters - values only overwritten if there are
        # pixels in that bin, otherwise center is cluster value and Pct is 0
        clusters <- as.data.frame(defaultClusters)

        if (hsv) {
          colnames(clusters) <- c("h", "s", "v")
        } else {
          colnames(clusters) <- c("r", "g", "b")
        }
    }

  # If bin.avg is flagged and as.vec is off, return a matrix with average color
  # of all pixels assigned to each bin and the percentage of pixels in that bin
  # (if none, retain bin center as cluster color)
  # If bin.avg is flagged by as.vec is TRUE, don't waste time calculating the
  # average bin color, since it won't be returned anyways
    if (bin.avg & !as.vec) {

    # Get list of all filled bins + their sizes
    d <- mgcv::uniquecombs(binnedImage, T)
    ind <- attr(d, "index")

    clusters$Pct <- rep(0, dim(clusters)[1])

    # Possible bins for comparison with assigned indices
    possibleBins <- expand.grid(c(1:bins[1]), c(1:bins[2]), c(1:bins[3]))

    # For every identified cluster, get the average R, G, and B values
    # If only one pixel was assigned there (unlikely, but, you know), just plop
    # that pixel in as the average color
    for (j in 1:dim(d)[1]) {
      pixTemp <- pix[which(ind == j), ]
      if (is.matrix(pixTemp)) {
        clusters[which(apply(possibleBins, 1, 
          function(x) all(x == d[j, ]))), 1:3] <- apply(pixTemp, 2, mean)
        clusters[which(apply(possibleBins, 1, 
          function(x) all(x == d[j, ]))), 4] <- dim(pixTemp)[1] / dim(pix)[1]
      } else {
        clusters[which(apply(possibleBins, 1,
          function(x) all(x == d[j, ]))), 1:3] <- pixTemp
        clusters[which(apply(possibleBins, 1, 
          function(x) all(x == d[j, ]))), 4] <- length(pixTemp) / dim(pix)[1]
      }
    }
  } else {

    # Count up pixels per bin without keeping track of which pixel was assigned
    # to which bin
    Pct <- as.vector(xtabs(~ ., binnedImage))
    clusters$Pct <- Pct / max(sum(Pct))

  }

  if (!as.vec & norm.pix) {
    # Normalize cluster coordinates if flagged
    if (norm.pix) {
      clusters <- cbind(t(apply(clusters, 1, 
                  function(x) x[1:3] / sum(x[1:3]))), clusters$Pct)
    }
  }

  # Plot if flagged
  if (plotting) {
    if (!is.vector(clusters)) {
      pixelBins <- as.vector(clusters[, 4])
    } else {
        pixelBins <- clusters
        }
    if (bin.avg) {
      if (hsv) {
        colExp <- apply(clusters, 1, 
                  function(x) grDevices::hsv(h = x[1],
                                             s = x[2], 
                                             v = x[3]))
      } else {
        colExp <- apply(clusters, 1, 
                  function(x) grDevices::rgb(red = x[1], 
                                             green = x[2], 
                                             blue = x[3]))
        }
    } else {
      colExp <- getHistColors(bins, hsv = hsv)
      }
    if (title == "path") {
      title <- strsplit(tail(strsplit(image$path, "/")[[1]], 1), "[.]")[[1]][1]
      }
    barplot(as.vector(pixelBins), col = colExp, main = title, ...)
  }

  if (as.vec) {
    clusters <- clusters$Pct
  }

  return(clusters)

}

#' Vector of hex colors for histogram bin coloration
#'
#' Gets a vector of colors for plotting histograms from
#' \code{\link{getImageHist}} in helpful ways.
#'
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. Bins = 3 will result in 3^3 = 27 bins; bins = c(2, 2, 3) will
#'   result in 2 * 2 * 3 = 12 bins (2 red, 2 green, 3 blue), etc.
#' @param hsv Logical. Should HSV be used instead of RGB?
#'
#' @return A vector of hex codes for bin colors.
#'
#' @examples
#' colordistance:::getHistColors(bins = 3)
#' colordistance:::getHistColors(bins = c(8, 3, 3), hsv = TRUE)
getHistColors <- function(bins, hsv = FALSE) {

  # If only 1 number given, use that number of bins for each channel
  if (length(bins) == 1) {
    bins <- rep(bins, 3)
  }

  breaks <- lapply(bins, function(x) seq(0, 1, by = 1 / x))
  outlist <- vector("list", length = length(breaks))

  # Make a list of the bin midpoints in each channel
  for (i in 1:length(breaks)) {
    value <- c()
    for (j in 1:(length(breaks[[i]]) - 1)) {
      value <- c(value, mean(c(breaks[[i]][j], breaks[[i]][j + 1])))
    }
    outlist[[i]] <- value
  }

  # Get every possible combination of the above midpoints to get the cube
  # centers
  d <- as.matrix(expand.grid(outlist))

  # And convert each of those possible centers into colors using either hsv or
  # rgb
  if (hsv) {
    colExp <- apply(d, 1, function(x) hsv(h = x[1],
                                          s = x[2],
                                          v = x[3]))
  } else {
    colExp <- apply(d, 1, function(x) rgb(red = x[1],
                                          green = x[2],
                                          blue = x[3]))
  }

  return(colExp)

}

#' Generate a list of cluster sets for multiple images
#'
#' Applies \code{\link{getImageHist}} to every image in a provided set of image
#' paths and/or directories containing images.
#'
#' @param images Character vector of directories, image paths, or both.
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. Bins=3 will result in 3^3 = 27 bins; bins=c(2, 2, 3) will
#'   result in 2*2*3=12 bins (2 red, 2 green, 3 blue), etc.
#' @param bin.avg Logical. Should the returned color clusters be the average of
#'   the pixels in that bin (bin.avg=\code{TRUE}) or the center of the bin
#'   ({FALSE})? If a bin is empty, the center of the bin is returned as the
#'   cluster color regardless.
#' @param lower RGB or HSV triplet specifying the lower bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]).
#' @param upper RGB or HSV triplet specifying the upper bounds for background
#'   pixels. Default upper and lower bounds are set to values that work well for
#'   a bright green background (RGB [0, 1, 0]). Determining these bounds may
#'   take some trial and error, but the following bounds may work for certain
#'   common background colors: \itemize{ \item Black: lower=c(0, 0, 0);
#'   upper=c(0.1, 0.1, 0.1) \item White: lower=c(0.8, 0.8, 0.8); upper=c(1, 1,
#'   1) \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1, 0.24) \item Blue:
#'   lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1) } If no background filtering is
#'   needed, set bounds to some non-numeric value (\code{NULL}, \code{FALSE},
#'   \code{"off"}, etc); any non-numeric value is interpreted as \code{NULL}.
#' @param alpha.channel Logical. If available, should alpha channel transparency be
#'   used to mask background? See \code{\link{removeBackground}} for more
#'   details.
#' @param norm.pix Logical. Should RGB or HSV cluster values be normalized using
#'   \code{\link{normalizeRGB}}?
#' @param plotting Logical. Should the histogram generated for each image be
#'   displayed?
#' @param pausing Logical. If \code{plotting=T}, should the function pause
#'   between graphing and wait for user to hit \code{[enter]} before continuing?
#'   Useful for data/histogram inspection.
#' @param hsv Logical. Should HSV be used instead of RGB?
#' @param title String for what the title the plots if plotting is on; defaults
#'   to the image name.
#' @param img.type Logical. Should the file extension for the images be retained
#'   when naming the output list elements? If \code{FALSE}, just the image name
#'   is used (so "Heliconius_01.png" becomes "Heliconius_01").
#' @param bounds Upper and lower limits for the channels; R reads in images with
#'   intensities on a 0-1 scale, but 0-255 is common.
#'
#' @return A list of \code{\link{getImageHist}} dataframes, 1 per image, named
#'   by image name.
#'
#' @note For every image, the pixels are binned according to the specified bin
#' breaks. By providing the bounds for the bins rather than letting an algorithm
#' select centers (as in \code{\link{getKMeansList}}), clusters of nearly
#' redundant colors are avoided.
#'
#' So you don't end up with, say, 3 nearly-identical yellow clusters which are
#' treated as unrelated just because there's a lot of yellow in your image; you
#' just get a very large yellow cluster and empty non-yellow bins.
#'
#' @examples
#' \dontrun{
#' # Takes >10 seconds if you run all examples
#' clusterList <- colordistance::getHistList(system.file("extdata",
#' "Heliconius/Heliconius_B", package="colordistance"), upper = rep(1, 3),
#' lower = rep(0.8, 3))
#'
#' clusterList <- colordistance::getHistList(c(system.file("extdata",
#' "Heliconius/Heliconius_B", package="colordistance"), system.file("extdata",
#' "Heliconius/Heliconius_A", package="colordistance")), pausing = FALSE,
#' upper = rep(1, 3), lower = rep(0.8, 3))
#'
#' clusterList <- colordistance::getHistList(system.file("extdata",
#' "Heliconius/Heliconius_B", package = "colordistance"), plotting = TRUE,
#' upper = rep(1, 3), lower = rep(0.8, 3))
#' }
#'
#' @export
getHistList <- function(images, bins = 3, bin.avg = TRUE, 
                        lower = c(0, 0.55, 0), upper = c(0.24, 1, 0.24),
                        alpha.channel = TRUE,
                        norm.pix = FALSE, plotting = FALSE, pausing = TRUE, 
                        hsv = FALSE, title = "path", img.type = FALSE, 
                        bounds = c(0, 1)) {
  
  warning("RGB and HSV are device-dependent, perceptually non-uniform color", 
          " spaces. See 'Color spaces' vignette for more information.\n")
  
  # If argument isn't a string/vector of strings, throw an error
  if (!is.character(images)) {
    stop("'images' argument must be a string (folder containing the images),", 
         " a vector of strings (paths to individual images),",
         " or a combination of both")
    }

  im.paths <- c()

  # Extract image paths from any folders
  if (length(which(dir.exists(images))) >= 1) {
    im.paths <- unlist(sapply(images[dir.exists(images)], 
                colordistance::getImagePaths), use.names = FALSE)
  }

  # For any paths that aren't folders, append to im.paths if they are existing
  # image paths
  # ok this is confusing so to unpack: images[!dir.exists(images)] are all paths
  # that are not directories; then from there we take only ones for which
  # file.exists=TRUE, so we're taking any paths that are not folders but which
  # do exist
  im.paths <- c(im.paths, 
        images[!dir.exists(images)][file.exists(images[!dir.exists(images)])])

  # Grab only valid image types (jpegs and pngs)
  im.paths <- im.paths[grep(x = im.paths, 
                            pattern = "[.][jpg.|jpeg.|png.]",
                            ignore.case = TRUE)]

  # Before we embark on this lengthy journey, make sure bins argument is valid
  # Convert bins to a vector of length 3 or throw an error if bins argument is
  # not valid
  if (length(bins) == 1 | length(bins) == 3) {
    if (length(bins) == 1) {
      message(paste("Using ", bins, "^3 = ", paste(bins ^ 3),
                    " total bins", sep = ""))
      bins <- rep(bins, 3)
    } else {
      message(paste("Using ", bins[1], "*", bins[2], "*",
                    bins[3], " = ", bins[1] * bins[2] * bins[3],
                    " bins", sep = ""))
    }
  } else {
    stop("Bins must be a numeric vector of length 1 or length 3")
    }

  if (length(im.paths) == 0) {
    stop("No images found")
  }
  # Empty list for histogram output
  end.list <- vector("list", length(im.paths))

  # If pausing is on (and plotting is also on because otherwise this is
  # pointless), fill in one element at a time, plot it, then wait for user input

  # Display progress bar
  pb <- txtProgressBar(min = 0, max = length(im.paths), style = 3)

  for (i in 1:length(end.list)) {
    end.list[[i]] <- suppressMessages(getImageHist(im.paths[i], bins = bins,
                      bin.avg = bin.avg, lower = lower, upper = upper, 
                      norm.pix = norm.pix, plotting = plotting, 
                      hsv = hsv, title = title, bounds = bounds))

    setTxtProgressBar(pb, i)

    if (pausing & plotting & i < length(end.list)) {
      pause()
    }

  }

  # Name each list element by image name and include file extension if
  # img.type is TRUE
  name.paths <- basename(im.paths)
  if (img.type) {
    names(end.list) <- name.paths
  } else {
    names(end.list) <- sapply(name.paths, 
                      function(x) strsplit(x, split = "[.]")[[1]][1])
  }

  return(end.list)

}
