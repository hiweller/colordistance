#' Generate a 3D histogram based on CIE Lab color coordinates in an image
#' 
#' Computes a histogram in CIE Lab color space by sorting pixels into specified bins.
#' 
#' @param image Path to a valid image (PNG or JPG) or a \code{\link{loadImage}}
#'   object.
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. Bins = 3 will result in 3^3 = 27 bins; bins = c(2, 2, 3) will
#'   result in 2 * 2 * 3 = 12 bins (2 L, 2 a, 3 b), etc.
#' @param sample.size Numeric. How many pixels should be randomly sampled from
#'   the non-background part of the image and converted into CIE Lab
#'   coordinates? If non-numeric, all pixels will be converted, but this can be
#'   very slow (see details).
#' @param ref.white Reference white passed to \code{\link{convertColorSpace}}.
#'   Unlike \code{convertColor}, no default is provided. See details for
#'   explanation of different reference whites.
#' @param from Original color space of image, probably either "sRGB" or "Apple
#'   RGB", depending on your computer.
#' @param bin.avg Logical. Should the returned color clusters be the average of
#'   the pixels in that bin (bin.avg=\code{TRUE}) or the center of the bin
#'   (\code{FALSE})? If a bin is empty, the center of the bin is returned as the
#'   cluster color regardless.
#' @param as.vec Logical. Should the bin sizes just be returned as a vector?
#'   Much faster if only using \code{\link{chisqDistance}} for comparison metric.
#' @param plotting Logical. Should a histogram of the bin colors and sizes be
#'   plotted?
#' @param lower,upper RGB or HSV triplets specifying the lower and upper bounds
#'   for background pixels. Default upper and lower bounds are set to values
#'   that work well for a bright green background (RGB [0, 1, 0]). Determining
#'   these bounds may take some trial and error, but the following bounds may
#'   work for certain common background colors: \itemize{ \item Black:
#'   lower=c(0, 0, 0); upper=c(0.1, 0.1, 0.1) \item White: lower=c(0.8, 0.8,
#'   0.8); upper=c(1, 1, 1) \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1,
#'   0.24) \item Blue: lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1) } If no
#'   background filtering is needed, set bounds to some non-numeric value
#'   (\code{NULL}, \code{FALSE}, \code{"off"}, etc); any non-numeric value is
#'   interpreted as \code{NULL}.
#' @param alpha.channel Logical. If available, should alpha channel transparency be
#'   used to mask background? See \code{\link{removeBackground}} for more
#'   details.
#' @param title String for what the title the plot if plotting is on; defaults
#'   to the image name.
#' @param a.bounds,b.bounds Numeric ranges for the a (green-red) and b
#'   (blue-yellow) channels of Lab color space. Technically, a and b have
#'   infinite range, but in practice nearly all values fall between -128 and 127
#'   (the default). Many images will have an even narrower range than this,
#'   depending on the lighting conditions and conversion; setting narrower
#'   ranges will result in finer-scale binning, without generating empty bins at
#'   the edges of the channels.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}.
#'   
#' @return A vector or dataframe (depending on whether \code{as.vec = TRUE}) of bin
#'   sizes and color coordinates.
#'
#' @details \code{getLabHist} uses \code{\link{convertColorSpace}} to convert
#'   pixels into CIE Lab coordinates, which requires a references white. There
#'   are seven CIE standardized illuminants available in \code{colordistance}
#'   (A, B, C, E, and D50, D55, and D65), but the most common are: \itemize{
#'   \item \code{"A"}: Standard incandescent lightbulb \item \code{"D65"}:
#'   Average daylight \item \code{"D50"}: Direct sunlight}
#'   
#' Color conversions will be highly dependent on the reference white used, which
#' is why no default is provided. Users should look into
#' \href{https://en.wikipedia.org/wiki/Standard_illuminant}{standard
#' illuminants} to choose an appropriate reference for a dataset.
#' 
#' The conversion from RGB to a standardized color space (XYZ, Lab, or Luv) is
#' approximate, non-linear, and relatively time-consuming. Converting a large
#' number of pixels can be computationally expensive, so
#' \code{convertColorSpace} will randomly sample a specified number of rows to
#' reduce the time. The default sample size, 10,000 rows, takes about 1 second to
#' convert from sRGB to Lab space on an early 2015 Macbook with 8 GB of RAM.
#' Time scales about linearly with number of rows converted.
#' 
#' Unlike RGB or HSV color spaces, the three channels of CIE Lab color space do
#' not all range between 0 and 1; instead, L (luminance) is always between 0 and
#' 100, and the a (green-red) and b (blue-yellow) channels generally vary
#' between -128 and 127, but usually occupy a narrower range depending on the
#' reference white. To achieve the best results, ranges for a and b should be
#' restricted to avoid generating empty bins.
#' 
#' @examples 
#' path <- system.file("extdata", "Heliconius/Heliconius_B/Heliconius_07.jpeg",
#' package="colordistance")
#' getLabHist(path, ref.white = "D65", bins = c(2, 3, 3), lower = rep(0.8, 3),
#' upper = rep(1, 3), sample.size = 1000, ylim = c(0, 1))
#' 
#' @export
getLabHist <- function(image, bins = 3, sample.size = 10000, 
                       ref.white, from = "sRGB", bin.avg = TRUE, 
                       alpha.channel = TRUE,
                       as.vec = FALSE, plotting = TRUE,
                       lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), 
                       title = "path", a.bounds = c(-128, 127),
                       b.bounds = c(-128, 127), ...) {

  message("Accuracy of CIE Lab color space depends on specification of an",
          " appropriate white reference. See 'Color spaces' vignette for",
          " more information.")
  
  # If filepath was provided, check to make sure it exists or throw an error
  if (is.character(image)) {
    if (file.exists(image)) {
      image <- loadImage(image, lower = lower, upper = upper, hsv = FALSE, 
                         CIELab = TRUE, sample.size = sample.size,
                         ref.white = ref.white,
                         alpha.channel = alpha.channel)
    }
  } else if (!is.list(image)) {
    stop("'image' must be either a path to the image",
         " or a list object returned by loadImage")
  }
  
  # If Lab coordinates were not calculated, calculate them
  if ("filtered.lab.2d" %in% names(image)) {
    pix <- image$filtered.lab.2d
  } else {
    pix <- convertColorSpace(image$filtered.rgb.2d, sample.size = sample.size, 
                             to.ref.white = ref.white, from = from, to = "Lab")
  }
  
  # Define universal boundaries
  boundaries <- list(L = c(0, 100), a = a.bounds, b = b.bounds)
  
  # Create vector of bins
  if (length(bins) == 1 | length(bins) == 3) {
    if (length(bins) == 1) {
      message(paste("Using ", bins, "^3 = ", paste(bins ^ 3),
                    " total bins", sep = ""))
      bins <- rep(bins, 3)
    } else {
      message(paste("Using ", bins[1], "*", bins[2], "*", bins[3],
                    " = ", bins[1] * bins[2] * bins[3], " bins",
                    sep = ""))
    }
  
  # Generate breakpoints/boundaries in which to bin the pixels
  breaks <- list(L = numeric(), a = numeric(), b = numeric())
  for (i in 1:length(breaks)) {
    breaks[[i]] <- seq(boundaries[[i]][1],
                       boundaries[[i]][2], length = bins[i] + 1)
  }

  } else {
    
    stop("Bins must be a numeric vector of length 1 or length 3")
    
  }
  
  # Check if all of the pixels fall within the a/b boundaries
  a_range_check <- pix$a < a.bounds[1] | pix$a > a.bounds[2]
  b_range_check <- pix$b < b.bounds[1] | pix$b > b.bounds[2]
  
  # if any of them are outside of 0, throw a warning and remove them from pix
  if ((sum(a_range_check) | sum(b_range_check)) > 0) {
    
    # get the actual range
    ab_range <- round(apply(pix[ , 2:3], 2, range), digits = 2)
    
    # Be nice and provide real ranges
    warning("The specified a and/or b boundaries have removed",
    " some non-background pixels from the analysis. \nMinimum ",
    "ranges to include all pixels are:\n",
    paste0("a: [", ab_range[1, 1], ", ", ab_range[2, 1]), "]\n",
    paste0("b: [", ab_range[1, 2], ", ", ab_range[2, 2], "]"))
    
    # Remove the pixels that were cut off to avoid NA bins
    # Find pixel indices
    cutoff_idx <- unique(c(which(a_range_check), 
                           which(b_range_check)))
    
    # Remove those pixels from the total
    pix <- pix[-cutoff_idx, ]
    
  }
  
  # Bin all the channels
  binnedImage <- data.frame(
    L = cut(pix[, 1], breaks = breaks$L, include.lowest = T, labels = F),
    a = cut(pix[, 2], breaks = breaks$a, include.lowest = T, labels = F),
    b = cut(pix[, 3], breaks = breaks$b, include.lowest = T, labels = F)
  )
  
  # Unless defaultClusters dataframe is provided, just make a dataframe where
  # each default cluster color is the center of the bin
  means <- lapply(breaks, 
          function(i) sapply(c(1:(length(i) - 1)),
            function(j) mean(c(i[j], i[j + 1]))))
  defaultClusters <- expand.grid(means)
  colnames(defaultClusters) <- c("L", "a", "b")
  
  # Set clusters as defaultClusters - values only overwritten if there are
  # pixels in that bin, otherwise center is cluster value and Pct is 0
  clusters <- as.data.frame(defaultClusters)
  
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
    
    # For every identified cluster, get the average L, a, and b values
    # If only one pixel was assigned there (unlikely, but, you know), just plop
    # that pixel in as the average color
    # Otherwise leave the default cluster in place
    for (j in 1:dim(d)[1]) {
      
      pixTemp <- pix[which(ind == j), ]
      
      if (nrow(pixTemp) > 0) {
        
        clusters[which(apply(possibleBins, 1, 
          function(x) all(x == d[j, ]))), 1:3] <- apply(pixTemp, 2, mean)
        
        clusters[which(apply(possibleBins, 1, 
          function(x) all(x == d[j, ]))), 4] <- nrow(pixTemp) / dim(pix)[1]
        
      }
    }
    
  } else {
    
    # Count up pixels per bin without keeping track of which pixel was assigned
    # to which bin
    Pct <- as.vector(xtabs(~ ., binnedImage))
    clusters$Pct <- Pct / max(sum(Pct))
    
  }
  
  # Plot if flagged
  if (plotting) {
    pixelBins <- clusters$Pct
    col.exp <- apply(clusters, 1, 
          function(i) grDevices::rgb(suppressMessages(convertColorSpace(i[1:3],
               from = "Lab", to = from, sample.size = 1,
               from.ref.white = ref.white))))
    
    
    if (title == "path") {
      title <- strsplit(tail(strsplit(image$path, "/")[[1]], 1), "[.]")[[1]][1]
    }
    barplot(as.vector(pixelBins), col = col.exp, main = title, ...)
  }
  
  if (as.vec) {
    clusters <- clusters$Pct
  }
  
  return(clusters)
  
}

#' Generate a list of cluster sets in CIE Lab color space
#' 
#' Applies \code{\link{getLabHist}} to every image in a provided set of image
#' paths and/or directories containing images.
#' 
#' @param images Character vector of directories, image paths, or both.
#' @param bins Number of bins for each channel OR a vector of length 3 with bins
#'   for each channel. Bins = 3 will result in 3^3 = 27 bins; bins = c(2, 2, 3) will
#'   result in 2 * 2 * 3 = 12 bins (2 L, 2 a, 3 b), etc.
#' @param sample.size Numeric. How many pixels should be randomly sampled from
#'   the non-background part of the image and converted into CIE Lab
#'   coordinates? If non-numeric, all pixels will be converted, but this can be
#'   very slow (see details).
#' @param ref.white Reference white passed to \code{\link{convertColorSpace}}.
#'   Unlike \code{convertColor}, no default is provided. See details for
#'   explanation of different reference whites.
#' @param from Original color space of image, probably either "sRGB" or "Apple
#'   RGB", depending on your computer.
#' @param bin.avg Logical. Should the returned color clusters be the average of
#'   the pixels in that bin (bin.avg=\code{TRUE}) or the center of the bin
#'   (\code{FALSE})? If a bin is empty, the center of the bin is returned as the
#'   cluster color regardless.
#' @param as.vec Logical. Should the bin sizes just be returned as a vector?
#'   Much faster if only using \code{\link{chisqDistance}} for comparison metric.
#' @param plotting Logical. Should a histogram of the bin colors and sizes be
#'   plotted?
#' @param pausing Logical. If \code{plotting=T}, should the function pause
#'   between graphing and wait for user to hit \code{[enter]} before continuing?
#'   Useful for data/histogram inspection.
#' @param lower,upper RGB or HSV triplets specifying the lower and upper bounds
#'   for background pixels. Default upper and lower bounds are set to values
#'   that work well for a bright green background (RGB [0, 1, 0]). Determining
#'   these bounds may take some trial and error, but the following bounds may
#'   work for certain common background colors: \itemize{ \item Black:
#'   lower=c(0, 0, 0); upper=c(0.1, 0.1, 0.1) \item White: lower=c(0.8, 0.8,
#'   0.8); upper=c(1, 1, 1) \item Green: lower=c(0, 0.55, 0); upper=c(0.24, 1,
#'   0.24) \item Blue: lower=c(0, 0, 0.55); upper=c(0.24, 0.24, 1) } If no
#'   background filtering is needed, set bounds to some non-numeric value
#'   (\code{NULL}, \code{FALSE}, \code{"off"}, etc); any non-numeric value is
#'   interpreted as \code{NULL}.
#' @param alpha.channel Logical. If available, should alpha channel transparency be
#'   used to mask background? See \code{\link{removeBackground}} for more
#'   details.
#' @param title String for what the title the plot if plotting is on; defaults
#'   to the image name.
#' @param a.bounds,b.bounds Numeric ranges for the a (green-red) and b
#'   (blue-yellow) channels of Lab color space. Technically, a and b have
#'   infinite range, but in practice nearly all values fall between -128 and 127
#'   (the default). Many images will have an even narrower range than this,
#'   depending on the lighting conditions and conversion; setting narrower
#'   ranges will result in finer-scale binning, without generating empty bins at
#'   the edges of the channels.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}.
#' 
#' @return A list of \code{\link{getLabHist}} dataframes, 1 per image, named
#'   by image name.
#'   
#' @details \code{getLabHist} uses \code{\link{convertColorSpace}} to convert
#'   pixels into CIE Lab coordinates, which requires a references white. There
#'   are seven CIE standardized illuminants available in \code{colordistance}
#'   (A, B, C, E, and D50, D55, and D65), but the most common are: \itemize{
#'   \item \code{"A"}: Standard incandescent lightbulb \item \code{"D65"}:
#'   Average daylight \item \code{"D50"}: Direct sunlight}
#'   
#' Color conversions will be highly dependent on the reference white used, which
#' is why no default is provided. Users should look into
#' \href{https://en.wikipedia.org/wiki/Standard_illuminant}{standard
#' illuminants} to choose an appropriate reference for a dataset.
#' 
#' Unlike RGB or HSV color spaces, the three channels of CIE Lab color space do
#' not all range between 0 and 1; instead, L (luminance) is always between 0 and
#' 100, and the a (green-red) and b (blue-yellow) channels generally vary
#' between -128 and 127, but usually occupy a narrower range depending on the
#' reference white. The exception is reference white A (standard incandescent
#' lighting), which tends to have lower values when converting with
#' \code{\link[grDevices]{convertColor}}.
#' 
#' @examples 
#' images <- system.file("extdata", "Heliconius/Heliconius_B",
#' package="colordistance")
#'
#' colordistance::getLabHistList(images, bins = 2, sample.size = 1000, ref.white
#' = "D65", plotting = TRUE, pausing = FALSE, lower = rep(0.8, 3), upper =
#' rep(1, 3), a.bounds = c(-100, 100), b.bounds = c(-127, 100), ylim = c(0, 1))
#' @export
getLabHistList <- function(images, bins = 3, sample.size = 10000, 
                           ref.white, from = "sRGB", bin.avg = TRUE, 
                           as.vec = FALSE, plotting = FALSE, pausing = TRUE,
                           lower = c(0, 0.55, 0), upper = c(0.24, 1, 0.24),
                           alpha.channel = TRUE,
                           title = "path", a.bounds = c(-128, 127),
                           b.bounds = c(-128, 127), ...) {
  
  message("Accuracy of CIE Lab color space depends on specification of",
          " an appropriate white reference. See 'Color spaces' vignette",
          " for more information.")
  
  # If argument isn't a string/vector of strings, throw an error
  if (!is.character(images)) {
    stop("'images' argument must be a string (folder containing",
         " the images), a vector of strings (paths to individual images),",
         " or a combination of both")
  }
  
  im.paths <- c()
  
  # Extract image paths from any folders
  if (length(which(dir.exists(images))) >= 1) {
    im.paths <- unlist(sapply(images[dir.exists(images)], 
                colordistance::getImagePaths), use.names = FALSE)
  }
  
  # For any paths that aren't folders, append to im.paths if they are existing
  # image paths ok this is confusing so to unpack: images[!dir.exists(images)]
  # are all paths that are not directories; then from there we take only ones
  # for which file.exists = TRUE, so we're taking any paths that are not folders
  # but which do exist
  im.paths <- c(im.paths,
        images[!dir.exists(images)][file.exists(images[!dir.exists(images)])])
  
  # Grab only valid image types (jpegs and pngs)
  im.paths <- im.paths[grep(x = im.paths, pattern = "[.][jpg.|jpeg.|png.]",
                            ignore.case = TRUE)]
  
  # Before we embark on this lengthy journey, make sure bins argument is valid
  # Convert bins to a vector of length 3 or throw an error if bins argument is
  # not valid
  if (length(bins) == 1 | length(bins) == 3) {
    if (length(bins) == 1) {
      message(paste("Using ", bins, "^3 = ", paste(bins ^ 3), " total bins",
                    sep = ""))
      bins <- rep(bins, 3)
    } else {
      message(paste("Using ", bins[1], "*", bins[2], "*", bins[3],
                    " = ", bins[1] * bins[2] * bins[3], " bins",
                    sep = ""))
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
    end.list[[i]] <- suppressMessages(getLabHist(im.paths[i], bins = bins, 
                                      bin.avg = bin.avg,
                                      lower = lower, upper = upper,
                                      alpha.channel = alpha.channel,
                                      plotting = plotting, title = title,
                                      ref.white = ref.white, from = from,
                                      a.bounds = a.bounds, b.bounds = b.bounds))
    
    setTxtProgressBar(pb, i)
    
    if (pausing & plotting & i < length(end.list)) {
      pause()
    }
    
  }
  
  names(end.list) <- basename(im.paths)
  
  return(end.list)
  
}
