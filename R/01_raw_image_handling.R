#' Fetch paths to all valid images in a given directory
#'
#' Find all valid image paths (PNG and JPG) in a directory (does not search
#' subdirectories). Will recover any image ending in .PNG, .JPG, or .JPEG,
#' case-insensitive.
#'
#' @param path Path to directory in which to search for images. Absolute or
#'   relative filepaths are fine.
#'
#' @return A vector of absolute filepaths to JPG and PNG images in the given
#'   directory.
#'
#' @note In the event that no compatible images are found in the directory, it
#' returns a message to that effect instead of an empty vector.
#'
#' @examples
#' imDir <- colordistance::getImagePaths(system.file("extdata",
#' "Heliconius/Heliconius_A", package="colordistance"))
#' \dontrun{
#' imDir <- colordistance::getImagePaths("some/nonexistent/directory")
#' }
#' imDir <- colordistance::getImagePaths(getwd())
#'
#' @export
getImagePaths <- function(path) {

  # Make sure input is both a string and a valid folder path
  if (!is.character(path)) {
    stop("Provided filepath is not a string (character type)")} else if (!file.exists(path)) {stop("Folder does not exist")
    } else {
    imDir <- normalizePath(dir(path, pattern="^.*.(jpg|jpeg|png)$", ignore.case=T, full.names=T)) # returns absolute filepaths of images in folder that end in either .jpg, .jpeg, or .png (case-insensitive)

    # If no images were found but the folder path was valid, print message instead of returning an empty vector; otherwise return vector of image paths
    if (length(imDir)==0) {
      message(paste("No images of compatible format (JPG or PNG) in", path))
      } else {
      return(imDir)
      }
  }
}

#' Import image and generate filtered 2D pixel array(s)
#'
#' Imports a single image and returns a list with the original image as a 3D
#' array, a 2D matrix with background pixels removed, and the absolute path to
#' the original image.
#'
#' @param path Path to image (a string).
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
#' @param hsv Logical. Should HSV pixel array also be calculated? Setting to
#'   \code{FALSE} will shave some time off the analysis, but not much (a few
#'   microseconds per image).
#' @param CIELab Logical. Should CIEL*a*b color space pixels be calculated from
#'   RGB? Requires specification of a reference white (see details).
#' @param sampleSize Number of pixels to be randomly sampled from filtered pixel
#'   array for conversion. If not numeric, all pixels are converted, but this
#'   can be time-consuming, especially for large images. See details for speed.
#' @param refWhite String; white reference for converting from RGB to CIEL*a*b
#'   color space. Accepts any of the standard white references for
#'   \code{\link[grDevices]{convertColor}} (see details).
#'
#' @return A list with original image ($original.rgb, 3D array), 2D matrix with
#'   background pixels removed ($filtered.rgb.2d and $filtered.hsv.2d), and path
#'   to the original image ($path).
#'
#' @note The 3D array is useful for displaying the original image, while the 2D
#' arrays (RGB and HSV) are treated as rows of data for clustering in the rest
#' of the package.
#'
#' @examples
#' loadedImg <- colordistance::loadImage(system.file("extdata",
#' "Heliconius/Heliconius_A/Heliconius_01.jpeg", package="colordistance"),
#' upper=rep(1, 3), lower=rep(0.8, 3))
#'
#' loadedImgNoHSV <- colordistance::loadImage(system.file("extdata",
#' "Heliconius/Heliconius_A/Heliconius_01.jpeg", package="colordistance"),
#' upper=rep(1, 3), lower=rep(0.8, 3), hsv=FALSE)
#'
#' @details The upper and lower limits for background pixel elimination set the
#' inclusive bounds for which pixels should be ignored for the 2D arrays; while
#' all background pixels are ideally a single color, images photographed against
#' "uniform" backgrounds often contain some variation, and even segmentation
#' done with photo editing software will produce some variance as a result of
#' image compression.
#'
#' The upper and lower bounds represent cutoffs: any pixel for which the first
#' channel falls between the first upper and lower bounds, the second channel
#' falls between the second upper and lower bounds, and the third channel falls
#' between the third upper and lower bounds, will be ignored. For example, if
#' you have a green pixel with RGB channel values [0.1, 0.9, 0.2], and your
#' upper and lower bounds were (0.2, 1, 0.2) and (0, 0.6, 0) respectively, the
#' pixel would be ignored because 0 <= 0.1 <= 0.2, 0.6 <= 0.9 <= 1, and 0 <= 0.2
#' <= 0.2. But a pixel with the RGB channel values [0.3, 0.9, 0.2] would not be
#' considered background because 0.3 >= 0.2.
#'
#' CIEL*a*b color space requires a reference 'white light' color (dimly and
#' brightly lit photographs of the same object will have very different RGB
#' palettes, but similar Lab palettes if appropriate white references are used).
#' The idea here is that the apparent colors in an image depend not just on the
#' "absolute" color of an object (whatever that means), but also on the
#' available light in the scene. There are seven CIE standardized illuminants
#' available in \code{colordistance} (A, B, C, E, and D50, D55, and D60), but
#' the most common are: \itemize{ \item \code{"A"}: Standard incandescent
#' lightbulb \item \code{"D65"}: Average daylight \item \code{"D50"}: Direct
#' sunlight}
#'
#' Color conversions will be highly dependent on the reference white used, which
#' is why no default is provided. Users should look into
#' \href{https://en.wikipedia.org/wiki/Standard_illuminant}{standard
#' illuminants} to choose an appropriate reference for a dataset.
#'
#' @export
loadImage <- function(path, lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), hsv=TRUE, CIELab=FALSE, sampleSize=100000, refWhite=NULL) {

  # Read in the file as either JPG or PNG (or, if neither, stop execution and return error message)
  if (!is.character(path)) {
    stop("Provided filepath is not a string (must be of character type)")
  }

  # Get absolute filepath in case relative one was provided
  path <- normalizePath(path)

  # Get filetype so we know how to read it in; make lowercase so checking later is easier
  filetype <- tolower(tail(strsplit(path, split="[.]")[[1]], 1))

  if (filetype %in% "png") {
    img <- png::readPNG(path)
    if (dim(img)[3]==4) {
      img <- img[,,1:3] # remove alpha channel if present
    }
  } else if (filetype %in% c("jpg", "jpeg")) {
    img <- jpeg::readJPEG(path)
  } else {
    stop("Images must be either JPEG (.jpg or .jpeg) or PNG (.png) format")
  }

  # Once the file is read in, eliminate pixels that fall between lower and upper bounds (background)
  if (is.numeric(upper) & is.numeric(lower)) {
    idx <- which((lower[1]<=img[, , 1] & img[, , 1]<=upper[1]) & (lower[2]<=img[, , 2] & img[, , 2]<=upper[2]) & (lower[3]<=img[, , 3] & img[, , 3]<=upper[3]))
  } else {idx <- NULL}

  # Reshape image matrix into 2-dimensional pixel matrix (3 columns, each row = 1 pixel with RGB values)
  pix <- img
  dim(pix) <- c(dim(img)[1]*dim(img)[2], 3)
  colnames(pix) <- c("r", "g", "b")
  if (length(idx)!=0) {
    pix <- pix[-idx, ] # remove background pixels
  }

  # Initialize and name empty list depending on flagged color spaces
  # At minimum, includes original image path, 3D RGB array, 2D RGB array with background pixels removed
  # Optional flags: HSV pixels, CIELab pixels
  endList <- vector("list", length=(3 + sum(c(hsv, CIELab))))
  endList_names <- c("path", "original.rgb", "filtered.rgb.2d")
  if (hsv) {
    endList_names <- c(endList_names, "filtered.hsv.2d")
  }

  if (CIELab & !is.null(refWhite)) {
    endList_names <- c(endList_names, "filtered.lab.2d")
  }

  names(endList) <- endList_names

  endList[1:3] <- list(path, img, pix)

  # Return a list with the path to the image, the original RGB image (3d array), and the reshaped matrix with background pixels removed (for clustering analysis)
  if (hsv) {
    endList$filtered.hsv.2d <- t(rgb2hsv(t(pix), maxColorValue = 1))
  }

  if (CIELab) {
    refWhites <- c("A", "B", "C", "E", "D50", "D55", "D65")

    # If user did not choose a reference white, skip conversion
    if (is.null(refWhite)) {

      warning("CIELab reference white not specified; skipping CIELab color space conversion")

    } else if (!(refWhite %in% refWhites)) {

      warning("Reference white is not a standard CIE illuminant (see function documentation); skipping CIELab color space conversion")

    } else {
      
      endList$filtered.lab.2d <- colordistance::convertColorSpace(pix, from="sRGB", to="Lab", to.ref.white=refWhite)
          endList$ref.white <- refWhite

        }
      } 

  return(endList)

}

#' Display an image in a plot window
#'
#' Plots an image as an image.
#'
#' @export
#' @param img Either a path to an image or a \code{\link{loadImage}} object.
#'
#' @return A plot of the provided image in the current plot window.
#'
#' @details Redundant, but a nice sanity check. Used in a few other functions in
#' \code{colordistance} package. Takes either a path to an image (RGB or PNG) or
#' an image object as read in by \code{\link{loadImage}}.
#'
#' @examples
#' colordistance::plotImage(system.file("extdata",
#' "Heliconius/Heliconius_A/Heliconius_01.jpeg", package="colordistance"))
#' colordistance::plotImage(loadImage(system.file("extdata",
#' "Heliconius/Heliconius_A/Heliconius_01.jpeg", package="colordistance"),
#' lower=rep(0.8, 3), upper=rep(1, 3)))
#' @export
plotImage <- function(img) {
  # If a filepath is passed, load the image from that filepath
  if (is.character(img)) {
    if (file.exists(img)) {
      # Read in the file as either JPG or PNG (or, if neither, stop execution and return error message)
      img <- loadImage(img)
    } else {stop("File does not exist")}
  }

  path <- img$path
  img <- img$original.rgb

  # If the image is an array of either 2 (black and white) or 3 (RGB) dimensions, plot it
  if (is.array(img) & (length(dim(img))==2 | length(dim(img))==3)) {

    asp <- dim(img)[1]/dim(img)[2]
    main <- tail(strsplit(path, split="[.]")[[1]], 1)
    plot(0:1, 0:1, type="n", ann=F, axes=F, asp=asp, main=main)
    graphics::rasterImage(img, 0, 0, 1, 1)
  } else {
    # If neither a valid filepath nor a valid array were provided, throw error
    stop("Provided filepath or array must be a 2D (black and white) or 3D (RGB) array for plotting")
  }
}

#' Plot pixels in color space
#'
#' Plots non-background pixels according to their color coordinates, and colors
#' them according to their RGB or HSV values. Dimensions are either RGB or HSV
#' depending on flags.
#' @export
#' @param img Either a path to an image or a \code{\link{loadImage}} object.
#' @param n Number of randomly selected pixels to plot; recommend <20000 for
#'   speed. If n exceeds the number of non-background pixels in the image, all
#'   pixels are plotted. If n is not numeric, all pixels are plotted.
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
#' @param hsv Logical. Should pixels be plotted in HSV instead of RGB
#'   color space?
#' @param rev Logical. Should the plot be rotated to view pixels which may be
#'   obscured when rev is \code{FALSE}?
#' @param ... Optional parameters passed to \code{\link[scatterplot3d]{scatterplot3d}}.
#'
#' @return 3D plot of pixels in either RGB or HSV color space, colored according
#'   to their color in the image. Uses
#'   \code{\link[scatterplot3d]{scatterplot3d}} function.
#' @examples
#' colordistance::plotPixels(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"),
#' n=20000, upper=rep(1, 3), lower=rep(0.8, 3))
#' @note If \code{n} is not numeric, then all pixels are plotted, but this is
#'   not recommended. Unless the image has a low pixel count, it takes much
#'   longer, and plotting this many points in the plot window can obscure
#'   important details.
#'
#' @export
plotPixels <- function(img, n=10000, lower=c(0, 0.55, 0), upper=c(0.25, 1, 0.25), hsv=FALSE, rev=FALSE, ...) {

  # If a filepath is passed, load the image from that filepath
  if (is.character(img)) {
    if (file.exists(img)) {
      # Read in the file as either JPG or PNG (or, if neither, stop execution and return error message)
      img <- loadImage(img, upper=upper, lower=lower, hsv=hsv)
    }
  } else if (!is.list(img)) {
    stop("'img' must be either a valid filepath to an image or a loadImage object")
    }

  # Set pixels and generate color vector
  if (hsv) {
    pix <- img$filtered.hsv.2d
    if (is.numeric(n) & n < dim(pix)[1]) {
      pix <- pix[sample(nrow(pix), n), ]
    } else {
      n <- "all"
    }
    colExp <- apply(pix, 1, function(x) hsv(x[1], x[2], x[3]))
    xlab <- "Hue"; ylab <- "Saturation"; zlab <- "Value"
  } else {
    pix <- img$filtered.rgb.2d
    if (is.numeric(n) & n < dim(pix)[1]) {
      pix <- pix[sample(nrow(pix), n),]
    } else {
      n <- "all"
    }
    colExp <- apply(pix, 1, function(x) rgb(x[1], x[2], x[3]))
    xlab <- "Red"; ylab <- "Green"; zlab <- "Blue"
  }

  # Plot 3d scatterplot
  if (rev) {
    pix <- -pix
  }
  scatterplot3d::scatterplot3d(pix, pch=20, color=colExp, xlab=xlab, ylab=ylab, zlab=zlab, main=paste(tail(strsplit(img$path, split="/")[[1]], 1), n, "points"), ...)

}