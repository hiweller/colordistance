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
#' im.dir <- colordistance::getImagePaths(system.file("extdata",
#' "Heliconius/Heliconius_A", package="colordistance"))
#' \dontrun{
#' im.dir <- colordistance::getImagePaths("some/nonexistent/directory")
#' }
#' im.dir <- colordistance::getImagePaths(getwd())
#'
#' @export
getImagePaths <- function(path) {

  # Make sure input is both a string and a valid folder path
  if (!is.character(path)) {
    
    stop("Provided filepath is not a string (character type)")
    
    } else if (!file.exists(path)) {
      
      stop("Folder does not exist")
      
    } else {
      
    im.dir <- normalizePath(dir(path, pattern = "^.*.(jpg|jpeg|png)$",
                                ignore.case = T, full.names = T))     
    # returns absolute filepaths of images in folder that end in either .jpg,
    # .jpeg, or .png (case-insensitive)

    # If no images were found but the folder path was valid, print message
    # instead of returning an empty vector; otherwise return vector of image
    # paths
    if (length(im.dir) == 0) {
      message(paste("No images of compatible format (JPG or PNG) in", path))
      } else {
      return(im.dir)
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
#' @param sample.size Number of pixels to be randomly sampled from filtered pixel
#'   array for conversion. If not numeric, all pixels are converted.
#' @param ref.white String; white reference for converting from RGB to CIEL*a*b
#'   color space. Accepts any of the standard white references for
#'   \code{\link[grDevices]{convertColor}} (see details).
#' @param alpha.channel Logical. If available, should alpha channel transparency be
#'   used to mask background? See \code{\link{removeBackground}} for more
#'   details.
#' @param alpha.message Logical. Output a message if using alpha channel
#'   transparency to mask background? Helpful for troubleshooting with PNGs.
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
loadImage <- function(path, lower = c(0, 0.55, 0),
                      upper = c(0.24, 1, 0.24), hsv = TRUE,
                      CIELab = FALSE, sample.size = 100000,
                      ref.white = NULL, alpha.channel = TRUE,
                      alpha.message = FALSE) {

  # Read in the file as either JPG or PNG (or, if neither, stop execution and
  # return error message)
  if (!is.character(path)) {
    stop("Provided filepath is not a string (must be of character type)")
  }

  # Get absolute filepath in case relative one was provided
  path <- normalizePath(path)

  # Get filetype so we know how to read it in; make lowercase so checking later
  # is easier
  filetype <- tolower(tail(strsplit(path, split = "[.]")[[1]], 1))

  if (filetype %in% "png") {
    img <- png::readPNG(path)
  } else if (filetype %in% c("jpg", "jpeg")) {
    img <- jpeg::readJPEG(path)
  } else {
    stop("Images must be either JPEG (.jpg or .jpeg) or PNG (.png) format")
  }

  # Once the file is read in, eliminate pixels that fall between lower and upper
  # bounds (background)
  filtered.img <- removeBackground(img,
                                   lower = lower,
                                   upper = upper,
                                   quietly = alpha.message,
                                   alpha.channel = alpha.channel)
  
  # Initialize and name empty list depending on flagged color spaces At minimum,
  # includes original image path, 3D RGB array, 2D RGB array with background
  # pixels removed 
  # Optional flags: HSV pixels, CIELab pixels
  end.list <- vector("list", length = (3 + sum(c(hsv, CIELab))))
  endList_names <- c("path", "original.rgb", "filtered.rgb.2d")
  if (hsv) {
    endList_names <- c(endList_names, "filtered.hsv.2d")
  }

  if (CIELab & !is.null(ref.white)) {
    endList_names <- c(endList_names, "filtered.lab.2d")
  }

  names(end.list) <- endList_names

  end.list[1:3] <- list(path,
                        filtered.img$original.rgb, 
                        filtered.img$filtered.rgb.2d)
  pix <- filtered.img$filtered.rgb.2d

  # Return a list with the path to the image, the original RGB image (3d array),
  # and the reshaped matrix with background pixels removed (for clustering
  # analysis)
  if (hsv) {
    end.list$filtered.hsv.2d <- t(rgb2hsv(t(pix), maxColorValue = 1))
  }

  if (CIELab) {
    ref.whites <- c("A", "B", "C", "E", "D50", "D55", "D65")

    # If user did not choose a reference white, skip conversion
    if (is.null(ref.white)) {

      warning("CIELab reference white not specified; skipping 
              CIELab color space conversion")

    } else if (!(ref.white %in% ref.whites)) {

      warning("Reference white is not a standard CIE illuminant 
              (see function documentation); skipping CIELab color 
              space conversion")

    } else {
      end.list$filtered.lab.2d <- colordistance::convertColorSpace(pix, 
                                 from = "sRGB", to = "Lab", 
                                 sample.size = sample.size, 
                                 to.ref.white = ref.white)
      end.list$ref.white <- ref.white
        }
      }

  return(end.list)

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
      # Read in the file as either JPG or PNG (or, if neither, stop execution
      # and return error message)
      img <- loadImage(img)
    } else {
      stop("File does not exist")
      }
  }

  path <- img$path
  img <- img$original.rgb

  # If the image is an array of either 2 (black and white) or 3 (RGB)
  # dimensions, plot it
  if (is.array(img) & (length(dim(img)) == 2 | length(dim(img)) == 3)) {
    asp <- dim(img)[1] / dim(img)[2]
    main <- tail(strsplit(path, split = "[.]")[[1]], 1)
    plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE, 
         asp = asp, main = main)
    graphics::rasterImage(img, 0, 0, 1, 1)
  } else {
    # If neither a valid filepath nor a valid array were provided, throw error
    stop("Provided filepath or array must be a 2D (black and white) 
         or 3D (RGB) array for plotting")
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
#' @param color.space The color space (\code{"rgb"}, \code{"hsv"}, or
#'   \code{"lab"}) to use for plotting.
#' @param ref.white The reference white passed to
#'   \code{\link[grDevices]{convertColor}}; must be specified if \code{img} does
#'   not already contain CIE Lab pixels. See \link{convertColorSpace}.
#' @param pch Passed to \code{\link[scatterplot3d]{scatterplot3d}}.
#' @param main Plot title. If left as "default", image name is used.
#' @param from Original color space of image if plotting in CIE Lab space,
#'   probably either "sRGB" or "Apple RGB", depending on your computer.
#' @param xlim,ylim,zlim Ranges for the X, Y, and Z axes. If "default", the
#'   widest ranges for each axis according to the specified color space (0-1 for
#'   RGB and HSV, 0-100 for L of Lab, -128-127 for a and b of Lab) are used.
#' @param ... Optional parameters passed to
#'   \code{\link[scatterplot3d]{scatterplot3d}}.
#'
#' @return 3D plot of pixels in either RGB or HSV color space, colored according
#'   to their color in the image. Uses
#'   \code{\link[scatterplot3d]{scatterplot3d}} function.
#' @examples
#' colordistance::plotPixels(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"),
#' n=20000, upper=rep(1, 3), lower=rep(0.8, 3), color.space = "rgb", angle = -45)
#' @note If \code{n} is not numeric, then all pixels are plotted, but this is
#'   not recommended. Unless the image has a low pixel count, it takes much
#'   longer, and plotting this many points in the plot window can obscure
#'   important details.
#'   
#'   There are seven CIE standardized illuminants available in
#'   \code{colordistance} (A, B, C, E, and D50, D55, and D65), but the most
#'   common are: \itemize{ \item \code{"A"}: Standard incandescent lightbulb
#'   \item \code{"D65"}: Average daylight \item \code{"D50"}: Direct sunlight}
#'
#' @export
plotPixels <- function(img, n = 10000, lower = c(0, 0.55, 0), 
                       upper = c(0.25, 1, 0.25), color.space = "rgb", 
                       ref.white = NULL, pch = 20, main = "default", 
                       from = "sRGB", xlim = "default",
                       ylim = "default", zlim = "default", ...) {

  # If a filepath is passed, load the image from that filepath
  if (is.character(img)) {
    
    if (file.exists(img)) {
      # Read in the file as either JPG or PNG (or, if neither, stop execution
      # and return error message)
      if (tolower(color.space) == "lab") {
        CIELab <- TRUE; hsv <- FALSE
      } else if (tolower(color.space) == "hsv") {
        CIELab <- FALSE; hsv <- TRUE
      } else {
        CIELab <- FALSE; hsv <- FALSE
      }
      
      img <- loadImage(img, upper = upper, lower = lower, hsv = hsv,
                       CIELab = CIELab, sample.size = n, 
                       ref.white = ref.white)
    }
  } else if (!is.list(img)) {
    stop("'img' must be either a valid filepath to an image or a loadImage
         object")
  }
  
  # Set graph title
  if (main == "default") {
    main <- paste(basename(img$path), ",", n, "points")
  }
  
  # Set pixels and generate color vector
  # If Lab color space, set axis labels and boundaries appropriately
  if (tolower(color.space) == "lab") {
    
    if (!("filtered.lab.2d" %in% names(img))) {
      
      pix <- convertColorSpace(img$filtered.rgb.2d, from = from, 
                               to = "Lab", sample.size = n, 
                               from.ref.white = ref.white)
    } else {
      pix <- img$filtered.lab.2d
    }
    
    xlab <- "Luminance"; ylab <- "a (green-red)"; zlab <- "b (blue-yellow)"
    xb <- c(0, 100); yb <- c(-128, 127); zb <- c(-128, 127)
    # If pixels being plotted > pixels in image, plot all; otherwise, sample
    if (is.numeric(n) & n < nrow(pix)) {
      pix <- pix[sample(nrow(pix), n), ]
    } else {
      n <- "all"
    }
    # Generate hex colors for each pixel
    colExp <- grDevices::rgb(suppressMessages(convertColorSpace(from = "Lab",
              to = "sRGB", color.coordinate.matrix = pix, 
              sample.size = "all", from.ref.white = img$ref.white)))
  } else {
    # RGB and HSV color spaces have 0-1 bounds
    xb <- c(0, 1); yb <- c(0, 1); zb <- c(0, 1)
    if (tolower(color.space) == "hsv") {
      pix <- img$filtered.hsv.2d
      xlab <- "Hue"; ylab <- "Saturation"; zlab <- "Value"
      if (is.numeric(n) & n < nrow(pix)) {
        pix <- pix[sample(nrow(pix), n), ]
      } else {
        n <- "all"
      }
      colExp <- apply(pix, 1, function(x) grDevices::hsv(x[1], x[2], x[3]))
    } else {
      pix <- img$filtered.rgb.2d
      if (is.numeric(n) & n < dim(pix)[1]) {
        pix <- pix[sample(nrow(pix), n), ]
      } else {
        n <- "all"
      }
      colExp <- apply(pix, 1, function(x) grDevices::rgb(x[1], x[2], x[3]))
      xlab <- "Red"; ylab <- "Green"; zlab <- "Blue"
    }
  }
  
  if (xlim[1] == "default") {
    xlim <- xb
  }
  
  if (ylim[1] == "default") {
    ylim <- yb
  }
  
  if (zlim[1] == "default") {
    zlim <- zb
  }
  
  # Plot 3d scatterplot
  scatterplot3d::scatterplot3d(pix, pch = 20, color = colExp,
                               xlab = xlab, ylab = ylab, zlab = zlab,
                               main = main, 
                               xlim = xlim, ylim = ylim, zlim = zlim,
                               ...)
}
