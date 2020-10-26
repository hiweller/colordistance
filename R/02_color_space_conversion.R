#' Convert between color spaces
#'
#' Wrapper for \code{\link[grDevices]{convertColor}} that builds in random
#' sampling, error messages, and removes default illuminant (D65) to enforce
#' manual specification of a reference white.
#' 
#' @param color.coordinate.matrix A color coordinate matrix with rows as colors
#'   and channels as columns. If a color histogram (e.g. as returned by
#'   \code{\link{getImageHist}}) is passed, the 'Pct' column is ignored.
#' @param from,to Input and output color spaces, passed to
#'   \code{\link[grDevices]{convertColor}}. See details.
#' @param sample.size Number of pixels to be randomly sampled from filtered pixel
#'   array for conversion. If not numeric or larger than number of colors
#'   provided (i.e. cluster matrix), all colors are converted. See details.
#' @param from.ref.white,to.ref.white Reference whites passed to
#'   \code{\link[grDevices]{convertColor}}. Unlike \code{convertColor}, no
#'   default is provided. See details for explanation of different reference
#'   whites.
#' 
#' @return A 3- or 4-column matrix depending on whether
#'   \code{color.coordinate.matrix} included a 'Pct' column (as from
#'   \code{\link{getImageHist}}), with one column per channel.
#'   
#' @examples
#' # Convert a single RGB triplet and then back convert it
#' rgb_color <- c(0, 1, 0)
#' lab_color <- colordistance::convertColorSpace(rgb_color,
#'  from="sRGB", to="Lab", to.ref.white="D65")
#' rgb_again <- colordistance::convertColorSpace(lab_color,
#'  from="Lab", to="sRGB", from.ref.white="D65")
#' 
#' # Convert pixels from loadImage() function
#' img <- colordistance::loadImage(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"))
#' lab_pixels <- colordistance::convertColorSpace(img$filtered.rgb.2d,
#' from="sRGB", to="XYZ", sample.size=5000)
#' 
#' # Convert clusters
#' img <- colordistance::loadImage(system.file("extdata",
#' "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance"))
#' img_hist <- colordistance::getImageHist(img, bins=2, plotting=FALSE)
#' lab_clusters <- colordistance::convertColorSpace(img_hist, to.ref.white="D55")
#' 
#' @details Color spaces are all passed to
#'   \code{\link[grDevices]{convertColor}}, and can be any of: \code{"XYZ"},
#'   \code{"sRGB"}, \code{"Apple RGB"}, \code{"CIE RGB"}, \code{"Lab"}, or
#'   \code{"Luv"}.
#'   
#'   \code{Lab} and \code{Luv} color spaces are approximately perceptually
#'   uniform, meaning they usually do the best job of reflecting intuitive color
#'   distances without the non-linearity problems of more familiar RGB spaces.
#'   However, because they describe object colors, they require a reference
#'   'white light' color (dimly and brightly lit photographs of the same object
#'   will have very different RGB palettes, but similar Lab palettes if
#'   appropriate white references are used). The idea here is that the apparent
#'   colors in an image depend not just on the "absolute" color of an object,
#'   but also on the available light in the scene. There are seven CIE
#'   standardized illuminants available in \code{colordistance} (A, B, C, E, and
#'   D50, D55, and D65), but the most common are: \itemize{ \item \code{"A"}:
#'   Standard incandescent lightbulb \item \code{"D65"}: Average daylight \item
#'   \code{"D50"}: Direct sunlight}
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
#' reduce the time. The default sample size, 100,000 rows, takes about 5 seconds
#' convert from sRGB to Lab space on an early 2015 Macbook with 8 GB of RAM.
#' Time scales about linearly with number of rows converted.
#' 
#' @export
#' 
convertColorSpace <- function(color.coordinate.matrix, from="sRGB",
                              to="Lab", sample.size=100000, 
                              from.ref.white, to.ref.white) {
  
  # Check whether color.coordinate.matrix is of an appropriate type for color
  # conversion (rows as colors)
  if (length(dim(color.coordinate.matrix)) > 2) {
    stop("Color coordinates must be in matrix form with rows as colors")
    # If a matrix with 3 columns was loaded, assume it's a series of pixels &
    # convert
  } else if (is.vector(color.coordinate.matrix)) {
    color.coordinate.matrix <- t(as.matrix(color.coordinate.matrix))
    Pct <- FALSE
  } else if (ncol(color.coordinate.matrix) == 3) {
    Pct <- FALSE
  } else if (ncol(color.coordinate.matrix) == 4 &
             "Pct" %in% colnames(color.coordinate.matrix)) {
    
    # If it has 4 columns and one is labeled 'Pct' (cluster matrix), then ignore
    # that one and print message
    Pct <- color.coordinate.matrix$Pct
    color.coordinate.matrix$Pct <- NULL
    color.coordinate.matrix <- as.matrix(color.coordinate.matrix)
    message("Ignoring 'Pct' column and treating remaining",
            " columns as color coordinates")
    
  } else {
    
    # Otherwise error out because it can only take tri-coordinate color
    # conversions
    stop("Function requires color matrix with colors",
         " as rows (either 3 columns or a cluster matrix)")
    
  }
  
  ref.whites <- c("A", "B", "C", "E", "D50", "D55", "D65")
  
  # Explicitly state which reference whites are NULL for printing to the
  # console
  if (missing(from.ref.white)) {
    from.ref.white <- NULL
  }
  
  if (missing(to.ref.white)) {
    to.ref.white <- NULL
  }
  
  # If either of the color spaces requires a reference white, make sure they
  # were actually specified
  if (sum(c(from, to) %in% c("Lab", "Luv")) > 0) {
    
    if (is.null(from.ref.white) & is.null(to.ref.white)) {
      
      stop("'Lab' or 'Luv' color space conversions",
           " require specification of a reference white")
      
      # If either were specified but not a standard reference white, stop
      # function
    } else if (!is.null(from.ref.white)) {
      
      if (!(from.ref.white %in% ref.whites)) {
        
        stop(paste(from.ref.white, 
                   "is not a standard CIE illuminant",
                   " (see function documentation)"))
        
      } else if (!is.null(to.ref.white)) {
        
        if (!(to.ref.white %in% ref.whites)) {
          
          stop(paste(to.ref.white, 
                     "is not a standard CIE illuminant",
                     " (see function documentation)"))
          
        }
      }
    }
    
    message(paste("From reference white:", from.ref.white, 
                  "\nTo reference white:", to.ref.white))
    
    }
  
  # If reference whites are appropriate (or not required), proceed
  # Convert sample.size of colors unless sample.size > pixel count
  # In which case, convert all colors
  if (is.numeric(sample.size)) {
    
    if (sample.size <= nrow(color.coordinate.matrix)) {
      message(paste("Converting", sample.size, 
                    "randomly selected pixel(s) from", 
                    from, "color space to", to, "color space"))
      
      ccm <- color.coordinate.matrix[sample(nrow(color.coordinate.matrix), 
                                            sample.size), ]
      
      # Convert colors, using specified parameters
      output.colormatrix <- convertColor(ccm, from = from, to = to,
                                        from.ref.white = from.ref.white, 
                                        to.ref.white = to.ref.white)
      output.colormatrix <- as.data.frame(output.colormatrix)
    } else if (sample.size > nrow(color.coordinate.matrix)) {
      
      message(paste("Subset of pixels for color conversion greater", 
                    " than number of colors provided",
                    "\nConverting all rows from", 
                    from, "color space to", to, "color space"))
      
      # Convert colors, using specified parameters
      output.colormatrix <- as.data.frame(convertColor(color.coordinate.matrix,
                           from = from, to = to, 
                           from.ref.white = from.ref.white, 
                           to.ref.white = to.ref.white))
      
    }
  } else {
    
    output.colormatrix <- as.data.frame(convertColor(color.coordinate.matrix,
                                       from = from, to = to, 
                                       from.ref.white = from.ref.white, 
                                       to.ref.white = to.ref.white))
  }
  
  # Keep Pct column if it existed in the first place
  if (is.numeric(Pct)) {
    output.colormatrix$Pct <- Pct
  }
  return(output.colormatrix)
}
