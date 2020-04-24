#' Remove background pixels in image
#'
#' Take an image array (from \code{\link[png]{readPNG}} or
#' \code{\link{jpeg}{readJPEG}}) and remove the background pixels based on
#' transparency (if a PNG with transparency) or color boundaries.
#' 
#' @param img Image array, either output from \code{\link[png]{readPNG}} or
#'   \code{\link{jpeg}{readJPEG}}.
#' @param lower,upper RGB or HSV triplets specifying the bounds for background
#'   pixels. See \code{\link{loadImage}}.
#' @param quietly Logical. Display a message if using transparency?
#' @param alpha.channel Logical. If available, should alpha channel transparency be
#'   used to mask background? See details.
#' 
#' @return A list with a 3-dimensional RGB array and a 2-dimensional array of
#'   non-background pixels with R, G, B columns.
#' 
#' @details If \code{alpha.channel = TRUE}, transparency takes precedence over
#'   color masking. If you provide a PNG with any pixels with alpha < 1,
#'   \code{removeBackground} ignores any \code{lower} and \code{upper} color
#'   boundaries and assumes transparent pixels are background. If all pixels are
#'   opaque (alpha = 1), color masking will apply.
#'   
#' @examples 
#' 
#' # remove background by transparency
#' img_path <- system.file("extdata/chrysochroa_NPL.png",
#'  package = "colordistance")
#'  
#' img_array <- png::readPNG(img_path)
#' 
#' img_filtered <- removeBackground(img_array)
#' 
#' # remove background by color
#' img_path <- dir(system.file("extdata/Heliconius", 
#' package = "colordistance"), 
#' recursive = TRUE, full.names = TRUE)[1]
#' 
#' img_array <- jpeg::readJPEG(img_path)
#' 
#' img_filtered <- removeBackground(img_array,
#' lower = rep(0.8, 3), upper = rep(1, 3))
#' 
#' @export
removeBackground <- function(img,
                             lower = NULL, upper = NULL,
                             quietly = FALSE,
                             alpha.channel = TRUE) {
  
  # assume no background masking to start
  idx <- NULL
  
  # and store RGB channels
  original.rgb <- img[ , , 1:3]
  
  # if there's transparency, use that for background indexing
  # set transparent pixels to white
  if (dim(img)[3] == 4 & alpha.channel == TRUE) {
    
    if (min(img[ , , 4]) < 1) {
      
      if (!quietly) {
        message("Using PNG transparency (alpha channel) as background mask")
      }
      
      # index background pixels based on opacity
      idx <- which(img[ , , 4] != 1)
      
      # make transparent pixels white for plotting
      for (i in 1:3) {
        channel <- original.rgb[ , , i]
        channel[idx] <- 1
        original.rgb[ , , i] <- channel
      }
    } else {
      
      warning("No transparent pixels in image")
      
    }
    
  }
  
  # if there was no transparency, try color
  if (is.null(idx)) {
    
    # if upper and lower are numeric:
    # index values inside of upper and lower bounds
    if (is.numeric(upper) & is.numeric(lower)) {
      idx <- which((lower[1] <= img[, , 1] &
                      img[, , 1] <= upper[1]) &
                     (lower[2] <= img[, , 2] &
                        img[, , 2] <= upper[2]) &
                     (lower[3] <= img[, , 3] &
                        img[, , 3] <= upper[3]))
    }
    
  }

  # make filtered.rgb.2d: all the non-indexed pixels from img
  pix <- original.rgb
  dim(pix) <- c(dim(img)[1] * dim(img)[2], 3)
  colnames(pix) <- c("r", "g", "b")
  if (length(idx) != 0) {
    pix <- pix[-idx, ] # remove background pixels
  }
  
  # return 3D array for plotting
  # and flattened non-background RGB triplets
  return(list(original.rgb = original.rgb,
              filtered.rgb.2d = pix))
}
