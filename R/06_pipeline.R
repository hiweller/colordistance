#' Generate and plot a color distance matrix from a set of images
#'
#' Takes images, computes color clusters for each image, and calculates distance
#' matrix/dendrogram from those clusters.
#'
#' @param images Character vector of directories, image paths, or both.
#' @param cluster.method Which method for getting color clusters from each image
#'   should be used? Must be either \code{"hist"} (predetermined bins generated
#'   by dividing each channel with equidistant bounds; calls
#'   \code{\link{getHistList}}) or \code{"kmeans"} (determine clusters using
#'   kmeans fitting on pixels; calls \code{\link{getKMeansList}}).
#' @param distance.method One of four possible comparison methods for calculating
#'   the color distances: \code{"emd"} (uses \code{\link{EMDistance}},
#'   recommended), \code{"chisq"} (uses \code{\link{chisqDistance}}),
#'   \code{"color.dist"} (uses \code{\link{colorDistance}}; not appropriate if
#'   bin.avg=F), or \code{"weighted.pairs"}
#'   (\code{\link{weightedPairsDistance}}).
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
#' @param hist.bins Only applicable if \code{cluster.method="hist"}. Number of
#'   bins for each channel OR a vector of length 3 with bins for each channel.
#'   Bins=3 will result in 3^3 = 27 bins; bins=c(2, 2, 3) will result in
#'   2*2*3=12 bins (2 red, 2 green, 3 blue), etc. Passed to
#'   \code{\link{getHistList}}.
#' @param kmeans.bins Only applicable if \code{cluster.method="kmeans"}. Number of
#'   KMeans clusters to fit. Unlike \code{\link{getImageHist}}, this represents
#'   the actual final number of bins, rather than the number of breaks in each
#'   channel.
#' @param bin.avg Logical. Should the color clusters used for the distance matrix
#'   be the average of the pixels in that bin (bin.avg=\code{TRUE}) or the center
#'   of the bin ({FALSE})? If a bin is empty, the center of the bin is returned
#'   as the cluster color regardless. Only applicable if
#'   \code{cluster.method="hist"}, since \code{kmeans} clusters are at the center
#'   of their assigned pixel clouds by definition.
#' @param norm.pix Logical. Should RGB or HSV cluster values be normalized using
#'   \code{\link{normalizeRGB}}?
#' @param bounds Upper and lower limits for the channels; R reads in images with
#'   intensities on a 0-1 scale, but 0-255 is common.
#' @param plot.bins Logical. Should the bins for each image be plotted as they
#'   are calculated?
#' @param pausing Logical. If \code{plot.bins=TRUE}, pause and wait for user
#'   keystroke before plotting bins for next image?
#' @param color.space The color space (\code{"rgb"}, \code{"hsv"}, or
#'   \code{"lab"}) in which to plot pixels.
#' @param ref.white The reference white passed to
#'   \code{\link{convertColorSpace}}; must be specified if using
#'   \code{color.space = "lab"}.
#' @param from  Display color space of image if clustering in CIE Lab space,
#'   probably either "sRGB" or "Apple RGB", depending on your computer.
#' @param img.type Logical. Should file extensions be retained with labels?
#' @param sample.size Only applicable if \code{cluster.method="kmeans"}. Number of
#'   pixels to be randomly sampled from filtered pixel array for performing fit.
#'   If set to \code{FALSE}, all pixels are fit, but this can be time-consuming,
#'   especially for large images. Passed to \code{\link{getKMeansList}}.
#' @param iter.max Only applicable if \code{cluster.method="kmeans"}. Inherited
#'   from \code{\link[stats]{kmeans}}. The maximum number of iterations allowed
#'   during kmeans fitting. Passed to \code{\link{getKMeansList}}.
#' @param nstart Only applicable if \code{cluster.method="kmeans"}. Inherited
#'   from \code{\link[stats]{kmeans}}. How many random sets should be chosen?
#'   Passed to \code{\link{getKMeansList}}.
#' @param ordering Logical if not left as "default". Should the color clusters
#'   in the list be reordered to minimize the distances between the pairs? If
#'   left as default, ordering depends on distance method: "emd" and "chisq" do
#'   not order clusters ("emd" orders on a case-by-case in the
#'   \code{\link{EMDistance}} function itself and reordering by size similarity
#'   would make chi-squared meaningless); "color.dist" and "weighted.pairs" use
#'   ordering. To override defaults, set to either \code{T} (for ordering) or
#'   \code{F} (for no ordering).
#' @param size.weight Weight of size similarity in determining overall score and
#'   ordering (if \code{ordering=T}).
#' @param color.weight Weight of color similarity in determining overall score
#'   and ordering (if \code{ordering=T}). Color and size weights do not
#'   necessarily have to sum to 1.
#' @param plot.heatmap Logical. Should a heatmap of the distance matrix be
#'   plotted?
#' @param return.distance.matrix Logical. Should the distance matrix be returned
#'   to the R environment or just plotted?
#' @param save.tree Either logical or a filepath for saving the tree; default if
#'   set to \code{TRUE} is to save in current working directory as
#'   "ColorTree.newick".
#' @param save.distance.matrix Either logical or filepath for saving distance
#'   matrix; default if set to \code{TRUE} is to save in current working
#'   directory as "ColorDistanceMatrix.csv"
#' @param a.bounds,b.bounds Passed to \code{\link{getLabHistList}}.Numeric
#'   ranges for the a (green-red) and b (blue-yellow) channels of Lab color
#'   space. Technically, a and b have infinite range, but in practice nearly all
#'   values fall between -128 and 127 (the default). Many images will have an
#'   even narrower range than this, depending on the lighting conditions and
#'   conversion; setting narrower ranges will result in finer-scale binning,
#'   without generating empty bins at the edges of the channels.
#'
#' @return Color distance matrix, heatmap, and saved distance matrix and tree
#'   files if saving is \code{TRUE}.
#'
#' @note This is the fastest way to get a distance matrix for color similarity
#' starting from a folder of images. Essentially, it just calls in a series of
#' other package functions in order: input images -> \code{\link{getImagePaths}}
#' -> \code{\link{getHistList}} or \code{\link{getKMeansList}} followed by
#' \code{\link{extractClusters}} -> \code{\link{getColorDistanceMatrix}} ->
#' plotting -> return/save distance matrix. Sort of railroads you, but good for
#' testing different combinations of clustering methods and distance metrics.
#'
#' @examples
#' \dontrun{
#' colordistance::imageClusterPipeline(dir(system.file("extdata", "Heliconius/",
#' package="colordistance"), full.names=TRUE), color.space="hsv", lower=rep(0.8,
#' 3), upper=rep(1, 3), cluster.method="hist", distance.method="emd",
#' hist.bins=3, plot.bins=TRUE, save.tree="example_tree.newick",
#' save.distance.matrix="example_DM.csv")
#' }
#'
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @export
imageClusterPipeline <- function(images, cluster.method = "hist", 
                            distance.method = "emd", lower = c(0, 140 / 255, 0),
                            upper = c(60 / 255, 1, 60 / 255), hist.bins = 3,
                            kmeans.bins = 27, bin.avg = TRUE, norm.pix = FALSE,
                            plot.bins = FALSE, pausing = TRUE, 
                            color.space = "rgb", ref.white, from = "sRGB",
                            bounds = c(0, 1), sample.size = 20000, 
                            iter.max = 50, nstart = 5, img.type = FALSE,
                            ordering = "default", size.weight = 0.5,
                            color.weight = 0.5, plot.heatmap = TRUE,
                            return.distance.matrix = TRUE,
                            save.tree = FALSE, save.distance.matrix = FALSE,
                            a.bounds = c(-127, 128), b.bounds = c(-127, 128)) {

  color.space <- tolower(color.space)
  if (color.space == "hsv") {
    hsv <- TRUE
  } else {
    hsv <- FALSE
  }
  # Get image paths ####
  # If argument isn't a string/vector of strings, throw an error
  if (!is.character(images)) {
    stop("'images' argument must be a string (folder containing the images),", 
         " a vector of strings (paths to individual images),", 
         " or a combination of both")
    }

  im.paths <- c()

  # Extract image paths from any folders
  if (length(which(dir.exists(images))) >= 1) {
    im.paths <- unlist(sapply(images[dir.exists(images)], getImagePaths),
                       use.names = FALSE)
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

  message(paste(length(im.paths), "images"))

  # Get cluster.list using either hist or kmeans ####
  if (cluster.method == "hist") {
    message("Pixel binning method: histogram (predetermined bins)")
    message("Binning images...")
    
    if (color.space == "lab") {
      cluster.list <- getLabHistList(im.paths, bins = hist.bins,
                        bin.avg = bin.avg, lower = lower, upper = upper,
                        plotting = plot.bins, from = from,
                        ref.white = ref.white, pausing = pausing,
                        sample.size = sample.size,
                        a.bounds = a.bounds, b.bounds = b.bounds)
    } else {
      cluster.list <- getHistList(im.paths, bins = hist.bins, 
                                  bin.avg = bin.avg, lower = lower,
                                  upper = upper, norm.pix = norm.pix,
                                  plotting = plot.bins, pausing = pausing,
                                  hsv = hsv, bounds = bounds, img.type = img.type)
    }
  } else if (cluster.method == "kmeans") {
    message("Pixel binning method: kmeans (algorithmically determined bins)")
    message("Generating clusters...")
    cluster.list <- getKMeansList(im.paths, bins = kmeans.bins, 
                                  sample.size = sample.size, 
                                  plotting = plot.bins,
                                  color.space = color.space,
                                  lower = lower, upper = upper, 
                                  iter.max = iter.max, nstart = nstart,
                                  img.type = img.type,
                                  ref.white = ref.white)
    cluster.list <- extractClusters(cluster.list, 
                                    ordering = FALSE, 
                                    normalize = norm.pix)
  } else {
    stop("cluster.method must be one of either 'hist' or 'kmeans'")
  }

  # Get distance matrix using provided method ####
  message(paste("\nComparison metric for distance matrix:", distance.method))
  message("\nCalculating distance matrix...")
  distance.matrix <- getColorDistanceMatrix(cluster.list, 
                      method = distance.method, ordering = ordering, 
                      plotting = plot.heatmap, size.weight = size.weight,
                      color.weight = color.weight)
  message("Done")
  
  # Save output if relevant ####
  if (save.tree != FALSE) {
    # If save.tree is a path, save to that path
    if (is.character(save.tree)) {

      # If provided filepath is a directory then create a filename
      if (dir.exists(save.tree)) {
        save.tree <- paste(save.tree, "/ColorTree.newick", sep = "")
        message(paste("No output filename specified, saving as", save.tree))
      }
    }

    # Otherwise, if save.tree=T, save to current working directory
    else if (save.tree) {
      save.tree <- "./ColorTree.newick"
      message("No output filename specified; saving as 'ColorTree.newick'")
    } else {
      stop("save.tree must be either a valid path or a logical")
    }

    exportTree(distance.matrix, file = save.tree)
    message(paste("Color dendrogram saved to", normalizePath(save.tree)))

  }

  if (save.distance.matrix != FALSE) {
    # If save.tree is a path, save to that path
    if (is.character(save.distance.matrix)) {

      # If provided filepath is a directory then create a filename
      if (dir.exists(save.distance.matrix)) {
        save.distance.matrix <- paste(save.distance.matrix, 
                                      "/ColorDistanceMatrix.csv", 
                                      sep = "")
        message(paste("No output filename specified, saving as", 
                      save.distance.matrix))
      }
    }

    # Otherwise, if save.tree=T, save to current working directory
    else if (save.distance.matrix) {
      save.distance.matrix <- "./ColorDistanceMatrix.csv"
      message("No output filename specified;", 
              " saving as 'ColorDistanceMatrix.csv'")
    } else {
      stop("save.distance.matrix must be either a valid path or a logical")
      }

    # Write csv
    write.csv(distance.matrix, file = save.distance.matrix)
    message(paste("Color distance matrix saved to", 
                  normalizePath(save.distance.matrix)))

  }
  
  # Return distance matrix ####
  if (return.distance.matrix) {
    return(distance.matrix)
    }


}
