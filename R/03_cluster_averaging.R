#' Average 3D color histograms by subdirectory
#'
#' Calculates color histograms for images in immediate subdirectories of a
#' folder, and averages histograms for images in the same subdirectory.
#'
#' @param folder Path to the folder containing subdirectories of images. Must be
#'   a character vector.
#' @param method Method for combining color histograms. Default is
#'   \code{"mean"}, but other generic functions (\code{"median"}, \code{"sum"},
#'   etc) will work. String is evaluated using \code{"eval"} so any appropriate
#'   R function is accepted.
#' @param ... Additional arguments passed to \code{\link{getHistList}},
#'   including number of bins, HSV flag, etc.
#'
#' @examples
#' combined_clusters <- colordistance::combineClusters(system.file("extdata",
#' "Heliconius", package="colordistance"), method="median", bins=2,
#' lower=rep(0.8, 3), upper=rep(1, 3))
#' @export
combineClusters <- function(folder, method="mean", ...) {

  # Get absolute filepath
  primary <- normalizePath(folder)

  # Get a list of all immediate subdirectories
  subdirs <- dir(primary, full.names = TRUE)
  subdir.names <- basename(subdirs)
  subdirs <- subdirs[dir.exists(subdirs)]

  # List of all images in each subdirectory, by subdirectory name
  images <- lapply(subdirs, colordistance::getImagePaths)

  # Fill using getHistList - kmeans doesn't make a lot of sense for this
  hist_list <- vector("list", length(images))
  names(hist_list) <- subdir.names
  for (i in 1:length(images)) {
    message(paste(subdir.names[i], ": ", 
                  length(images[[i]]), " images", sep = ""))
    hist_list[[i]] <- suppressMessages(colordistance::getHistList(images[[i]],
                                                                  ...))
  }

  # Combine colors in specified way
  combined_list <- lapply(hist_list, 
                   function(x) colordistance::combineList(x, method = method))

  return(combined_list)
}

#' Combine a list of cluster features into a single cluster set
#'
#' Combine a list of cluster features as returned by \code{\link{getHistList}}
#' according to the specified method.
#'
#' @param hist_list A list of cluster dataframes as returned by
#'   \code{\link{getHistList}}.
#' @param method Method for combining color histograms. Default is
#'   \code{"mean"}, but other generic functions (\code{"median"}, \code{"sum"},
#'   etc) will work. String is evaluated using \code{"eval"} so any appropriate
#'   R function is accepted.
#'
#' @examples
#' hist_list <- getHistList(system.file("extdata", "Heliconius/Heliconius_A",
#' package="colordistance"), lower=rep(0.8, 3), upper=rep(1, 3))
#' median_clusters <- combineList(hist_list, method="median")
#'
#' @note While the function can also accept clusters generated using kmeans
#'   (\code{\link{getKMeansList}} followed by \code{\link{extractClusters}}),
#'   this is not recommended, as kmeans does not provide explicit analogous
#'   pairs of clusters, and clusters are combined by row number (all row 1
#'   clusters are treated as analogous, etc). Color histograms are appropriate
#'   because the bins are defined the same way for each image.
#' @export
combineList <- function(hist_list, method="mean") {
  requireNamespace(package = "abind")
  temp <- do.call(abind::abind, c(hist_list, list(along = 3)))
  df <- as.data.frame(apply(temp, 1:2, eval(method)))
  df[, 4] <- df[, 4] / sum(df[, 4])
  return(df)
}
