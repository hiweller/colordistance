#' Pause and wait for user input
#'
#' Tiny little function wrapper, mostly used for looping or when several plots are output by a single function. Waits for user keystroke to move on to next image or exit.
#'
#' @examples
#' for (i in c(1:5)) {
#'   print(i)
#'   if (i < 5) {
#'     pause()
#'   }
#' }
pause <- function(){
  invisible(readline(prompt="Press [enter] to continue or [esc] to exit"))
}

#' Export a distance matrix as a tree object
#'
#' Converts a symmetrical distance matrix to a tree and saves it in newick format. Uses \code{\link[stats]{hclust}} to form clusters.
#'
#' @param getColorDistanceMatrixObject A distance matrix, especially as returned by \code{\link{getColorDistanceMatrix}}, but any numeric symmetrical matrix will work.
#' @param file Character vector of desired filename for saving tree. Should end in ".newick".
#' @param returnTree Logical. Should the tree object be returned to the working environment in addition to being saved as a file?
#'
#' @return Newick tree saved in specified location and \code{as.phylo} tree object if \code{returnTree=TRUE}.
#'
#' @examples
#' clusterList <- getHistList("Heliconius/")
#' CDM <- getColorDistanceMatrix(clusterList, method="emd", plotting=F)
#' heliconius_tree <- exportTree(CDM, "./HeliconiusColorTree.newick", returnTree=T)
#' @export
exportTree <- function(getColorDistanceMatrixObject, file, returnTree=F) {
  tree <- ape::as.phylo(hclust(as.dist(getColorDistanceMatrixObject)))
  ape::write.tree(tree, file=file)
  if (returnTree) {
    return(tree)
  }
}
