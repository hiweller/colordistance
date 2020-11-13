#' Pause and wait for user input
#'
#' Tiny little function wrapper, mostly used for looping or when several plots
#' are output by a single function. Waits for user keystroke to move on to next
#' image or exit.
#'
#' @examples
#' for (i in c(1:5)) {
#'   print(i)
#'   if (i < 5) {
#'     colordistance:::pause()
#'   }
#' }
pause <- function(){
  invisible(readline(prompt = "Press [enter] to continue or [esc] to exit"))
}

#' Export a distance matrix as a tree object
#'
#' Converts a symmetrical distance matrix to a tree and saves it in newick
#' format. Uses \code{\link[stats]{hclust}} to form clusters.
#'
#' @param getColorDistanceMatrixObject A distance matrix, especially as returned
#'   by \code{\link{getColorDistanceMatrix}}, but any numeric symmetrical matrix
#'   will work.
#' @param file Character vector of desired filename for saving tree. Should end
#'   in ".newick".
#' @param return.tree Logical. Should the tree object be returned to the working
#'   environment in addition to being saved as a file?
#'
#' @return Newick tree saved in specified location and \code{as.phylo} tree
#'   object if \code{return.tree=TRUE}.
#'
#' @examples
#' \dontrun{
#' clusterList <- colordistance::getHistList(dir(system.file("extdata",
#' "Heliconius/", package="colordistance"), full.names=TRUE), lower=rep(0.8, 3),
#' upper=rep(1, 3))
#' CDM <- colordistance::getColorDistanceMatrix(clusterList, method="emd",
#' plotting=FALSE)
#'
#' # Tree is both saved in current working directory and stored in
#' # heliconius_tree variable
#' 
#' heliconius_tree <- colordistance::exportTree(CDM,
#' "./HeliconiusColorTree.newick", return.tree=TRUE)}
#' @export
exportTree <- function(getColorDistanceMatrixObject, file, return.tree = FALSE) {
  tree <- ape::as.phylo(stats::hclust(stats::as.dist(getColorDistanceMatrixObject)))
  ape::write.tree(tree, file = file)
  if (return.tree) {
    return(tree)
  }
}
