#' Fetch nodes
#' @usage node(verbose = FALSE)
#'
#' @param verbose logical. Optional parameter to enable verbose logging (default
#'   = \code{FALSE}).
#' @return The nodes.
#' @seealso \code{\link{occurrence}} \code{\link{checklist}} \code{\link{group}}
#'   \code{\link{area}} \code{\link{dataset}} \code{\link{taxon}}
#' @export
node <- function(verbose = FALSE) {
  return(simple_paged("node", verbose))
}

