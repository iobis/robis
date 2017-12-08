#' Fetch taxonomic groups
#' @usage group(verbose = FALSE)
#'
#' @param verbose logical. Optional parameter to enable verbose logging (default
#'   = \code{FALSE}).
#' @return The taxonomic groups.
#' @seealso \code{\link{occurrence}} \code{\link{checklist}} \code{\link{node}}
#'   \code{\link{area}} \code{\link{dataset}} \code{\link{taxon}}
#' @export
group <- function(verbose = FALSE) {
  return(simple_paged("group", verbose))
}
