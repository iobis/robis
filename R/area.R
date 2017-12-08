#' Fetch areas.
#' @usage area(verbose = FALSE)
#'
#' @param verbose logical. Optional parameter to enable verbose logging (default
#'   = \code{FALSE}).
#' @return The areas.
#' @seealso \code{\link{occurrence}} \code{\link{checklist}} \code{\link{node}}
#'   \code{\link{group}} \code{\link{dataset}} \code{\link{taxon}}
#' @export
area <- function(verbose = FALSE) {
  return(simple_paged("area", verbose))
}

