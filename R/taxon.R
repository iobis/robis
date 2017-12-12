#' Fetch taxon.
#' @usage taxon(obisid = NULL, aphiaid = NULL, scientificname = NULL, verbose = FALSE)
#'
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The areas.
#' @seealso \code{\link{taxon_common}} \code{\link{occurrence}} \code{\link{checklist}} \code{\link{node}} \code{\link{area}}
#'   \code{\link{dataset}} \code{\link{group}}
#' @export
taxon <- function(obisid = NULL,
                  aphiaid = NULL,
                  scientificname = NULL,
                  verbose = FALSE) {
  query <- list()
  query[["obisid"]] <- obisid
  query[["aphiaid"]] <- aphiaid
  query[["scientificname"]] <- scientificname
  if(length(query) == 1) {
    result <- http_request("GET", "taxon", query)
    if (verbose) {
      log_request(result)
    }
    stop_for_status(result)
    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE )

    if(is.null(res$message)) {
      return(res$results)
    } else {
      warning(res$message)
      return(NULL)
    }
  } else if(length(query) == 0) {
    stop("robis::taxon Missing parameter: obisid, aphiaid or scientificname should be provided")
  } else if(length(query) > 1) {
    stop("robis::taxon Too many parameters: Only one off obisid, aphiaid or scientificname should be provided")
  }
}

#' Fetch common names.
#' @usage taxon_common(verbose = FALSE)
#'
#' @param obisid numeric. OBIS identifier for the taxon.
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The areas.
#' @seealso \code{\link{taxon_common}} \code{\link{occurrence}} \code{\link{checklist}} \code{\link{node}} \code{\link{area}}
#'   \code{\link{dataset}} \code{\link{group}}
#' @export
taxon_common <- function(obisid, verbose = FALSE) {
  if(length(obisid) == 1 && suppressWarnings(!is.na(as.numeric(obisid)))) {
    result <- http_request("GET", paste0("taxon/",obisid,"/common"), list())
    if (verbose) {
      log_request(result)
    }
    stop_for_status(result)
    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE )
    return(res$results)
  } else {
    stop("robis::taxon_common Invalid parameter [obisid] should be a single number")
  }
}
