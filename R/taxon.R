#' Get taxon by taxon ID.
#'
#' @usage taxon(taxonID, verbose = FALSE)
#' @param taxonid the taxon identifier (WoRMS AphiaID).
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return Tibble containing taxon records.
#' @examples
#' taxon(c(141433, 141434))
#' @export
taxon <- function(
  taxonid,
  verbose = FALSE
) {
  result <- http_request("GET", paste0("taxon/", handle_vector(taxonid)), list())
  if (verbose) {
    log_request(result)
  }
  stop_for_status(result)
  text <- content(result, "text", encoding = "UTF-8")
  res <- fromJSON(text, simplifyVector = TRUE)
  return(as_tibble(res$results))
}
