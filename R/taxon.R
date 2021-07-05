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
  if (is.character(taxonid)) {
    taxonid <- str_trim(strsplit(taxonid, ",")[[1]])
  }
  ids <- split(taxonid, ceiling(seq_along(taxonid) / 100))
  results <- purrr::map(ids, function(ids) {
    ids <- handle_vector(ids)
    result <- http_request("GET", paste0("taxon/", ids), list(), verbose)
    if (is.null(result)) return(invisible(NULL))
    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    res$results
  })
  names(results) <- NULL
  return(as_tibble(bind_rows(results)))
}
