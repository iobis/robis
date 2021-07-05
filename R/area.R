#' Fetch a list of areas
#'
#' @usage area(verbose = FALSE)
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The areas.
#' @examples
#' areas <- area(verbose = FALSE)
#' @export
area <- function(verbose = FALSE) {
  result <- http_request("GET", "area", NULL, verbose)
  if (is.null(result)) return(invisible(NULL))
  text <- content(result, "text", encoding = "UTF-8")
  res <- fromJSON(text, simplifyVector = TRUE)
  total <- res$total
  data <- res$results
  fetched <- nrow(res$results)
  log_progress(fetched, total)
  return(as_tibble(data))
}
