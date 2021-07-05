#' Fetch a list of nodes
#'
#' @usage node(verbose = FALSE)
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The nodes
#' @examples
#' nodes <- node()
#' @export
node <- function(verbose = FALSE) {
  result <- http_request("GET", "node", NULL, verbose)
  if (is.null(result)) return(invisible(NULL))
  text <- content(result, "text", encoding = "UTF-8")
  res <- fromJSON(text, simplifyVector = TRUE)
  total <- res$total
  res$results <- res$results[,!(names(res$results) %in% c("feeds", "contacts"))]
  data <- res$results
  fetched <- nrow(res$results)
  log_progress(fetched, total)
  return(as_tibble(data))
}
