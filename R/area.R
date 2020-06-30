#' Fetch a list of areas
#'
#' @usage area()
#' @return The areas.
#' @examples
#' areas <- area()
#' @export
area <- function() {
  result <- http_request("GET", "area", NULL)
  stop_for_status(result)
  text <- content(result, "text", encoding = "UTF-8")
  res <- fromJSON(text, simplifyVector = TRUE)
  total <- res$total
  data <- res$results
  fetched <- nrow(res$results)
  log_progress(fetched, total)
  return(as_tibble(data))
}
