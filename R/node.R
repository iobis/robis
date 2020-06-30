#' Fetch a list of nodes
#'
#' @usage node()
#' @return The nodes
#' @examples
#' nodes <- node()
#' @export
node <- function() {
  result <- http_request("GET", "node", NULL)
  stop_for_status(result)
  text <- content(result, "text", encoding = "UTF-8")
  res <- fromJSON(text, simplifyVector = TRUE)
  total <- res$total
  res$results <- res$results[,!(names(res$results) %in% c("feeds", "contacts"))]
  data <- res$results
  fetched <- nrow(res$results)
  log_progress(fetched, total)
  return(as_tibble(data))
}
