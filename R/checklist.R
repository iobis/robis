#' Create a checklist.
#'
#' @export
checklist <- function(
  scientificname = NULL,
  taxonid = NULL,
  datasetid = NULL,
  nodeid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  verbose = FALSE
) {

  skip <- 0
  result_list <- list()
  last_page <- FALSE
  i <- 1
  fetched <- 0

  while (!last_page) {

    query <- list(
      scientificname = handle_vector(scientificname),
      taxonid = handle_vector(taxonid),
      datasetid = handle_vector(datasetid),
      nodeid = handle_vector(nodeid),
      startdate = handle_date(startdate),
      enddate = handle_date(enddate),
      startdepth = startdepth,
      enddepth = enddepth,
      geometry = geometry,
      skip = skip,
      size = page_size()
    )

    result <- http_request("GET", "checklist", query)

    if (verbose) {
      log_request(result)
    }

    stop_for_status(result)

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    total <- res$total
    skip <- skip + page_size()

    if (!is.null(res$results) && is.data.frame(res$results) && nrow(res$results) > 0) {
      result_list[[i]] <- res$results
      fetched <- fetched + nrow(res$results)
      log_progress(fetched, total)
      i <- i + 1
    } else {
      last_page <- TRUE
    }

  }

  data <- bind_rows(result_list)
  #data <- data[,!empty_cols(data)]

  return(data)

}
