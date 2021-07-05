#' Create a checklist.
#'
#' @usage checklist(scientificname = NULL, taxonid = NULL, datasetid = NULL,
#'   nodeid = NULL, areaid = NULL, startdate = NULL, enddate = NULL,
#'   startdepth = NULL, enddepth = NULL, geometry = NULL, redlist = NULL,
#'   hab = NULL, wrims = NULL, dropped = NULL, flags = NULL, exclude = NULL,
#'   verbose = FALSE)
#' @param scientificname the scientific name.
#' @param taxonid the taxon identifier (WoRMS AphiaID).
#' @param datasetid the dataset identifier.
#' @param nodeid the OBIS node identifier.
#' @param areaid the OBIS area identifier.
#' @param startdate the earliest date on which occurrence took place.
#' @param enddate the latest date on which the occurrence took place.
#' @param startdepth the minimum depth below the sea surface.
#' @param enddepth the maximum depth below the sea surface.
#' @param geometry a WKT geometry string.
#' @param redlist include only IUCN Red List species.
#' @param hab include only IOC-UNESCO HAB species.
#' @param wrims include only WRiMS species.
#' @param dropped only include dropped records (\code{TRUE}), exclude dropped records (\code{NULL}) or include dropped records (\code{include}).
#' @param flags quality flags which need to be set.
#' @param exclude quality flags to be excluded from the results.
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The checklist.
#' @examples
#' taxa <- checklist(scientificname = "Tellinidae")
#' taxa <- checklist(geometry = "POLYGON ((2.3 51.8, 2.3 51.6, 2.6 51.6, 2.6 51.8, 2.3 51.8))")
#' taxa <- checklist(areaid = 10181)
#' @export
checklist <- function(
  scientificname = NULL,
  taxonid = NULL,
  datasetid = NULL,
  nodeid = NULL,
  areaid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  redlist = NULL,
  hab = NULL,
  wrims = NULL,
  dropped = NULL,
  flags = NULL,
  exclude = NULL,
  verbose = FALSE
) {

  result_list <- list()
  last_page <- FALSE
  partition <- 0
  fetched <- 0

  while (!last_page) {

    query <- list(
      scientificname = handle_vector(scientificname),
      taxonid = handle_vector(taxonid),
      datasetid = handle_vector(datasetid),
      nodeid = handle_vector(nodeid),
      areaid = handle_vector(areaid),
      startdate = handle_date(startdate),
      enddate = handle_date(enddate),
      startdepth = startdepth,
      enddepth = enddepth,
      geometry = geometry,
      redlist = handle_logical(redlist),
      hab = handle_logical(hab),
      wrims = handle_logical(wrims),
      dropped = dropped,
      flags = handle_vector(flags),
      exclude = handle_vector(exclude),
      partition = partition
    )

    result <- http_request("GET", "checklist", query, verbose)
    if (is.null(result)) return(invisible(NULL))

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    total <- res$total
    partition = partition + 1
    if (!("partitions" %in% names(res)) || partition >= res$partitions) {
      last_page <- TRUE
    }

    if (is.data.frame(res$results)) {
      result_list[[partition + 1]] <- res$results
      fetched <- fetched + nrow(res$results)
    }
    log_progress(fetched, total)

  }

  if (length(result_list) > 0) {
    data <- bind_rows(result_list)
    data <- data[order(data$records, decreasing = TRUE),]
    return(as_tibble(data %>% arrange(desc(.data$records))))
  } else {
    return(tibble())
  }
}
