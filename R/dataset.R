#' Create a list of datasets.
#'
#' @usage dataset(scientificname = NULL, taxonid = NULL, datasetid = NULL,
#'   nodeid = NULL, instituteid = NULL, areaid = NULL, startdate = NULL,
#'   enddate = NULL, startdepth = NULL, enddepth = NULL, geometry = NULL,
#'   redlist = NULL, hab = NULL, wrims = NULL, hasextensions = NULL,
#'   exclude = NULL, verbose = FALSE)
#' @param scientificname the scientific name.
#' @param taxonid the taxon identifier (WoRMS AphiaID).
#' @param datasetid the dataset identifier.
#' @param nodeid the OBIS node identifier.
#' @param instituteid the OBIS institute identifier.
#' @param areaid the OBIS area identifier.
#' @param startdate the earliest date on which occurrence took place.
#' @param enddate the latest date on which the occurrence took place.
#' @param startdepth the minimum depth below the sea surface.
#' @param enddepth the maximum depth below the sea surface.
#' @param geometry a WKT geometry string.
#' @param redlist include only IUCN Red List species.
#' @param hab include only IOC-UNESCO HAB species.
#' @param wrims include only WRiMS species.
#' @param hasextensions which extensions need to be present (e.g. MeasurementOrFact, DNADerivedData, default = \code{NULL}).
#' @param exclude quality flags to be excluded from the results.
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The datasets.
#' @examples
#' datasets <- dataset(scientificname = "Tellinidae")
#' datasets <- dataset(geometry = "POLYGON ((2.3 51.8, 2.3 51.6, 2.6 51.6, 2.6 51.8, 2.3 51.8))")
#' datasets <- dataset(areaid = 10181)
#' @export
dataset <- function(
  scientificname = NULL,
  taxonid = NULL,
  datasetid = NULL,
  nodeid = NULL,
  instituteid = NULL,
  areaid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  redlist = NULL,
  hab = NULL,
  wrims = NULL,
  hasextensions = NULL,
  exclude = NULL,
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
      instituteid = handle_vector(instituteid),
      areaid = handle_vector(areaid),
      startdate = handle_date(startdate),
      enddate = handle_date(enddate),
      startdepth = startdepth,
      enddepth = enddepth,
      geometry = geometry,
      redlist = handle_logical(redlist),
      hab = handle_logical(hab),
      wrims = handle_logical(wrims),
      hasextensions = handle_vector(hasextensions),
      exclude = handle_vector(exclude),
      skip = skip,
      size = page_size()
    )

    result <- http_request("GET", "dataset", query, verbose)
    if (is.null(result)) return(invisible(NULL))

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    total <- res$total
    skip <- skip + page_size()

    if (!is.null(res$results) && is.data.frame(res$results) && nrow(res$results) > 0) {
      res$results$node_id <- sapply(res$results$nodes, function(x) { return(paste0(x$id, collapse = ",")) })
      res$results$node_name <- sapply(res$results$nodes, function(x) { return(paste0(x$name, collapse = ",")) })
      res$results <- res$results[,!(names(res$results) %in% c("node", "feed", "institutes"))]
      result_list[[i]] <- res$results
      fetched <- fetched + nrow(res$results)
      log_progress(fetched, total)
      i <- i + 1
    } else {
      last_page <- TRUE
    }

  }

  data <- bind_rows(result_list)
  return(as_tibble(data))
}
