#' Create a list of datasets.
#'
#' @usage dataset(scientificname = NULL, taxonid = NULL, datasetid = NULL,
#'   nodeid = NULL, instituteid = NULL, areaid = NULL, startdate = NULL,
#'   enddate = NULL, startdepth = NULL, enddepth = NULL, geometry = NULL,
#'   redlist = NULL, hab = NULL, wrims = NULL, hasextensions = NULL,
#'   exclude = NULL, keyword = NULL, verbose = FALSE)
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
#' @param hasextensions which extensions need to be present (e.g. MeasurementOrFact, DNADerivedData).
#' @param exclude quality flags to be excluded from the results.
#' @param keyword Keyword(s) to search for in dataset metadata using
#' @param keyword Keyword(s) to search for in dataset metadata. When \code{keyword} is used, no other filter parameters may be specified.
#'
#'   The search uses \href{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html}{Elasticsearch \code{simple_query_string} syntax}.
#'   The following symbolic operators are supported:
#'   \itemize{
#'     \item \code{+}: Equivalent to AND, requiring all terms. Any whitespace is also an implicit AND.
#'       For example, \code{coral+reef} is equivalent to \code{coral reef}.
#'     \item Quotes (\code{"..."}): Used for exact phrase matching. For example, \code{"coral reef"} is stricter than the unquoted version.
#'     \item \code{|}: Equivalent to OR. Returns records where any listed term is present.
#'     \item \code{-}: Equivalent to NOT. Excludes records containing the specified term.
#'     \item \code{*}: Wildcard matching is supported \strong{only at the end of a word}.
#'       For example, \code{star*} matches "starfish" and \code{sea*} matches "seastar".
#'       However, \code{*star*} returns no results, and leading wildcards like \code{*star} may appear to work
#'       in some cases but should not be relied upon.
#'     \item Grouping with parentheses: \code{(coral | kelp) -fish} returns results that include coral OR kelp, but NOT fish.
#'       Removing the parentheses would change the meaning — it would return results that include coral,
#'       or kelp only when fish is also present (i.e., \code{coral OR (kelp -fish)}).
#'   }
#'
#'
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return A tibble of matching datasets.
#' @examples
#' dataset(scientificname = "Tellinidae")
#' dataset(areaid = 10181)
#' dataset(keyword = '(coral | kelp) -fish')
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
    keyword = NULL,
    verbose = FALSE
) {
  # Prevent combining keyword with other filters
  if (!is.null(keyword)) {
    validate_keyword(keyword)

    if (any(!sapply(list(scientificname, taxonid, datasetid, nodeid, instituteid, areaid,
                         startdate, enddate, startdepth, enddepth, geometry,
                         redlist, hab, wrims, hasextensions, exclude),
                    is.null))) {
      stop("When 'keyword' is used, no other filter parameters may be specified.")
    }

    params <- list(
      q = keyword,
      limit = 100,
      offset = 0
    )

    result <- http_request("GET", "dataset/search2", params, verbose)
    if (is.null(result)) return(invisible(NULL))

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    if (!is.null(res$results) && is.data.frame(res$results) && nrow(res$results) > 0) {
      data <- as_tibble(res$results)
    } else {
      data <- tibble()
    }
    return(data)
  }

  # Default behavior: use /v3/dataset with paging
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
      res$results$node_id <- sapply(res$results$nodes, function(x) paste0(x$id, collapse = ","))
      res$results$node_name <- sapply(res$results$nodes, function(x) paste0(x$name, collapse = ","))
      res$results <- res$results[, !(names(res$results) %in% c("node", "feed", "institutes"))]
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
