extract_dataset <- function(res) {
  dataset <- list(
    "id" = res$id,
    "digirname" = res$digirname,
    "website" = res$website,
    "citation" = res$citation,
    "usage" = res$usage,
    "species_cnt" = res$species_cnt,
    "taxon_cnt" = res$taxon_cnt,
    "record_cnt" = res$record_cnt,
    "imis_dasid" = res$imis_dasid,
    "archiveurl" = res$archiveurl,
    "name" = res$name
  )
  dataset[!vapply(dataset, is.null, TRUE)]
}

#' Fetch dataset metadata by id.
#'
#' \code{dataset_by_id} is an internal method for fetching dataset metadata based on the internal
#' identifier.
#'
#' @usage dataset_by_id(id, verbose = FALSE)
#' @param id numeric. One or more dataset ids.
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The dataset metadata records.
dataset_by_id <- function(id, verbose = FALSE) {
  datalist <- list()

  for (i in seq_along(id)) {
    result <- http_request("GET", paste0("resource/", id[i]), NULL)

    if (verbose) {
      log_request(result)
    }
    warn_for_status(result)
    text <- content(result, "text", encoding="UTF-8")
    res <- fromJSON(text, simplifyVector=TRUE)

    dataset <- extract_dataset(res)

    datalist[[i]] <- dataset
    log_progress(i, length(id))
  }
  cat("\n")
  data <- bind_rows(datalist)
  return(data)
}

#' Fetch dataset metadata.
#'
#' \code{dataset} returns dataset metadata based on the id or some query parameters.
#'
#' @usage dataset(id = NULL, scientificname = NULL, q = NULL, obisid = NULL, aphiaid =
#'   NULL, groupid = NULL, areaid = NULL, nodeid = NULL, startdate = NULL, enddate = NULL,
#'   startdepth = NULL, enddepth = NULL, geometry = NULL, verbose = FALSE)
#' @param id integer vector. One or more dataset ids, if provided all the other query
#'   parameters will be ignored.
#' @param scientificname character vector. The full scientific name, with authorship and
#'   date information if known.
#' @param q character. The search string.
#' @param obisid integer vector. The OBIS identifier of the species.
#' @param aphiaid integer vector. The WoRMS identifier of the species.
#' @param groupid integer. The taxonomic group id. See also \code{\link{group}} for the
#'   list of taxonomic groups.
#' @param nodeid integer vector. The OBIS node identifier. See also \code{\link{node}} for
#'   the list of nodes.
#' @param areaid integer vector. The OBIS area identifier (country, marine world heritage
#'   site, ABNJ, EBSA, ...). See also \code{\link{area}} for the list areas.
#' @param startdate The earliest date on which the Event occurred.
#' @param enddate The latest date on which the Event occurred.
#' @param startdepth The minimum depth below the sea surface.
#' @param enddepth The maximum depth below the sea surface.
#' @param geometry A wkt geometry string.
#' @param verbose logical. Optional parameter to enable verbose logging (default =
#'   \code{FALSE}).
#' @examples
#' datasets <- dataset(id = 1)
#' datasets <- dataset(q = "MICROBIS")
#' datasets <- dataset(nodeid = c(0,1))
#' @return The dataset metadata records.
#' @export
dataset <- function(id = NULL, scientificname = NULL, q = NULL,
                    obisid = NULL, aphiaid = NULL,
                    groupid = NULL, areaid = NULL, nodeid = NULL,
                    startdate = NULL, enddate = NULL,
                    startdepth = NULL, enddepth = NULL,
                    geometry = NULL, verbose = FALSE) {

  query <- list(scientificname = handle_vector(scientificname),
                q = q,
                obisid = handle_vector(obisid),
                aphiaid = handle_vector(aphiaid),
                groupid = groupid,
                areaid = handle_vector(areaid),
                nodeid = handle_vector(nodeid),
                startdate = handle_date(startdate),
                enddate = handle_date(enddate),
                startdepth = startdepth,
                enddepth = enddepth,
                geometry = geometry)

  if(!is.null(id)) {
    if(all(vapply(query, is.null, TRUE))) {
      return(dataset_by_id(id, verbose))
    } else {
      stop("robis::dataset Either the dataset id or the other query parameters should be provided but not both")
    }
  } else {
    return(simple_paged("resource", verbose,
                        query=query, resultsfn = extract_dataset))
  }
}
