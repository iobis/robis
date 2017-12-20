#' Generate checklists.
#' @usage checklist(scientificname = NULL, year = NULL, obisid = NULL, aphiaid = NULL,
#'   groupid = NULL, resourceid = NULL, areaid = NULL, startdate = NULL, enddate = NULL,
#'   startdepth = NULL, enddepth = NULL, geometry = NULL, qc = NULL, fields = NULL,
#'   verbose = FALSE)
#' @param scientificname character vector. The full scientific name, with authorship and
#'   date information if known.
#' @param year integer vector. The year in which the Event occurred.
#' @param obisid integer vector. The OBIS identifier of the species.
#' @param aphiaid integer vector. The WoRMS identifier of the species.
#' @param groupid integer. The taxonomic group id. See also \code{\link{group}} for the
#'   list of taxonomic groups.
#' @param resourceid integer vector. The dataset identifier. See also
#'   \code{\link{dataset}} for querying the list of datasets.
#' @param areaid integer vector. The OBIS area identifier (country, marine world heritage
#'   site, ABNJ, EBSA, ...). See also \code{\link{area}} for the list of areas.
#' @param startdate The earliest date on which the Event occurred.
#' @param enddate The latest date on which the Event occurred.
#' @param startdepth The minimum depth below the sea surface.
#' @param enddepth The maximum depth below the sea surface.
#' @param geometry A wkt geometry string.
#' @param qc A vector of quality control flags you want to filter on. List of
#'   \link[=qc]{QC flags}.
#' @param fields A vector of field names you want to have returned in order, by default
#'   all fields with values are returned.
#' @param verbose logical. Optional parameter to enable verbose logging (default =
#'   \code{FALSE}).
#' @return The checklist.
#' @examples
#' taxa <- checklist(scientificname = c("Pterois volitans", "Voluta musica"))
#' taxa <- checklist(aphiaid = 137091, year = 2000:2002, fields = c("species", "records"))
#' @seealso \code{\link{occurrence}} \code{\link{dataset}} \code{\link{area}}
#' @export
checklist <- function(
  scientificname = NULL,
  year = NULL,
  obisid = NULL,
  aphiaid = NULL,
  groupid = NULL,
  resourceid = NULL,
  areaid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  qc = NULL,
  fields = NULL,
  verbose = FALSE) {

  if(!is.null(year)) {
    invalid_years <- suppressWarnings(is.na(as.numeric(year)))
    if (any(invalid_years)) {
      warning("Invalid years")
    }
    year <- year[!invalid_years]
    if (length(year) > 0) {
      year <- paste0(year, collapse = ",")
    } else {
      year <- NULL
    }
  }

  if(!is.null(qc)) {
    qc <- setdiff(qc, c(9,20)) # ignore QC 9 and 20 (NOT IMPLEMENTED)
    qc <- qc[qc > 1 & qc <= 30] # restrict to valid qcnumbers range
    qc <- paste0(qc, collapse = ",")
  }

  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()

  while (!lastpage) {
    query <- list(scientificname = handle_vector(scientificname),
                  year = handle_vector(year),
                  obisid = handle_vector(obisid),
                  aphiaid = handle_vector(aphiaid),
                  groupid = groupid,
                  resourceid = handle_vector(resourceid),
                  areaid = handle_vector(areaid),
                  startdate = startdate,
                  enddate = enddate,
                  startdepth = startdepth,
                  enddepth = enddepth,
                  geometry = geometry,
                  qc = qc,
                  offset = format(offset, scientific = FALSE))

    result <- http_request("POST", "taxa", query)

    if (verbose) {
      log_request(result)
    }
    stop_for_status(result)
    text <- content(result, "text", encoding="UTF-8")
    res <- fromJSON(text, simplifyVector=TRUE)

    if(!is.null(res$message)) {
      lastpage <- TRUE
      warning(res$message)
    } else {
      limit <- res$limit
      offset <- offset + limit
      lastpage <- res$lastpage
      datalist[[i]] <- res$results
      total <- total + nrow(res$results)
      log_progress(total, res$count)
      i <- i + 1
    }
  }
  cat("\n")
  data <- bind_rows(datalist)
  data <- handle_fields(data, fields)
  return(data)
}

