#' Find occurrences.
#'
#' @param scientificname The full scientific name, with authorship and date
#'   information if known.
#' @param year The year in which the Event occurred.
#' @param obisid The OBIS identifier of the species.
#' @param aphiaid The WoRMS identifier of the species.
#' @param groupid The taxonomic group id. See also \code{\link{group}} for the
#'   list of taxonomic groups.
#' @param resourceid The dataset identifier. See also \code{\link{dataset}} for
#'   querying the list of datasets.
#' @param nodeid The OBIS node identifier. See also \code{\link{node}} for the
#'   list of nodes.
#' @param areaid The OBIS area identifier (country, marine world heritage site,
#'   ABNJ, EBSA, ...). See also \code{\link{area}} for the list areas.
#' @param startdate The earliest date on which the Event occurred.
#' @param enddate The latest date on which the Event occurred.
#' @param startdepth The minimum depth below the sea surface.
#' @param enddepth The maximum depth below the sea surface.
#' @param geometry A wkt geometry string.
#' @param qc A vector of quality control flags you want to filter on. List of
#'   \link[=qc]{QC flags}.
#' @param fields A vector of field names you want to have returned in order, by
#'   default all fields with values are returned.
#' @param verbose logical. Optional parameter to enable verbose logging (default
#'   = \code{FALSE}).
#' @return The occurrence records.
#' @examples
#' records <- occurrence(scientificname = "Abra sibogai")
#' records <- occurrence(aphiaid = 141438, startdate = as.Date("2007-10-10"))
#' records <- occurrence(aphiaid = 141438, geometry = "POLYGON ((0 0, 0 45, 45 45, 45 0, 0 0))")
#' records <- occurrence(scientificname = "Abra sibogai", qc = c(1:6, 27))
#' records <- occurrence(scientificname = "Abra sibogai", fields = c("species", "decimalLongitude", "decimalLatitude"))
#' @seealso \code{\link{leafletmao}} \code{\link{qcflags}}
#'   \code{\link{checklist}} \code{\link{dataset}} \code{\link{area}}
#'   \code{\link{node}} \code{\link{group}}
#' @export
occurrence <- function(
  scientificname = NULL,
  year = NULL,
  obisid = NULL,
  aphiaid = NULL,
  groupid = NULL,
  resourceid = NULL,
  nodeid = NULL,
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
    if (sum(invalid_years) > 0) {
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
    qc <- setdiff(qc, c(8, 9, 20)) # ignore QC 8,9,20 (NOT IMPLEMENTED)
    qc <- qc[qc > 1 & qc <= 30] # restrict to valid qcnumbers range
  }

  skipid = -1
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()

  t <- proc.time()

  while (!lastpage) {
    query <- list(scientificname = handle_vector(scientificname),
                  year = year,
                  obisid = obisid,
                  aphiaid = aphiaid,
                  groupid = groupid,
                  resourceid = resourceid,
                  nodeid = nodeid,
                  areaid = areaid,
                  startdate = handle_date(startdate),
                  enddate = handle_date(enddate),
                  startdepth = startdepth,
                  enddepth = enddepth,
                  geometry = geometry,
                  qc = handle_vector(qc),
                  fields = handle_vector(fields),
                  skipid = format(skipid, scientific = FALSE)
    )

    # use POST for large queries, only GET is cached
    if (sum(nchar(query)) > max_characters()) {
      result <- http_request("POST", "occurrence", query)
    } else {
      result <- http_request("GET", "occurrence", query)
    }

    if (verbose) {
      log_request(result)
    }
    stop_for_status(result)
    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE )

    if(!is.null(res$message)) {
      lastpage = TRUE
      warning(res$message)
    } else {
      limit <- res$limit
      skipid = res$results$id[nrow(res$results)]
      lastpage <- res$lastpage
      if(res$count > 0) {
        datalist[[i]] <- res$results
        total <- total + nrow(res$results)
        log_progress(total, res$count)
        i <- i + 1
      }
    }
  }
  cat("\n")
  if (verbose) {
    cat("Total time:", (proc.time() - t)[["elapsed"]], "seconds\n")
  }

  data <- bind_rows(datalist)

  if (!is.null(fields) & nrow(data) > 0) {
    missing_fields <- setdiff(fields, colnames(data))
    if (length(missing_fields) > 0) {
      warning("Following fields where not found and initialized to NA: ", paste0(missing_fields, collapse = ", "))
      data[, missing_fields] <- NA
    }
    for (extra_col in setdiff(colnames(data), fields)) { # remove fields that were not requested
      data[, extra_col] <- NULL
    }
    data <- data[, fields] # re-order columns to the expected order
  }

  return(data)
}
