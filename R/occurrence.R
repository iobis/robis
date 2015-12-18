#' Find occurrences.
#'
#' @param scientificname
#' @param year
#' @param obisid
#' @param aphiaid
#' @param startdate
#' @param enddate
#' @param geometry A wkt geometry string.
#' @param qc A vector of quality control flags you want to filter on. List of \link[=qc]{QC flags}.
#' @param fields A vector of field names you want to have returned in order, by default all fields with values are returned.
#' @return The occurrence records.
#' @examples
#' records <- occurrence(scientificname = "Abra sibogai")
#' records <- occurrence(aphiaid = 141438, startdate = as.Date("2007-10-10"))
#' records <- occurrence(aphiaid = 141438, geometry = "POLYGON ((0 0, 0 45, 45 45, 45 0, 0 0))")
#' records <- occurrence(scientificname = "Abra sibogai", qc = c(1:6, 27))
#' records <- occurrence(scientificname = "Abra sibogai", fields = c("species", "decimalLongitude", "decimalLatitude"))
#' @export
occurrence <- function(
  scientificname = NULL,
  year = NULL,
  obisid = NULL,
  aphiaid = NULL,
  startdate = NULL,
  enddate = NULL,
  geometry = NULL,
  qc = NULL,
  fields = NULL,
  verbose = FALSE) {

  if(!is.null(year) && is.na(as.numeric(year))) {
    warning("Invalid year: ", year)
    year <- NULL
  }
  if(!is.null(qc)) {
    qc <- setdiff(qc, c(8,9,20)) # ignore QC 8,9,20 (NOT IMPLEMENTED)
    qc <- qc[qc > 1 & qc <= 30] # restrict to valid qcnumbers range
  }

  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()

  while (!lastpage) {
    query <- list(scientificname = scientificname,
                  year = year,
                  obisid = obisid,
                  aphiaid = aphiaid,
                  startdate = handle_date(startdate),
                  enddate = handle_date(enddate),
                  geometry = geometry,
                  qc = handle_vector(qc),
                  fields = handle_vector(fields),
                  offset = format(offset, scientific=FALSE))

    # use POST for complex geometries
    if (!is.null(geometry) && nchar(geometry) > max_characters()) {
      result <- http_request("POST", "occurrence", query)
    } else {
      result <- http_request("GET", "occurrence", query)
    }

    if (verbose) {
      log_request(result)
    }
    httr::stop_for_status(result)
    res <- httr::content(result, simplifyVector=TRUE)

    if(!is.null(res$message)) {
      lastpage = TRUE
      warning(res$message)
    } else {
      limit <- res$limit
      offset <- offset + limit
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
  data <- rbind.fill(datalist)

  if(!is.null(fields)) {
    missing_fields <- setdiff(fields, colnames(data))
    if(length(missing_fields) > 0) {
      warning("Following fields where not found: ", paste0(missing_fields, collapse = ", "))
    }
    for (extra_col in setdiff(colnames(data), fields)) { # remove fields that were not requested
      data[,extra_col] <- NULL
    }
    data <- data[,setdiff(fields, missing_fields)] # re-order columns to the expected order
  }
  return(data)
}
