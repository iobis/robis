#' Generate checklists.
#'
#' @param scientificname
#' @param year
#' @param obisid
#' @param aphiaid
#' @param resourceid
#' @param startdate
#' @param enddate
#' @param startdepth
#' @param enddepth
#' @param geometry A wkt geometry string.
#' @param qc A vector of qc numbers you want to filter out.
#' @return The checklist.
#' @export
checklist <- function(
  scientificname = NULL,
  year = NULL,
  obisid = NULL,
  aphiaid = NULL,
  resourceid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  qc = NULL,
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
    qc <- setdiff(qc, 9) # ignore QC 9 (NOT IMPLEMENTED)
    qc <- qc[qc > 1 & qc <= 30] # restrict to valid qcnumbers range
    qc <- paste0(qc, collapse = ",")
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
                  resourceid = resourceid,
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
      lastpage = TRUE
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
  return(data)
}

