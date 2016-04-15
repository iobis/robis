#' Generate taxon lists.
#'
#' @param scientificname
#' @param year
#' @param obisid
#' @param aphiaid
#' @param startdate
#' @param enddate
#' @param geometry A wkt geometry string.
#' @param qc A vector of qc numbers you want to filter out.
#' @return The taxon list.
#' @export
taxa <- function(
  scientificname = NULL,
  year = NULL,
  obisid = NULL,
  aphiaid = NULL,
  startdate = NULL,
  enddate = NULL,
  geometry = NULL,
  qc = NULL,
  verbose = FALSE) {

  if(!is.null(year) && is.na(as.numeric(year))) {
    warning(paste("Invalid year:", year))
    year <- NULL
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
                  startdate = startdate,
                  enddate = enddate,
                  geometry = geometry,
                  qc = qc,
                  offset = format(offset, scientific=FALSE))

    # use POST for complex geometries
    if (!is.null(geometry) && nchar(geometry) > max_characters()) {
      result <- http_request("POST", "taxa", query)
    } else {
      result <- http_request("GET", "taxa", query)
    }

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
  data <- rbind_all(datalist)
  return(data)
}

