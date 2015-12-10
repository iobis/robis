.url <- function() {
  # options(obisclient_url) <- "http://127.0.0.1:8090/"
  getOption("obisclient_url", "http://api.iobis.org/")
}

handle_date <- function(date) {
  if(!is.null(date) && class(date) == "Date") {
    as.character(date)
  } else {
    date
  }
}

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
#' @return The occurrence records.
#' @examples
#' occurrence(scientificname = "Abra sibogai")
#' occurrence(scientificname = "Abra sibogai", qc = c(1:6, 27))
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
  verbose = FALSE) {

  if(!is.null(year) && is.na(as.numeric(year))) {
    warning(paste("Invalid year:", year))
    year <- NULL
  }
  if(!is.null(qc)) {
    qc <- setdiff(qc, c(8,9,20)) ## ignore QC 8,9,20 (NOT IMPLEMENTED)
    qc <- qc[qc > 1 & qc <= 30] ## restrict to valid qcnumbers range
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
                  startdate = handle_date(startdate),
                  enddate = handle_date(enddate),
                  geometry = geometry,
                  qc = qc,
                  offset = format(offset, scientific=FALSE))

    result <- httr::GET(.url(), httr::user_agent("obisclient - https://github.com/iobis/obisclient"),
                        path = "occurrence", query = query)
    httr::stop_for_status(result)
    if (verbose) {
      cat(result$request$url, "\n")
    }
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
      }
      cat("\rRetrieved ", total, " records of ", res$count, " (", floor(total/res$count*100),"%)", sep="")
      i <- i + 1
    }
  }
  cat("\n")
  data <- rbind.fill(datalist)
  return(data)
}

