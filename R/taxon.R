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
taxon <- function(
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
    qc <- setdiff(qc, 9) ## ignore QC 9 (NOT IMPLEMENTED)
    qc <- qc[qc > 1 & qc <= 30] ## restrict to valid qcnumbers range
    qc <- paste0(qc, collapse = ",")
  }

  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()

  while (!lastpage) {
    body <- list(scientificname = scientificname,
                  year = year,
                  obisid = obisid,
                  aphiaid = aphiaid,
                  startdate = startdate,
                  enddate = enddate,
                  geometry = geometry,
                  qc = qc,
                  offset = format(offset, scientific=FALSE))

    result <- httr::POST(.url(), httr::user_agent("obisclient - https://github.com/iobis/obisclient"),
                        path = "taxon", body = body)
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
      datalist[[i]] <- res$results
      total <- total + nrow(res$results)

      cat("\rRetrieved ", total, " records of ", res$count, " (", floor(total/res$count*100),"%)", sep="")
      i <- i + 1
    }
  }
  cat("\n")
  data <- rbind.fill(datalist)
  return(data)
}

