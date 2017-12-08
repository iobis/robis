#' Fetch dataset metadata.
#' @usage dataset(id, verbose = FALSE)
#' @param id numeric. One or more dataset ids.
#' @param verbose logical. Optional parameter to enable verbose logging (default
#'   = \code{FALSE}).
#' @return The dataset metadata records.
#' @export
dataset <- function(id, verbose = FALSE) {
  datalist <- list()

  for (i in 1:length(id)) {
    result <- http_request("GET", paste0("resource/", id[i]), NULL)

    if (verbose) {
      log_request(result)
    }
    warn_for_status(result)
    text <- content(result, "text", encoding="UTF-8")
    res <- fromJSON(text, simplifyVector=TRUE)

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
    dataset <- dataset[!sapply(dataset, is.null)]

    datalist[[i]] <- dataset
    log_progress(i, length(id))
  }
  cat("\n")
  data <- bind_rows(datalist)
  return(data)
}

