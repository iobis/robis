#' Find occurrences.
#'
#' @usage occurrence(scientificname = NULL, taxonid = NULL, datasetid = NULL,
#'   nodeid = NULL, instituteid = NULL, areaid = NULL, startdate = NULL, enddate = NULL,
#'   startdepth = NULL, enddepth = NULL, geometry = NULL,
#'   measurementtype = NULL, measurementtypeid = NULL, measurementvalue = NULL,
#'   measurementvalueid = NULL, measurementunit = NULL, measurementunitid = NULL,
#'   redlist = NULL, hab = NULL, wrims = NULL, extensions = NULL, hasextensions = NULL,
#'   mof = NULL, dna = NULL, absence = NULL, event = NULL, dropped = NULL,
#'   flags = NULL, exclude = NULL, fields = NULL, qcfields = NULL, verbose = FALSE)
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
#' @param measurementtype the measurement type to be included in the measurements data.
#' @param measurementtypeid the measurement type ID to be included in the measurements data.
#' @param measurementvalue the measurement value to be included in the measurements data.
#' @param measurementvalueid the measurement value ID to be included in the measurements data.
#' @param measurementunit the measurement unit to be included in the measurements data.
#' @param measurementunitid the measurement unit ID to be included in the measurements data.
#' @param geometry a WKT geometry string.
#' @param redlist include only IUCN Red List species.
#' @param hab include only IOC-UNESCO HAB species.
#' @param wrims include only WRiMS species.
#' @param extensions which extensions to include (e.g. MeasurementOrFact, DNADerivedData, default = \code{NULL}).
#' @param hasextensions which extensions need to be present (e.g. MeasurementOrFact, DNADerivedData, default = \code{NULL}).
#' @param mof include measurements data (default = \code{NULL}).
#' @param dna include DNA data (default = \code{NULL}).
#' @param absence only include absence records (\code{TRUE}), exclude absence records (\code{NULL}) or include absence records (\code{include}).
#' @param event only include pure event records (\code{TRUE}), exclude pure event records (\code{NULL}) or include event records (\code{include}).
#' @param dropped only include dropped records (\code{TRUE}), exclude dropped records (\code{NULL}) or include dropped records (\code{include}).
#' @param flags quality flags which need to be set.
#' @param exclude quality flags to be excluded from the results.
#' @param fields fields to be included in the results.
#' @param qcfields include lists of missing and invalid fields (default = \code{NULL}).
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The occurrence records.
#' @examples
#' records <- occurrence(scientificname = "Abra sibogai")
#' records <- occurrence(taxonid = 141438, startdate = as.Date("2007-10-10"))
#' records <- occurrence(taxon = 141438, geometry = "POLYGON ((0 0, 0 45, 45 45, 45 0, 0 0))")
#' @export
occurrence <- function(
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
  measurementtype = NULL,
  measurementtypeid = NULL,
  measurementvalue = NULL,
  measurementvalueid = NULL,
  measurementunit = NULL,
  measurementunitid = NULL,
  redlist = NULL,
  hab = NULL,
  wrims = NULL,
  extensions = NULL,
  hasextensions = NULL,
  mof = NULL,
  dna = NULL,
  absence = NULL,
  event = NULL,
  dropped = NULL,
  flags = NULL,
  exclude = NULL,
  fields = NULL,
  qcfields = NULL,
  verbose = FALSE
) {

  after <- "-1"
  result_list <- list()
  last_page <- FALSE
  i <- 1
  fetched <- 0

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
    measurementtype = measurementtype,
    measurementtypeid = measurementtypeid,
    measurementvalue = measurementvalue,
    measurementvalueid = measurementvalueid,
    measurementunit = measurementunit,
    measurementunitid = measurementunitid,
    redlist = handle_logical(redlist),
    hab = handle_logical(hab),
    wrims = handle_logical(wrims),
    extensions = handle_vector(extensions),
    hasextensions = handle_vector(hasextensions),
    mof = handle_logical(mof),
    dna = handle_logical(dna),
    absence = absence,
    event = event,
    dropped = dropped,
    flags = handle_vector(flags),
    exclude = handle_vector(exclude),
    fields = handle_fields(fields),
    qcfields = handle_logical(qcfields)
  )

  if (getOption("robis_log_usage", TRUE)) {
    http_request("GET", "metrics/logusage", c(query, list(agent = "robis")), verbose)
  }

  total <- NA

  while (!last_page) {

    result <- http_request("GET", "occurrence", c(query, list(
      after = after,
      size = page_size(),
      total = FALSE # needs to be set explicitely to not track counts for subsequent pages
    )), verbose)
    if (is.null(result)) return(invisible(NULL))

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    if (is.na(total)) total <- res$total
    after <- res$results$id[nrow(res$results)]

    if (!is.null(res$results) && is.data.frame(res$results) && nrow(res$results) > 0) {

      # handle array values

      if ("node_id" %in% names(res$results)) {
        res$results$node_id <- sapply(res$results$node_id, paste0, collapse = ",")
      }
      if ("flags" %in% names(res$results)) {
        res$results$flags <- sapply(res$results$flags, paste0, collapse = ",")
        res$results$flags[res$results$flags == ""] <- NA
      }
      if ("invalid" %in% names(res$results)) {
        res$results$invalid <- sapply(res$results$invalid, paste0, collapse = ",")
        res$results$invalid[res$results$invalid == ""] <- NA
      }
      if ("missing" %in% names(res$results)) {
        res$results$missing <- sapply(res$results$missing, paste0, collapse = ",")
        res$results$missing[res$results$missing == ""] <- NA
      }

      # force class character

      character_cols <- c("sex", "testing")

      res$results <- res$results %>%
        mutate_at(intersect(names(res$results), character_cols), as.character)

      result_list[[i]] <- res$results
      fetched <- fetched + nrow(res$results)
      log_progress(fetched, total)
      i <- i + 1
    } else {
      last_page <- TRUE
    }

  }

  data <- bind_rows(result_list)

  depthFields <- intersect(c("minimumDepthInMeters", "maximumDepthInMeters"), names(data))
  if (length(depthFields) > 0) {
    data$depth <- rowMeans(data[depthFields], na.rm = TRUE)
    data$depth[which(is.nan(data$depth))] <- NA
  }

  return(as_tibble(data))
}
