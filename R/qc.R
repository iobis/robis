#' Count QC flags set.
#'
#' @param qc The QC value as returned from the \code{\link{occurrence}} method.
#' @param id The QC flag ids.
#' @return Number of QC flags set.
#' @details At the moment there are 27 QC flags, numbered from 1 to 30 with disabled flag (qc 8, 9 and 20).
#' \itemize{
#' \item 1. OBIS data format: are the required fields from the OBIS Schema completed?
#' \item 2. Taxonomy: is the taxon name matched to WoRMS?
#' \item 3. Taxonomy: is the taxon level genus or lower?
#' \item 4. Geography (lat/lon): are the latitude/longitude values different from zero?
#' \item 5. Geography (lat/lon): are the latitude/longitude values within their possible boundaries? (world coordinates)
#' \item 6. Geography (lat/lon): are the coordinates situated in sea or along the coastline (20 km buffer)?
#' \item 7. Completeness (date/time): is the sampling year (start/end) completed and valid?
#' \item 10. OBIS data format: is the 'Basis of Record' documented, and is an existing OBIS code used?
#' \item 11. Completeness (date/time): is the sampling date (year/month/day; start/end) valid?
#' \item 12. Completeness (date/time): if a start and end date are given, is the start before the end?
#' \item 13. Completeness (date/time): if a sampling time is given, is this valid and is the time zone completed?
#' \item 14. Completeness (presence/abundance/biomass): is the value of the field 'ObservedIndividualCount' empty or >0?
#' \item 15. Completeness (presence/abundance/biomass): is the value of the field 'Observedweight' empty or >0?
#' \item 16. Completeness (presence/abundance/biomass): is the field 'SampleSize' completed if the field 'ObservedIndividualCount' is >0?
#' \item 17. OBIS data format: is the value of the field 'Sex' empty or is an existing OBIS code used?
#' \item 18. Geography (depth): is minimum depth <= maximum depth?
#' \item 19. Geography (depth): is the sampling depth possible when compared with GEBCO depth map (incl. margin)?
#' \item 21. Species outliers (environment/depth): is the observation within six MADs from the median depth of this taxon?
#' \item 22. Species outliers (environment/depth): is the observation within three IQRs from the first & third quartile depth of this taxon?
#' \item 23. Species outliers (environment/SSS): is the observation within six MADs from the median sea surface salinity (SSS) of this taxon?
#' \item 24. Species outliers (environment/SSS): is the observation within three IQRs from the first & third quartile sea surface salinity (SSS) of this taxon?
#' \item 25. Species outliers (environment/SST): is the observation within six MADs from the median sea surface temperature (SST) of this taxon?
#' \item 26. Species outliers (environment/SST): is the observation within three IQRs from the first & third quartile sea surface temperature (SST) of this taxon?
#' \item 27. Species outliers (geography): is the observation within six MADs from the distance to the geographic centroid of this taxon?
#' \item 28. Species outliers (geography): is the observation within three IQRs from the first & third quartile distance to the geographic centroid of this taxon?
#' \item 29. Dataset outliers (geography): is the observation within six MADs from the distance to the geographic centroid of this dataset?
#' \item 30. Dataset outliers (geography): is the observation within three IQRs from the first & third quartile distance to the geographic centroid of this dataset?
#' }
#' @references Vandepitte, L., Bosch, S., Tyberghein, L., Waumans, F., Vanhoorne, B., Hernandez, F., ??? Mees, J. (2015). Fishing for data and sorting the catch: assessing the data quality, completeness and fitness for use of data in marine biogeographic databases. Database, 2015, bau125???bau125. \url{http://dx.doi.org/10.1093/database/bau125}
#' @seealso \code{\link{occurrence}} \code{\link{leafletmap}}
#' @export
qcflags <- function(qc, id) {
  mask <- 2^(id-1)
  return(vapply(qc, function(x) {
    return(sum(bitwAnd(x, mask) > 0))
  }, 0))
}

#' Quality control flags
#'
#' @name qc
#' @details At the moment there are 29 QC flags, numbered from 1 to 30 with disabled flag (qc 8, 9 and 20).
#' \itemize{
#' \item 1. OBIS data format: are the required fields from the OBIS Schema completed?
#' \item 2. Taxonomy: is the taxon name matched to WoRMS?
#' \item 3. Taxonomy: is the taxon level genus or lower?
#' \item 4. Geography (lat/lon): are the latitude/longitude values different from zero?
#' \item 5. Geography (lat/lon): are the latitude/longitude values within their possible boundaries? (world coordinates)
#' \item 6. Geography (lat/lon): are the coordinates situated in sea or along the coastline (20 km buffer)?
#' \item 7. Completeness (date/time): is the sampling year (start/end) completed and valid?
#' \item 10. OBIS data format: is the 'Basis of Record' documented, and is an existing OBIS code used?
#' \item 11. Completeness (date/time): is the sampling date (year/month/day; start/end) valid?
#' \item 12. Completeness (date/time): if a start and end date are given, is the start before the end?
#' \item 13. Completeness (date/time): if a sampling time is given, is this valid and is the time zone completed?
#' \item 14. Completeness (presence/abundance/biomass): is the value of the field 'ObservedIndividualCount' empty or >0?
#' \item 15. Completeness (presence/abundance/biomass): is the value of the field 'Observedweight' empty or >0?
#' \item 16. Completeness (presence/abundance/biomass): is the field 'SampleSize' completed if the field 'ObservedIndividualCount' is >0?
#' \item 17. OBIS data format: is the value of the field 'Sex' empty or is an existing OBIS code used?
#' \item 18. Geography (depth): is minimum depth <= maximum depth?
#' \item 19. Geography (depth): is the sampling depth possible when compared with GEBCO depth map (incl. margin)?
#' \item 21. Species outliers (environment/depth): is the observation within six MADs from the median depth of this taxon?
#' \item 22. Species outliers (environment/depth): is the observation within three IQRs from the first & third quartile depth of this taxon?
#' \item 23. Species outliers (environment/SSS): is the observation within six MADs from the median sea surface salinity (SSS) of this taxon?
#' \item 24. Species outliers (environment/SSS): is the observation within three IQRs from the first & third quartile sea surface salinity (SSS) of this taxon?
#' \item 25. Species outliers (environment/SST): is the observation within six MADs from the median sea surface temperature (SST) of this taxon?
#' \item 26. Species outliers (environment/SST): is the observation within three IQRs from the first & third quartile sea surface temperature (SST) of this taxon?
#' \item 27. Species outliers (geography): is the observation within six MADs from the distance to the geographic centroid of this taxon?
#' \item 28. Species outliers (geography): is the observation within three IQRs from the first & third quartile distance to the geographic centroid of this taxon?
#' \item 29. Dataset outliers (geography): is the observation within six MADs from the distance to the geographic centroid of this dataset?
#' \item 30. Dataset outliers (geography): is the observation within three IQRs from the first & third quartile distance to the geographic centroid of this dataset?
#' }
#' @references Vandepitte, L., Bosch, S., Tyberghein, L., Waumans, F., Vanhoorne, B., Hernandez, F., ??? Mees, J. (2015). Fishing for data and sorting the catch: assessing the data quality, completeness and fitness for use of data in marine biogeographic databases. Database, 2015, bau125???bau125. doi:10.1093/database/bau125
#' @seealso \code{\link{qcflags}}
NULL
