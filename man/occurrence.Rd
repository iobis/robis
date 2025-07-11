% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occurrence.R
\name{occurrence}
\alias{occurrence}
\title{Find occurrence records}
\usage{
occurrence(scientificname = NULL, taxonid = NULL, datasetid = NULL,
  nodeid = NULL, instituteid = NULL, areaid = NULL, startdate = NULL, enddate = NULL,
  startdepth = NULL, enddepth = NULL, geometry = NULL,
  measurementtype = NULL, measurementtypeid = NULL, measurementvalue = NULL,
  measurementvalueid = NULL, measurementunit = NULL, measurementunitid = NULL,
  redlist = NULL, hab = NULL, wrims = NULL, extensions = NULL, hasextensions = NULL,
  mof = NULL, dna = NULL, absence = NULL, event = NULL, dropped = NULL,
  flags = NULL, exclude = NULL, fields = NULL, qcfields = NULL, q=NULL, verbose = FALSE)
}
\arguments{
\item{scientificname}{the scientific name.}

\item{taxonid}{the taxon identifier (WoRMS AphiaID).}

\item{datasetid}{the dataset identifier.}

\item{nodeid}{the OBIS node identifier.}

\item{instituteid}{the OBIS institute identifier.}

\item{areaid}{the OBIS area identifier.}

\item{startdate}{the earliest date on which occurrence took place.}

\item{enddate}{the latest date on which the occurrence took place.}

\item{startdepth}{the minimum depth below the sea surface.}

\item{enddepth}{the maximum depth below the sea surface.}

\item{geometry}{a WKT geometry string.}

\item{measurementtype}{the measurement type to be included in the measurements data.}

\item{measurementtypeid}{the measurement type ID to be included in the measurements data.}

\item{measurementvalue}{the measurement value to be included in the measurements data.}

\item{measurementvalueid}{the measurement value ID to be included in the measurements data.}

\item{measurementunit}{the measurement unit to be included in the measurements data.}

\item{measurementunitid}{the measurement unit ID to be included in the measurements data.}

\item{redlist}{include only IUCN Red List species.}

\item{hab}{include only IOC-UNESCO HAB species.}

\item{wrims}{include only WRiMS species.}

\item{extensions}{which extensions to include (e.g. MeasurementOrFact, DNADerivedData, default = \code{NULL}).}

\item{hasextensions}{which extensions need to be present (e.g. MeasurementOrFact, DNADerivedData, default = \code{NULL}).}

\item{mof}{include measurements data (default = \code{NULL}).}

\item{dna}{include DNA data (default = \code{NULL}).}

\item{absence}{only include absence records (\code{TRUE}), exclude absence records (\code{NULL}) or include absence records (\code{include}).}

\item{event}{only include pure event records (\code{TRUE}), exclude pure event records (\code{NULL}) or include event records (\code{include}).}

\item{dropped}{only include dropped records (\code{TRUE}), exclude dropped records (\code{NULL}) or include dropped records (\code{include}).}

\item{flags}{quality flags which need to be set.}

\item{exclude}{quality flags to be excluded from the results.}

\item{fields}{fields to be included in the results.}

\item{qcfields}{include lists of missing and invalid fields (default = \code{NULL}).}

\item{q}{text search (experimental).}

\item{verbose}{logical. Optional parameter to enable verbose logging (default = \code{FALSE}).}
}
\value{
The occurrence records in a \code{tibble} format. The number of columns will vary according to the number of information associated with the records.
For more information about the type of data available with OBIS occurrences, see the \href{https://manual.obis.org/}{OBIS manual.}
}
\description{
Download occurrence records from OBIS. A set of filters can be used to delimit
which data will be downloaded. Note that scientificname can be a species name or any other taxon name
(e.g. a family name).
}
\note{
When using a \code{geometry}, the function will fail if the geometry is too complex. One alternative is to use a bounding box or a simplified version of the geometry.
}
\examples{
records <- occurrence(scientificname = "Abra sibogai")
records <- occurrence(taxonid = 141438, startdate = as.Date("2007-10-10"))
}
