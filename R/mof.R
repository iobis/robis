#' Extract measurements or facts from occurrence data with a mof column.
#'
#' @usage measurements(df, fields = "id")
#' @param df the occurrence dataframe.
#' @param fields columns from the occurrence dataframe to include.
#' @return The measurements.
#' @export
measurements <- function(df, fields = "id") {
  .Deprecated("unnest_extension")
  unnest_extension(df, "MeasurementOrFact", fields)
}
