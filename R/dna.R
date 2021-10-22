#' Extract DNA records from occurrence data with a dna column.
#'
#' @usage dna_records(df, fields = "id")
#' @param df the occurrence dataframe.
#' @param fields columns from the occurrence dataframe to include.
#' @return The DNA records.
#' @export
dna_records <- function(df, fields = "id") {
  .Deprecated("unnest_extension")
  unnest_extension(df, "DNADerivedData", fields)
}
