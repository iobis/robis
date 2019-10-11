#' Extract measurements or facts from occurrence data with a mof column.
#'
#' @usage measurements(df, fields = c("id"))
#' @param df the occurrence dataframe.
#' @param fields columns from the occurrence dataframe to include.
#' @return The measurements.
#' @export
measurements <- function(df, fields = c("id")) {
  if ("id" %in% names(df) & "mof" %in% names(df)) {
    if (class(df$mof) == "list") {
      return(unnest(df %>% select(c(fields, "mof")), cols = "mof"))
    } else {
      return(data.frame())
    }
  } else {
    warning("Missing columns id or mof")
    return(NULL)
  }
}
