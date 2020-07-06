#' Extract measurements or facts from occurrence data with a mof column.
#'
#' @usage measurements(df, fields = "id")
#' @param df the occurrence dataframe.
#' @param fields columns from the occurrence dataframe to include.
#' @return The measurements.
#' @export
measurements <- function(df, fields = "id") {
  if ("id" %in% names(df) & "mof" %in% names(df)) {
    if (class(df$mof) == "list") {
      m <- unnest_legacy(
        df %>%
          select(c(fields, "mof")) %>%
          filter(!sapply(.data$mof, is.null))
      , .data$mof)
      return(as_tibble(m))
    } else {
      return(tibble())
    }
  } else {
    warning("Missing columns id or mof")
    return(NULL)
  }
}
