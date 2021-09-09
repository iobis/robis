mof_cols <- c("measurementID", "occurrenceID", "measurementType", "measurementTypeID", "measurementValue", "measurementValueID", "measurementAccuracy", "measurementUnit", "measurementUnitID", "measurementDeterminedDate", "measurementDeterminedBy", "measurementMethod", "measurementRemarks")

fast_unnest <- function(dt, cols) {
  dt[, unlist(mof, recursive = FALSE), by = mget(cols)]
}

clean_mof_table <- function(m) {
  if (is.data.frame(m)) {
    m <- m %>%
      select(where(~!all(is.na(.x))))
    missing_cols <- setdiff(mof_cols, names(m))
    m[missing_cols] <- as.character(NA)
    m %>%
      select(all_of(c(mof_cols, "level")))
  } else {
    NULL
  }
}

#' Extract measurements or facts from occurrence data with a mof column.
#'
#' @usage measurements(df, fields = "id")
#' @param df the occurrence dataframe.
#' @param fields columns from the occurrence dataframe to include.
#' @return The measurements.
#' @export
measurements <- function(df, fields = "id") {
  fields <- unique(c("id", fields))
  if ("id" %in% names(df) & "mof" %in% names(df)) {
    if (class(df$mof) == "list") {
      dt <- df %>%
        select(all_of(c(fields, "mof"))) %>%
        filter(!sapply(.data$mof, is.null)) %>%
        mutate(mof = lapply(mof, clean_mof_table)) %>%
        as.data.table()
      dt %>%
        fast_unnest(fields) %>%
        as_tibble()
    } else {
      tibble()
    }
  } else {
    warning("Missing columns id or mof")
    NULL
  }
}
