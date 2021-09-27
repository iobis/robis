utils::globalVariables("where")

dna_cols <- get_dwc_fields("https://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2021-07-05.xml")

fast_unnest_dna <- function(dt, cols) {
  dna <- NULL
  dt[, unlist(dna, recursive = FALSE), by = mget(cols)]
}

clean_dna_table <- function(m) {
  if (is.data.frame(m)) {
    m <- m %>%
      select(where(~!all(is.na(.x))))
    missing_cols <- setdiff(dna_cols, names(m))
    m[missing_cols] <- as.character(NA)
    m %>%
      select(all_of(c(dna_cols, "level")))
  } else {
    NULL
  }
}

#' Extract DNA records from occurrence data with a dna column.
#'
#' @usage dna_records(df, fields = "id")
#' @param df the occurrence dataframe.
#' @param fields columns from the occurrence dataframe to include.
#' @return The DNA records.
#' @export
dna_records <- function(df, fields = "id") {
  dna <- NULL
  fields <- unique(c("id", fields))
  if ("id" %in% names(df) & "dna" %in% names(df)) {
    if (class(df$dna) == "list") {
      dt <- df %>%
        select(all_of(c(fields, "dna"))) %>%
        filter(!sapply(.data$dna, is.null)) %>%
        mutate(dna = lapply(dna, clean_dna_table)) %>%
        as.data.table()
      dt %>%
        fast_unnest_dna(fields) %>%
        as_tibble()
    } else {
      tibble()
    }
  } else {
    warning("Missing columns id or dna")
    NULL
  }
}
