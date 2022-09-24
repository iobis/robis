use_cache <- function() {
  getOption("robis_use_cache", FALSE)
}

api_url <- function() {
  getOption("robis_api_url", "https://api.obis.org/")
}

page_size <- function() {
  getOption("robis_page_size", 5000)
}

handle_date <- function(date) {
  if (!is.null(date) && class(date) == "Date") {
    return(as.character(date))
  } else {
    return(date)
  }
}

handle_logical <- function(b) {
  if (!is.null(b)) {
    return(tolower(as.character(b)))
  } else {
    return(NULL)
  }
}

handle_vector <- function(x) {
  if (!is.null(x)) {
    return(paste0(x, collapse = ","))
  } else {
    return(x)
  }
}

handle_fields <- function(x) {
  if (!is.null(x)) {
    if (!("id" %in% x)) {
      x <- c("id", x)
    }
    return(paste0(x, collapse = ","))
  } else {
    return(x)
  }
}

handle_request_error <- function(e) {
  message("Error: Failed to connect to the OBIS API. If the problem persists, please contact helpdesk@obis.org.")
  return(invisible(NULL))
}

http_request <- function(method, path, query, verbose=FALSE) {
  if (!curl::has_internet()) {
    message("Error: No internet connection.")
    return(invisible(NULL))
  }
  if (use_cache()) {
    get <- httpcache::GET
    post <- httpcache::POST
  } else {
    get <- httr::GET
    post <- httr::POST
  }
  url <- paste0(api_url(), path)
  if (method == "GET") {
    result <- tryCatch(
      get(url, user_agent("robis - https://github.com/iobis/robis"), query = query),
      error = handle_request_error
    )
  } else if (method == "POST") {
    result <- tryCatch(
      post(url, user_agent("robis - https://github.com/iobis/robis"), body = query),
      error = handle_request_error
    )
  }
  if (!is.null(result)) {
    if (verbose) {
      log_request(result)
    }
    if (httr::http_error(result)) {
      message("Error: The OBIS API was not able to process your request. If the problem persists, please contact helpdesk@obis.org.")
      result <- invisible(NULL)
    }
  }
  return(result)
}

log_request <- function(result) {
  message(paste0("URL: ", result$request$url))
  message(paste0("Status: ", result$status_code))
  message(paste0("Age: ", result$headers$age))
  message(paste0("Time: ", result$times[["total"]]))
}

empty_cols <- function(df) {
  return(sapply(df, function(k) {
    all(is.na(k))
  }))
}

log_progress <- function(count, total) {
  if (total > 0) {
    pct <- floor(count / total * 100)
    if (pct > 100) pct <- 100
  } else {
    pct <- 100
  }
  message(paste0("\rRetrieved ", count, " records of approximately ", total, " (", pct, "%)", sep = ""), appendLF = FALSE)
}

get_dwc_fields <- function(url) {
  cont <- xml2::read_xml(content(GET(url), "text", encoding = "utf-8"))
  cont %>%
    xml_ns_strip() %>%
    xml_find_all("//property") %>%
    xml_attr("name")
}

fast_unnest <- function(dt, fields, column) {
  dt[, unlist(get(column), recursive = FALSE), by = mget(fields)]
}

get_extension_cols = function(extension) {
  if (extension == "DNADerivedData") {
    return(get_dwc_fields("https://rs.gbif.org/extension/gbif/1.0/dna_derived_data_2021-07-05.xml"))
  } else if (extension == "MeasurementOrFact") {
    return(get_dwc_fields("https://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml"))
  }
}

get_extension_cols_cached <- memoise::memoise(get_extension_cols)

utils::globalVariables(c("where", "givenname", "surname", "organization", "name", ":="))

clean_extension_table <- function(df, extension) {
  cols <- get_extension_cols_cached(extension)
  if (is.data.frame(df)) {
    df <- df %>%
      select(where(~!all(is.na(.x))))
    missing_cols <- setdiff(cols, names(df))
    df[missing_cols] <- as.character(NA)
    df %>%
      select(all_of(c(cols, "level")))
  } else {
    NULL
  }
}

#' Extract extension records from occurrence data with nested extension column.
#'
#' @usage unnest_extension(df, extension, fields = "id")
#' @param df the occurrence dataframe.
#' @param extension the extension type (e.g. `MeasurementOrFact`, `DNADerivedData`).
#' @param fields columns from the occurrence dataframe to include.
#' @return The extension records.
#' @export
unnest_extension <- function(df, extension, fields = "id") {
  if (extension == "MeasurementOrFact") {
    column <- "mof"
  } else if (extension == "DNADerivedData") {
    column <- "dna"
  }
  fields <- unique(c("id", fields))
  if ("id" %in% names(df) & column %in% names(df)) {
    if (is.list(df[,column])) {
      dt <- df %>%
        select(all_of(c(fields, column))) %>%
        filter(!sapply(.data[[column]], is.null)) %>%
        mutate({{column}} := lapply(.data[[column]], clean_extension_table, extension)) %>%
        as.data.table()
      dt %>%
        fast_unnest(fields, column) %>%
        as_tibble()
    } else {
      tibble()
    }
  }
}

#' Generate a citation from metadata elements.
#'
#' @usage generate_citation(title, published, url, contacts)
#' @param title the dataset title.
#' @param published the dataset published date.
#' @param url the dataset url.
#' @param contacts the dataset contacts as a dataframe.
#' @return A citation string.
#' @export
generate_citation <- function(title, published, url, contacts) {
  title <- gsub("\\.$", "", title)
  published <- substr(published, 1, 10)
  url <- url
  names_list <- contacts %>%
    select(givenname, surname, organization) %>%
    distinct() %>%
    arrange(surname) %>%
    mutate(givenname = ifelse(!is.na(givenname), paste0(substr(givenname, 1, 1), "."), NA)) %>%
    rowwise() %>%
    mutate(name = ifelse(is.na(surname), organization, paste0(na.omit(c(surname, givenname)), collapse = ", "))) %>%
    pull(name) %>%
    paste0(collapse = ", ")
  names_list <- gsub("\\.$", "", names_list)
  glue("{names_list}. {title}. Published {published}. {url}.")
}
