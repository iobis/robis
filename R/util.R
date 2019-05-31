api_url <- function() {
  getOption("robis_api_url", "https://api.obis.org/v3/")
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

http_request <- function(method, path, query) {
  url <- paste0(api_url(), path)
  if (method == "GET") {
    GET(url, user_agent("robis - https://github.com/iobis/robis"), query = query)
  } else if (method == "POST") {
    POST(url, user_agent("robis - https://github.com/iobis/robis"), body = query)
  }
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

log_progress <- function(total, count) {
  pct <- floor(total / count * 100)
  if (pct > 100) pct <- 100
  message(paste0("\rRetrieved ", total, " records of approximately ", count, " (", pct, "%)", sep = ""), appendLF = FALSE)
}
