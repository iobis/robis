obis_url <- function() {
  getOption("obisclient_url", "http://api.iobis.org/")
}

max_characters <- function() {
  getOption("obisclient_max_characters", 50000)
}

handle_date <- function(date) {
  if(!is.null(date) && class(date) == "Date") {
    as.character(date)
  } else {
    date
  }
}

handle_vector <- function(x) {
  if(!is.null(x)) {
    paste0(x, collapse = ",")
  } else {
    x
  }
}

log_request <- function(result) {
  cat("\n", paste(result$request$method, result$request$url, result$status_code, result$headers$age), "\n", sep="")
}

log_progress <- function(total, count) {
  cat("\rRetrieved ", total, " records of ", count, " (", floor(total/count*100),"%)", sep="")
}

http_request <- function(method, path, query) {
  if (method == "GET") {
    httr::GET(obis_url(), httr::user_agent("obisclient - https://github.com/iobis/obisclient"),
              path = path, query = query)
  } else if (method == "POST") {
    httr::POST(obis_url(), httr::user_agent("obisclient - https://github.com/iobis/obisclient"),
               path = path, body = query)
  }
}
