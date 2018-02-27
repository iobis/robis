obis_url <- function() {
  getOption("robis_url", "http://api.iobis.org/")
}

max_characters <- function() {
  getOption("robis_max_characters", 2048)
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

handle_fields <- function(data, fields) {
  if (!is.null(fields) & nrow(data) > 0) {
    missing_fields <- setdiff(fields, colnames(data))
    if (length(missing_fields) > 0) {
      warning("Following fields where not found and initialized to NA: ",
              paste0(missing_fields, collapse = ", "))
      data[, missing_fields] <- NA
    }
    # remove fields that were not requested
    for (extra_col in setdiff(colnames(data), fields)) {
      data[, extra_col] <- NULL
    }
    data <- data[, fields, drop = FALSE] # re-order columns to the expected order
  }
  data
}

log_request <- function(result) {
  print(result$request)
  cat(paste0("Status: ", result$status_code))
  cat(paste0("\nAge: ", result$headers$age))
  cat(paste0("\nTime: ", result$times[["total"]]), "\n")
}

log_progress <- function(total, count) {
  cat("\rRetrieved ", total, " records of ", count,
      " (", floor(total / count * 100), "%)", sep = "")
}

http_request <- function(method, path, query) {
  if (method == "GET") {
    GET(obis_url(), user_agent("robis - https://github.com/iobis/robis"),
              path = path, query = query)
  } else if (method == "POST") {
    POST(obis_url(), user_agent("robis - https://github.com/iobis/robis"),
               path = path, body = query)
  }
}


simple_paged <- function(endpoint, verbose, query=list(), resultsfn=identity) {
  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()

  while (!lastpage) {
    query[["offset"]] <- format(offset, scientific = FALSE)

    result <- http_request("POST", endpoint, query)

    if (verbose) {
      log_request(result)
    }
    stop_for_status(result)
    text <- content(result, "text", encoding="UTF-8")
    res <- fromJSON(text, simplifyVector=TRUE)

    if(!is.null(res$message)) {
      lastpage <- TRUE
      warning(res$message)
    } else {
      limit <- res$limit
      offset <- offset + limit
      lastpage <- res$lastpage
      datalist[[i]] <- resultsfn(res$results)
      total <- total + nrow(res$results)
      log_progress(total, res$count)
      i <- i + 1
    }
  }
  cat("\n")
  data <- bind_rows(datalist)
  return(data)
}

