#' Create a Leaflet map.
#'
#' @param data occurrence records you want to map, minimal required fields are
#'   \code{decimalLongitude} and \code{decimalLatitude}.
#' @param popup Column to be used for popups.
#' @return HTML widget object.
#' @seealso \code{\link{occurrence}}
#' @export
leafletmap <- function(data, popup = NULL) {
  m <- leaflet(data) %>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")
  p <- NULL
  if (!is.null(popup)) {
    p <- as.character(data[,popup])
  }
  m <- m %>% addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = p, radius = 3, weight = 1, fillColor = "red", color = "red", opacity = 1, fillOpacity = 0.1)
  return(m)
}
