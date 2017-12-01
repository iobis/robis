#' Create a Leaflet map.
#'
#' @param data
#' @param popup Column to be used for popups.
#' @return HTML widget object.
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
