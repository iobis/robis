#' Create a Leaflet map.
#'
#' @param data
#' @return HTML widget object.
#' @export
leafletmap <- function(data) {
  m <- leaflet(data) %>%
    addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
    addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = ~as.character(id), radius = 3, weight = 1, fillColor = "red", color = "red", opacity = 1, fillOpacity = 0.1)
  return(m)
}
