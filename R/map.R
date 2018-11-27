#' Create a Leaflet map.
#'
#' @export
map_leaflet <- function(data, popup = NULL) {
  m <- leaflet(data) %>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")
  p <- NULL
  if (!is.null(popup)) {
    p <- apply(data, 1, popup)
  }
  m <- m %>% addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = p, radius = 3, weight = 1, fillColor = "red", color = "red", opacity = 1, fillOpacity = 0.1)
  return(m)
}
