#' Create a leaflet map.
#'
#' @usage map_leaflet(data, color = "#ff3399",
#'   provider_tiles = "OpenStreetMap.BlackAndWhite", popup = function(x) { x["id"] })
#' @param data the occurrences from \code{occurrence()}.
#' @param color color to be used for the dots.
#' @param provider_tiles the base map provider.
#' @param popup function generating the popup content.
#' @export
map_leaflet <- function(data, color = "#ff3399", provider_tiles = "OpenStreetMap.BlackAndWhite", popup = function(x) { x["id"] }) {
  m <- leaflet(data) %>%
    addProviderTiles(provider_tiles)
  p <- NULL
  if (!is.null(popup)) {
    p <- apply(data, 1, popup)
  }
  m <- m %>% addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = p, radius = 3, weight = 1, fillColor = color, color = color, opacity = 1, fillOpacity = 0.1)
  return(m)
}

#' Create a ggplot2 map.
#'
#' @usage map_ggplot(data, color = "#ff3399")
#' @param data the occurrences from \code{occurrence()}.
#' @param color color to be used for the dots.
#' @export
map_ggplot <- function(data, color = "#ff3399") {
  world <- borders("world", colour = "gray80", fill = "gray80")
  m <- ggplot() +
    world +
    geom_point(data = data, aes_string(x = "decimalLongitude", y = "decimalLatitude"), size = 1.5, stroke = 0.8, alpha = 0.3, colour = color) +
    xlab("longitude") +
    ylab("latitude") +
    coord_quickmap()
}
