#' Create a leaflet map.
#'
#' @usage map_leaflet(data, color = "#ff3399",
#'   provider_tiles = "OpenStreetMap.BlackAndWhite", popup = function(x) { x["id"] }, antarctic = FALSE)
#' @param data the occurrences from \code{occurrence()}.
#' @param color color to be used for the dots.
#' @param provider_tiles the base map provider.
#' @param popup function generating the popup content.
#' @param antarctic use antarctic polar stereographic projection.
#' @export
map_leaflet <- function(data, color = "#ff3399", provider_tiles = "OpenStreetMap.BlackAndWhite",
                        popup = function(x) { x["id"] }, antarctic = FALSE) {
  p <- NULL
  if (!is.null(popup)) {
    p <- apply(data, 1, popup)
  }
  if (antarctic) {
    resolutions <- c(48310.14147851562, 24155.07073925781, 12077.535369628906, 6038.767684814453, 3019.3838424072264, 1509.6919212036132, 754.8459606018066, 377.4229803009033, 188.71149015045165, 94.35574507522583, 47.17787253761291, 23.588936268806457, 11.794468134403228, 5.897234067201614, 2.948617033600807, 1.4743085168004035, 0.7371542584002018)
    zoom <- 0
    maxZoom <- 16
    extent <- 12367396.2185
    crsAntartica <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:3031",
      proj4def = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
      resolutions = resolutions,
      origin = c(-extent, extent),
      bounds = list( c(-extent, -extent), c(extent, extent) )
    )
    m <- leaflet(data, options = leafletOptions(crs = crsAntartica, minZoom = 0, maxZoom = 16)) %>%
      setView(0, -90, 0) %>%
      addTiles(
        urlTemplate = "https://tile.gbif.org/3031/omt/{z}/{x}/{y}@1x.png?style=gbif-light",
        attribution = "OpenStreetMap | GBIF",
        layerId = "antartica_tiles",
        options = tileOptions(
          tileSize = 512,
          noWrap = TRUE,
          continuousWorld = TRUE
        )) %>%
      addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = p, radius = 3, weight = 1, fillColor = color, color = color, opacity = 1, fillOpacity = 0.1)
    return(m)
  } else {
    m <- leaflet(data) %>%
      addProviderTiles(provider_tiles) %>%
      addCircleMarkers(~decimalLongitude, ~decimalLatitude, popup = p, radius = 3, weight = 1, fillColor = color, color = color, opacity = 1, fillOpacity = 0.1)
    return(m)
  }
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
