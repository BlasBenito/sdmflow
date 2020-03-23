#plotea las presencias y una variable con leaflet
###################################################
###################################################
plotPresencia <- function(brick = NULL, variable = NULL, lon = NULL, lat = NULL, group = NULL){

  require(leaflet)
  require(raster)
  require(viridis)

  #preparando raster para mapa
  x <- brick[[variable]]
  x.values <- na.omit(raster::values(x))
  pal.raster <- colorNumeric(
    palette = viridis::viridis(100),
    domain = x.values,
    na.color = "transparent"
  )

  #preparando paleta para grupos
  if(is.null(group)){
    group = rep(1, length(x))
  }
  pal.groups <- colorFactor(
    palette = viridis::plasma(length(unique(group))),
    domain = unique(group),
    na.color = "transparent"
  )

  #mapa
  leaflet() %>%
    setView(
      lng = mean(lon),
      lat = mean(lat),
      zoom = 04
    ) %>%
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%
    addRasterImage(
      x,
      colors = pal.raster,
      opacity = 0.4
      ) %>%
    addLegend(
      pal = pal.raster,
      values = x.values,
      title = variable,
      opacity = 1
      ) %>%
    addCircles(
      lng = lon,
      lat = lat,
      weight = 10,
      radius = 10,
      color = pal.groups(group),
      stroke = TRUE,
      opacity = 1,
      fillOpacity = 0.2
      ) %>%
    addLegend(
      pal = pal.groups,
      values = unique(group),
      title = "Taxon",
      opacity = 1
    )

  # addMarkers(
  #   lng = lon,
  #   lat = lat,
  #   group = group)

}

