library(leaflet)

(this <- leaflet(data = cidades) %>%
  addTiles %>%
  addCircleMarkers(~LONGITUDE, ~LATITUDE,
                   radius = .1)
)