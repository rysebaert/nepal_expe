
# Show first 20 rows from the `quakes` dataset
library(leaflet)
leaflet(data = peak_db) %>% addTiles() %>%
  addMarkers(peak_db)

leaflet() %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addLabelOnlyMarkers(data = peak_db,
                      label =  peak_db$PKNAME, 
                      labelOptions = labelOptions(noHide = TRUE, opacity = 0.8,
                                                  style = list("font-size" = "9px")))