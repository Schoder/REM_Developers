# numeriert -----

# Install and load the required packages
#install.packages("leaflet")
library(tidyverse)
library(leaflet)
library(htmlwidgets)

# Define the locations
locations <- tibble(label = c(1:9),
                    fromto = c("1978-1988","1988-1997","1997-1998","1998-2003",
                                "2003-2004","2004-2011","2011-2015","2015-2016","seit 2016"),
                    station = c("ES","BB","FÜ","FR","BO","FR","SA","KN","FR"),
                    lon = c(9.30473, 9.01171, 10.70171, 7.846918978502479, -71.105480, 7.847984460259871, 13.06045, 9.17582,  7.843852101998308),
                    lat = c(48.73961, 48.68212, 47.57143, 47.994849344710005, 42.350790, 47.99580246963386, 47.78846, 47.66033, 47.99017317454657)
)


# Create the map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = 10.5, lat = 48, zoom = 6.5)

# Add markers for each location with labels
m <- m %>% addMarkers(data = locations, lng = ~lon, lat = ~lat, label = ~fromto,
                      labelOptions = labelOptions(noHide = TRUE))

# Add animated polyline connecting the locations
m <- m %>% addPolylines(data = locations, lng = ~lon, lat = ~lat,
                        weight = 3, color = "blue",
                        options = pathOptions(animate = TRUE))
#m
# Save the map as an HTML file
saveWidget(m,xfun::from_root("img","my_map.html"), selfcontained = TRUE)





# alter Code ----
# Install and load the required packages
#install.packages("leaflet")
#install.packages("htmlwidgets")
#library(leaflet)
#library(htmlwidgets)

# Prepare the data for your locations and route
#locations <- data.frame(
#  name = c("Location 1", "Location 2", "Location 3", "Location 4"),
#  lon = c(-75.1639, -71.0589, -81.6944, -95.3698),
#  lat = c(39.9526, 42.3601, 41.8781, 29.7604)
#)

# Create a leaflet map object
#map <- leaflet() %>%
#  addTiles()  # Add the default map tiles

# Add markers for each location
#for (i in 1:nrow(locations)) {
#  map <- addMarkers(map, lng = locations$lon[i], lat = locations$lat[i], label = locations$name[i])
#}

# Create a line representing the route
#route <- locations[, c("lon", "lat")]

# Add the initial route segment
#map <- addPolylines(map, lng = route$lon[1:2], lat = route$lat[1:2], color = "blue")

# Loop to animate the route
#for (i in 2:(nrow(route) - 1)) {
#  Sys.sleep(1)  # Pause for 1 second (adjust as desired)
#  map <- addPolylines(map, lng = route$lon[i:(i+1)], lat = route$lat[i:(i+1)], color = "blue")
#}
#map
# Save the map as an HTML file
#saveWidget(map, "map.html", selfcontained = TRUE)

