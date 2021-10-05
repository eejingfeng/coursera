library(leaflet)

#1.2
my_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lat=39.2980803,lng=-76.5898801,popup = "Jeff's")
my_map


#1.3
df <- data.frame(lat = runif(20,min=39.2,max=39.3),
                 lng = runif(20,min=-76.6,max=-76.5))

my_map <- df%>%
  leaflet() %>% 
  addTiles() %>%
  addMarkers()
my_map

#1.4

nusIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/en/b/b9/NUS_coat_of_arms.svg",
  iconWidth = 31*215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY =16
)

nussites <- "<a href=https://nus.edu.sg/>NUS</a>"

my_map <- df%>%
  leaflet() %>% 
  addTiles() %>%
  addMarkers(icon = nusIcon, popup = nussites)
my_map

#1.5

my_map <- df%>%
  leaflet() %>% 
  addTiles() %>%
  addMarkers(icon = nusIcon, popup = nussites,clusterOptions = markerClusterOptions())
my_map


my_map <- df%>%
  leaflet() %>% 
  addTiles() %>%
  addCircleMarkers()
my_map


my_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lat=1.2966,lng=103.7764,icon=nusIcon,popup = nussites)
my_map

nusIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/en/b/b9/NUS_coat_of_arms.svg",
  iconWidth = 31*215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY =16
)

nussite <- "<a href=https://nus.edu.sg/>NUS</a><br>21 Lower Kent Ridge Rd<br>Singapore 119077"

my_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lat=1.2966,lng=103.7764,icon=nusIcon,popup = nussite)
my_map
