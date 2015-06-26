install.packages("rstudio/leaflet")
library(leaflet)

install.packages("dplyr")
library(dplyr)

# REGULAR NON-PIPING WAY
ct_only <- filter(dunk, state=="CT") # This is taking the dataframe 'dunk' that has all the locations and information on all Dunkin' Donuts across the country and creating a new one by pulling only the rows with 'CT' in the column 'state'
ct_count <- count(ct_only, city) # This is counting the number of city observations and assigning it to the variable 'ct_count'

# USING THE PIPE OPERATOR
ct_count <- dunk %>% filter(state=="CT") %>% count(city) # This is saying take the 'dunk' data frame, filter it, and then count how often 'city' appears

# Note: the filter() and count() functions are from the 'dplyr' package that you loaded earlier

# Making a map with a marker
m <- leaflet() %>%
  addTiles() %>%  
  setView(-72.690940, 41.651426, zoom = 8) %>%
  addMarkers(lng=-72.690940, lat=41.651426, popup="<b>Hello</b><br><a href='http://www.trendct.org'>-TrendCT.org</a>")
m 

# Making a map from a list of latitude and longitude
ct <- read.csv("ctlist.csv", stringsAsFactors=FALSE) # Brings in the file 'ctlist.csv'
# Be sure to first set the working directory in R to where the file is listed

m <- leaflet(ct) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% setView(-72.690940, 41.651426, zoom = 8)
m %>% addCircles(~lng, ~lat, popup=ct$type, weight = 3, radius=40, 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 

# Add a legend to the map
m <- leaflet(ct) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.651426, zoom = 8) %>% 
  addCircles(~lng, ~lat, popup=ct$type, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels="Dunkin'", title="In Connecticut")

m