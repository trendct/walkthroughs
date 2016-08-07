
# Bring in the data
stops <- read.csv("data/hamden_stops.csv", stringsAsFactors=FALSE)

# Check and eliminate the rows that don't have location information 
stops <- stops[!is.na(stops$InterventionLocationLatitude),]

# Add a column to identifying a driver as white or a minority
stops$ethnicity <- ifelse(((stops$SubjectRaceCode ==  "W") & (stops$SubjectEthnicityCode =="N")), "White", "Minority")



###Importing the shapefiles

# Bring in the shape files for census tracts

require(rgdal)

# dsn is the folder the shape files are in. layer is the name of the file.
towntracts <- readOGR(dsn="shapes", layer="census_tracts")

# creating a copy
towntracts_only <- towntracts

# turn the shapefile into a dataframe that can be worked on in R
require(maptools)
require(ggplot2)

towntracts <- fortify(towntracts, region="GEOID10")

# So now we have towntracts and towntracts_only
# What's the difference? towntracts is a dataframe and can be seen easily
library(knitr)
kable(head(towntracts))

# It's for analyzing data and performing joins and calculations

# While towntracts_only is a Large SpatialPolygonsDataFrame

# It's for rendering the spatial data in R graphically


###Mapping the data

# We only need the columns with the latitude and longitude
coords <- stops[c("InterventionLocationLongitude", "InterventionLocationLatitude")]

# Making sure we are working with rows that don't have any blanks
coords <- coords[complete.cases(coords),]

library(sp)

# Letting R know that these are specifically spatial coordinates
sp <- SpatialPoints(coords)

# Applying projections to the coordinates so they match up with the shapefile we're joining them with
# More projections information http://trac.osgeo.org/proj/wiki/GenParms 
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

# Rendering the census tracts
plot(towntracts_only)

# Adding the coordinates of the traffic stops
plot(sp, col="red" , add=TRUE)



###Points in a polygon

# Calculating points in a polygon

by_tract <- over(sp, towntracts_only)

# What just happened: Every point in the list now has a corresponding census tract 
kable(head(by_tract, 5))

# Use dplyr to gather up and count how many instances of census tracts there are

require(dplyr)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

# Get rid of the census tracts with no data
by_tract <- by_tract[!is.na(by_tract$GEOID10),]

kable(head(by_tract,5))

# Rename the columns of this datframe so it can be joined to future data
colnames(by_tract) <- c("id", "total")

# Changing the GEOID number to character so it can be joined to future data
by_tract$id <- as.character(by_tract$id)

# Bring in a dataframe that has matches census tract ID numbers to town names
tracts2towns <- read.csv("data/tracts_to_towns.csv", stringsAsFactors=FALSE)

kable(head(tracts2towns, 5))

# Changing the column names so it can be joined to the by_tract dataframe
colnames(tracts2towns) <- c("id", "town_name")

# Changing the GEOID number to character so it can be joined to the by_tract dataframe
tracts2towns$id <- as.character(tracts2towns$id)

# Adding a 0 to the front of the GEOID string because it was originally left out when it was imported
tracts2towns$id <- paste0("0", tracts2towns$id)

# Bringing in a library to deal with strings
library(stringr)

# Eliminating leading and trailing white space just in case
tracts2towns$town_name <- str_trim(tracts2towns$town_name)

# Joining the by_tract dataframe to the tracts2towns dataframe

by_tract <- left_join(by_tract, tracts2towns)

kable(head(by_tract,5))

# Why did we do this? Because Hamden sometimes made traffic stops outside of its jurisdiction. Now we can tell for sure which towns police overextended themselves.


###Making a choropleth

# Now we can finally map it

# Join the by_tract points to polygon dataframe to the original census tracts dataframe
total_map <- left_join(towntracts, by_tract)


require(ggmap)
require(scales)

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where Hamden police conduct traffic stops", fill="")
print(tm_ct)

# Excellent. This is a great start but we have a lot of gray tracts. 
# Let's try again but without the gray.

# Filter out the tracts with NA in the total column

total_map <- subset(total_map, !is.na(total))

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where Hamden police conduct traffic stops", fill="")
print(tm_ct)

# Ok, much better. But we're still unclear which part is Hamden and which are parts of other towns.

townborders <- readOGR(dsn="shapes", layer="ctgeo")
townborders_only <- townborders
townborders<- fortify(townborders, region="NAME10")

# Subset the town borders to just Hamden since that's the department we're looking at
town_borders <- subset(townborders, id=="Hamden")

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  geom_polygon(data = town_borders, aes(x=long, y=lat, group=group, fill=total), color = "black", fill=NA, size=0.5) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where Hamden police conduct traffic stops", fill="")
print(tm_ct)

# That's much clearer


# Before we move on, we need to also calculate the number of minority stops in Hamden census tracts

coords <- subset(stops, ethnicity=="Minority")
coords <- coords[c("InterventionLocationLongitude", "InterventionLocationLatitude")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

m_tract <- over(sp, towntracts_only)

m_tract <- m_tract %>%
  group_by(GEOID10) %>%
  summarise(minority=n())

m_tract <- m_tract[!is.na(m_tract$GEOID10),]
colnames(m_tract) <- c("id", "minority")
m_tract$id <- as.character(m_tract$id)

joined_tracts <- left_join(by_tract, m_tract)
kable(head(joined_tracts))

# Calculating percent of stops that were minorities

joined_tracts$minority_p <- round(joined_tracts$minority/joined_tracts$total*100,2)
kable(head(joined_tracts))



###Importing census data

# We'll be using the censusapi package from Hanna Recht
# https://github.com/hrecht/censusapi
# Some documentation
# http://urbaninstitute.github.io/R-Trainings/accesing-census-apis/presentation/index.html

# If you do not yet have the censusapi package installed, uncomment the lines below and run them.

#install.packages("devtools")
#devtools::install_github("hrecht/censusapi")
library("censusapi")

# Loading my census key from an external script
source("keys.R")

# Replace census_key below with "your_own_key_whatever_it_is"
# Apply for one here http://api.census.gov/data/key_signup.html

race_tracts <- getCensus(name="acs5",
                         vintage=2014,
                         key=census_key,
                         vars=c("NAME", "B02001_001E", "B02001_002E"),
                         region="tract:*", regionin="state:09")

# What did we just do?

# I pulled the following population data for all census tracts in state 09, which is Connecticut

# B02001_001E - Total
# B02001_002E - White alone

kable(head(race_tracts))

# ok, let's clean this up
race_tracts$NAME <- NULL

# Creating a new column for the GEOID that can be joined with the dataframe we already have
race_tracts$id <- paste0(race_tracts$state, race_tracts$county, race_tracts$tract)

# Renaming the column names for clarity
colnames(race_tracts) <- c("state_code", "county_code", "tract_code", "total_pop", "white_pop", "id")




###Calculating disparity

# Making some calculations

# Determining the minority population by subtracting the white population from the total
race_tracts$minority_pop <- race_tracts$total_pop - race_tracts$white_pop

# Now figuring out the percent makeup of each census tract
race_tracts$white_pop_p <- round(race_tracts$white_pop/race_tracts$total_pop*100,2)
race_tracts$minority_pop_p <- round(race_tracts$minority_pop/race_tracts$total_pop*100,2)

kable(head(race_tracts,5))
# Joining the two datframes
joined_tracts <- left_join(joined_tracts, race_tracts)

kable(head(joined_tracts,5))

# Calculating disparity between minority traffic stops and population

joined_tracts$min_disp <- joined_tracts$minority_p - joined_tracts$minority_pop_p

kable(head(joined_tracts[c("tract_code", "min_disp")]))




###Visualizing geographic disparity

mapping_disparity <- left_join(towntracts, joined_tracts)
mapping_disparity <- subset(mapping_disparity, !is.na(min_disp))

# A library for color scales

pm_ct <- ggplot() 
pm_ct <- pm_ct + geom_polygon(data = mapping_disparity, aes(x=long, y=lat, group=group, fill=min_disp/100), color="white", size=.25)
pm_ct <- pm_ct + geom_polygon(data = town_borders, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.5)
pm_ct <- pm_ct + coord_map() 
pm_ct <- pm_ct + scale_fill_distiller(type="seq", trans="reverse", palette = "PuOr", label=percent, breaks=pretty_breaks(n=10), name="Gap") 
pm_ct <- pm_ct + theme_nothing(legend=TRUE) 
pm_ct <- pm_ct + labs(x=NULL, y=NULL, title="Hamden: Minority traffic stops versus population")
pm_ct <- pm_ct + theme(text = element_text(size=15))
pm_ct <- pm_ct + theme(plot.title=element_text(face="bold", hjust=.4))
pm_ct <- pm_ct + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
pm_ct <- pm_ct + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
pm_ct <- pm_ct + theme(legend.key.size = unit(1, "cm"))
print(pm_ct)


### Visualizing geographic disparity

#### With annotations

pm_ct <- ggplot() 
pm_ct <- pm_ct + geom_polygon(data = mapping_disparity, aes(x=long, y=lat, group=group, fill=min_disp/100), color="white", size=.25)
pm_ct <- pm_ct + geom_polygon(data = town_borders, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.5)
pm_ct <- pm_ct + coord_map() 
pm_ct <- pm_ct + scale_fill_distiller(type="seq", trans="reverse", palette = "PuOr", label=percent, breaks=pretty_breaks(n=10), name="Gap") 
pm_ct <- pm_ct + theme_nothing(legend=TRUE) 
pm_ct <- pm_ct + labs(x=NULL, y=NULL, title="Hamden: Minority traffic stops versus population")
pm_ct <- pm_ct + theme(text = element_text(size=15))
pm_ct <- pm_ct + theme(plot.title=element_text(face="bold", hjust=.4))
pm_ct <- pm_ct + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
pm_ct <- pm_ct + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
pm_ct <- pm_ct + theme(legend.key.size = unit(1, "cm"))

# Annotations 

pm_ct <- pm_ct + annotate("segment", x = -72.93, xend = -72.87, y = 41.325, yend = 41.325, colour = "lightblue", size=.5) 
pm_ct <- pm_ct + annotate("point", x = -72.93, y = 41.325, colour = "lightblue", size = 2) 
pm_ct <- pm_ct + annotate("text", x = -72.85, y = 41.325, label = "New Haven", size=5, colour="gray30") 
pm_ct <- pm_ct + annotate("segment", x = -72.89, xend = -72.86, y = 41.375, yend = 41.375, colour = "lightblue", size=.5) 
pm_ct <- pm_ct + annotate("point", x = -72.89, y = 41.375, colour = "lightblue", size = 2) 
pm_ct <- pm_ct + annotate("text", x = -72.845, y = 41.375, label = "Hamden", size=5, colour="gray30") 
pm_ct <- pm_ct + annotate("point", x = -72.83, y = 41.375, colour="white", size=.2) 

print(pm_ct)

