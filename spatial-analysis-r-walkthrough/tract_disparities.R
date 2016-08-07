require(scales)
require(dplyr)
require(gtools)
require(ggplot2)
require(rgdal)
require(ggmap)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
library(devtools)
library(stringr)
library(raster)
library(sp)
library(lubridate)
library(ggplot2)
library(ggalt)
library(leaflet)
library(tidyr)
library(extrafont)
source("keys.R")
#install.packages("devtools")
#devtools::install_github("hrecht/censusapi")
library("censusapi")

stops <- read.csv("data/mega.csv", stringsAsFactors=FALSE)
stops <- stops[!is.na(stops$InterventionLocationLatitude),]

stops$timeofday <- as.POSIXct(as.Date(stops$InterventionTime, origin="1899-12-30"))


stops$ethnicity <- ifelse(((stops$SubjectRaceCode ==  "W") & (stops$SubjectEthnicityCode =="N")), "White", "Minority")
stops$RE <- paste0(stops$SubjectRaceCode, stops$SubjectEthnicityCode)
stops$RE <- gsub("AH", "Hispanic", stops$RE)
stops$RE <- gsub("AM", "Middle_eastern", stops$RE)
stops$RE <- gsub("AN", "Asian", stops$RE)
stops$RE <- gsub("BH", "Black", stops$RE)
stops$RE <- gsub("BM", "Black", stops$RE)
stops$RE <- gsub("BN", "Black", stops$RE)
stops$RE <- gsub("IH", "Indian", stops$RE)
stops$RE <- gsub("IM", "Middle_eastern", stops$RE)
stops$RE <- gsub("IN", "Indian", stops$RE)
stops$RE <- gsub("WH", "Hispanic", stops$RE)
stops$RE <- gsub("WM", "Middle_eastern", stops$RE)
stops$RE <- gsub("WN", "White", stops$RE)

attr(stops$timeofday,"tzone") <- "EST"

towntracts <- readOGR(dsn="shapes", layer="census_tracts")
towntracts_only <- towntracts
towntracts <- fortify(towntracts, region="GEOID10")

tracts2towns <- read.csv("data/tracts_to_towns.csv", stringsAsFactors=FALSE)
colnames(tracts2towns) <- c("id", "town_name")
tracts2towns$id <- as.character(tracts2towns$id)
tracts2towns$id <- paste0("0", tracts2towns$id)
tracts2towns$town_name <- str_trim(tracts2towns$town_name)

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

coords <- stops[c("InterventionLocationLongitude", "InterventionLocationLatitude")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

plot(towntracts_only)
plot(sp, col="red" , add=TRUE)

by_tract <- over(sp, towntracts_only)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

by_tract <- by_tract[!is.na(by_tract$GEOID10),]
colnames(by_tract) <- c("id", "total")
by_tract$id <- as.character(by_tract$id)

by_tract <- left_join(by_tract, tracts2towns)

by_tract <- subset(by_tract, town_name!="Scotland")

adjacent <- read.csv("data/adjacent_search.csv", stringsAsFactors = FALSE)

by_tract <- left_join(by_tract, adjacent)

total_map <- left_join(towntracts, by_tract)

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where traffic stops occur", fill="")
print(tm_ct)


## Minorities versus whites 
gpclibPermit()
gpclibPermitStatus()

towntracts <- readOGR(dsn="shapes", layer="census_tracts")
towntracts_only <- towntracts
towntracts <- fortify(towntracts, region="GEOID10")


tracts2towns <- read.csv("data/tracts_to_towns.csv", stringsAsFactors=FALSE)
colnames(tracts2towns) <- c("id", "town_name")
tracts2towns$id <- as.character(tracts2towns$id)
tracts2towns$id <- paste0("0", tracts2towns$id)
tracts2towns$town_name <- str_trim(tracts2towns$town_name)

# Minority stops
coords <- subset(stops, ethnicity=="Minority")
coords <- coords[c("InterventionLocationLongitude", "InterventionLocationLatitude")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

plot(towntracts_only)
plot(sp, col="red" , add=TRUE)

by_tract <- over(sp, towntracts_only)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

by_tract <- by_tract[!is.na(by_tract$GEOID10),]
colnames(by_tract) <- c("id", "total")
by_tract$id <- as.character(by_tract$id)

by_tract <- left_join(by_tract, tracts2towns)

by_tract <- subset(by_tract, town_name!="Scotland")

adjacent <- read.csv("data/adjacent_search.csv", stringsAsFactors = FALSE)

by_tract <- left_join(by_tract, adjacent)

minority_tracts <- by_tract

# White stops
coords <- subset(stops, ethnicity=="White")
coords <- coords[c("InterventionLocationLongitude", "InterventionLocationLatitude")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)
plot(towntracts_only)
plot(sp, col="red" , add=TRUE)

by_tract <- over(sp, towntracts_only)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

by_tract <- by_tract[!is.na(by_tract$GEOID10),]
colnames(by_tract) <- c("id", "total")
by_tract$id <- as.character(by_tract$id)

by_tract <- left_join(by_tract, tracts2towns)
by_tract <- subset(by_tract, town_name!="Scotland")

adjacent <- read.csv("data/adjacent_search.csv", stringsAsFactors = FALSE)

by_tract <- left_join(by_tract, adjacent)
by_tract <- by_tract[c("id", "total")]
colnames(by_tract) <- c("id", "white")

mw_tract <- left_join(minority_tracts, by_tract)
mw_tract$minority_p <- round(mw_tract$total/(mw_tract$total+mw_tract$white)*100,2)
mw_tract$white_p <- round(mw_tract$white/(mw_tract$total+mw_tract$white)*100,2)

total_map <- left_join(towntracts, mw_tract)

# Black stops
coords <- subset(stops, RE=="Black")
coords <- coords[c("InterventionLocationLongitude", "InterventionLocationLatitude")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)
plot(towntracts_only)
plot(sp, col="red" , add=TRUE)

by_tract <- over(sp, towntracts_only)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

by_tract <- by_tract[!is.na(by_tract$GEOID10),]
colnames(by_tract) <- c("id", "total")
by_tract$id <- as.character(by_tract$id)

by_tract <- left_join(by_tract, tracts2towns)
by_tract <- subset(by_tract, town_name!="Scotland")

adjacent <- read.csv("data/adjacent_search.csv", stringsAsFactors = FALSE)

by_tract <- left_join(by_tract, adjacent)

by_tract <- by_tract[c("id", "total")]
colnames(by_tract) <- c("id", "black")

mw_tract <- left_join(mw_tract, by_tract)

mw_tract$black_p <- round(mw_tract$black/(mw_tract$total+mw_tract$white)*100,2)

total_map <- left_join(towntracts, mw_tract)

# Hispanic stops
coords <- subset(stops, RE=="Hispanic")
coords <- coords[c("InterventionLocationLongitude", "InterventionLocationLatitude")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

plot(towntracts_only)
plot(sp, col="red" , add=TRUE)
by_tract <- over(sp, towntracts_only)

by_tract <- by_tract %>%
  group_by(GEOID10) %>%
  summarise(total=n())

by_tract <- by_tract[!is.na(by_tract$GEOID10),]
colnames(by_tract) <- c("id", "total")
by_tract$id <- as.character(by_tract$id)

by_tract <- left_join(by_tract, tracts2towns)

by_tract <- subset(by_tract, town_name!="Scotland")

adjacent <- read.csv("data/adjacent_search.csv", stringsAsFactors = FALSE)

by_tract <- left_join(by_tract, adjacent)

by_tract <- by_tract[c("id", "total")]
colnames(by_tract) <- c("id", "hispanic")

mw_tract <- left_join(mw_tract, by_tract)

mw_tract$hispanic_p <- round(mw_tract$hispanic/(mw_tract$total+mw_tract$white)*100,2)

total_map <- left_join(towntracts, mw_tract)

# town specific analysis

gpclibPermit()
gpclibPermitStatus()
townborders <- readOGR(dsn="shapes", layer="ctgeo")
townborders_only <- townborders
townborders<- fortify(townborders, region="NAME10")
total_map <- subset(total_map, !is.na(town_name))

town_name <- "East Hartford"
#test_map <- subset(total_map, town_department==town_name)
test_map <- subset(total_map, town_department=="East Hartford")

test_map <- subset(test_map, !is.na(white_p))

test_borders <- subset(townborders, id==town_name)

# Hispanic

pm_ct <- ggplot() 
pm_ct <- pm_ct + geom_polygon(data = test_map, aes(x=long, y=lat, group=group, fill=hispanic_p/100), color="white", size=.25)
pm_ct <- pm_ct + geom_polygon(data = test_borders, aes(x=long, y=lat, group=group), fill=NA, color = "black", size=0.5)
pm_ct <- pm_ct + coord_map() 
pm_ct <- pm_ct + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=9), labels=percent, name="Stops") 
pm_ct <- pm_ct + theme_nothing(legend=TRUE) 
pm_ct <- pm_ct + labs(x=NULL, y=NULL, title=paste("Where Hispanic drivers are pulled over by", town_name, "police"))
pm_ct <- pm_ct + theme(text = element_text(size=15))
pm_ct <- pm_ct + theme(plot.title=element_text(face="bold", hjust=.4))
pm_ct <- pm_ct + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
pm_ct <- pm_ct + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
pm_ct <- pm_ct + annotate("segment", x = -72.58, xend = -72.675, y = 41.815, yend = 41.815, colour = "lightblue", size=.5) 
pm_ct <- pm_ct + annotate("point", x = -72.58, y = 41.815, colour = "lightblue", size = 2) 
pm_ct <- pm_ct + annotate("text", x = -72.71, y = 41.815, label = "South Windsor", size=5, colour="gray30") 
pm_ct <- pm_ct + annotate("segment", x = -72.5, xend = -72.55, y = 41.75, yend = 41.71, colour = "lightblue", size=.5) 
pm_ct <- pm_ct + annotate("point", x = -72.5, y = 41.75, colour = "lightblue", size = 2) 
pm_ct <- pm_ct + annotate("text", x = -72.578, y = 41.71, label = "Manchester", size=5, colour="gray30") 
pm_ct <- pm_ct + annotate("point", x = -72.75, y = 41.71, colour="white", size=.2) 
pm_ct <- pm_ct + theme(legend.key.size = unit(1, "cm"))
pm_ct



# B02001_001E - Total
# B02001_002E - White alone
# B02001_003E - Black alone
# B03001_001E - Hispanic total
# 
race_tracts <- getCensus(name="acs5",
    vintage=2014,
    key=census_key,
    vars=c("NAME", "B02001_001E", "B02001_002E", "B02001_003E", "B03001_001E"),
    region="tract:*", regionin="state:09")

race_tracts$NAME <- NULL
race_tracts$id <- paste0(race_tracts$state, race_tracts$county, race_tracts$tract)
colnames(race_tracts) <- c("state_code", "county_code", "tract_code", "total_pop", "white_pop", "black_pop", "hispanic_pop", "id")
race_tracts$minority_pop <- race_tracts$total_pop - race_tracts$white_pop
race_tracts$white_pop_p <- round(race_tracts$white_pop/race_tracts$total_pop*100,2)
race_tracts$minority_pop_p <- round(race_tracts$minority_pop/race_tracts$total_pop*100,2)
race_tracts$black_pop_p <- round(race_tracts$black_pop/race_tracts$total_pop*100,2)
race_tracts$hispanic_pop_p <- round(race_tracts$black_pop/race_tracts$hispanic_pop*100,2)

#race_tracts$id <- paste0("0", as.character(race_tracts$id))

mw_tract <- left_join(mw_tract, race_tracts)

total_map <- left_join(towntracts, mw_tract)

# percent white population
pm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=white_pop_p), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=white_pop_p), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="White population by tract", fill="")
print(pm_ct)

pm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=minority_pop_p), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=minority_pop_p), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Minority population by tract", fill="")
print(pm_ct)

## DISPARITY

mw_tract$white_disp <- mw_tract$white_p - mw_tract$white_pop_p
mw_tract$min_disp <- mw_tract$minority_p - mw_tract$minority_pop_p
mw_tract$black_disp <- mw_tract$black_p - mw_tract$black_pop_p
mw_tract$hispanic_disp <- mw_tract$hispanic_p - mw_tract$hispanic_pop_p
corr_df <- mw_tract

total_map <- left_join(towntracts, mw_tract)

## Minority disparity
pm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=min_disp), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=min_disp), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Spectral", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Disparity between Minority drivers stopped and census tract population", fill="")
print(pm_ct)

## Black disparity
pm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=black_disp), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=black_disp), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Spectral", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Disparity between Minority drivers stopped and census tract population", fill="")
print(pm_ct)

## Hispanic disparity
pm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=hispanic_disp), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=hispanic_disp), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Spectral", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Disparity between Hispanic drivers stopped and census tract population", fill="")
print(pm_ct)


# geojson?

install.packages("devtools")
devtools::install_github("ropensci/geojsonio")
library("geojsonio")

geojson_json(total_map)


#

mw_tract$majority_pop <- ifelse(mw_tract$minority_pop_p > mw_tract$white_pop_p, "minority", "white")

library(lubridate)
table(stops$InterventionDate)

stops$wday <- gsub(",.*", "", stops$InterventionDate)
stops$Date<- gsub(".*y, ", "", stops$InterventionDate)
stops$Date<- gsub(",.*", "", stops$Date)
stops$Date<- gsub(".* ", "", stops$Date)

table(nchar(stops$Date))

stops$Date2 <- ifelse( (nchar(stops$Date)==2), stops$Date, "" )
stops$Date3 <- ifelse( (nchar(stops$Date)==5), stops$Date, "" )
stops$Date3 <- as.Date(as.numeric(stops$Date3), origin = "1899-12-30")
stops$Date3 <- as.character(stops$Date3)
stops$Date3 <- gsub(".*-", "", stops$Date3)
stops$Date3 <- gsub("-.*", "", stops$Date3)

stops$Date2 <- ifelse(stops$Date2=="", stops$Date3, stops$Date2)

stops$Date2 <- as.numeric(stops$Date2)

ggplot(stops, aes(Date2)) + geom_histogram(binwidth=1) + facet_grid(DepartmentName ~RE)

stops %>%
  group_by(DepartmentName, ethnicity) %>%
  summarise(avg_day=mean(Date2, na.rm=T))
  