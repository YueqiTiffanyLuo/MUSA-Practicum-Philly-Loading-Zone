library(tidycensus)
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(raster)
library(RColorBrewer)
library(mapview)
library(leaflet)
library(plotly)
library(ggpmisc)


trip_advisor <- read.csv("C:/Users/14145/Downloads/tripadvisor.csv")

trip_advisor <- trip_advisor[,c(6,9)]


trip_advisor <- trip_advisor %>%
  filter(BMQDV != "")

trip_advisor$BMQDV <- gsub("^\\d+|\\d+$", "", trip_advisor$BMQDV)

trip_advisor$BMQDV <- gsub(". ", "", trip_advisor$BMQDV, fixed=TRUE)

trip_advisor$IiChw <- gsub(" reviews", "", trip_advisor$IiChw, fixed=TRUE)

trip_advisor$IiChw <- as.integer(trip_advisor$IiChw)

trip_advisor <- trip_advisor %>%
  filter(IiChw > 0) %>%
  rename(Restaurant_Name = BMQDV,
         Num_reviews = IiChw)

#L&I Data
licenses <- read.csv("C:/Users/14145/Downloads/business_licenses.csv")

licenses <- licenses %>%
  filter(licensetype %in% c("Food Preparing and Serving (30+ SEATS)",
                            "Food Preparing and Serving",
                            "Food Establishment, Retail Permanent Location",
                            "Food Establishment, Retail Perm Location (Large)",
                            "Food Establishment, Outdoor"))

licenses <- licenses[!is.na(as.numeric(licenses$lat)), ]

licenses <- licenses[!is.na(as.numeric(licenses$lng)), ]

licenses <- licenses %>%
  st_as_sf(., coords = c("lng","lat"), crs=4326, remove=FALSE)

cc_licenses <- licenses %>%
  filter(lat > 39.941 & lat < 39.963) %>%
  filter(lng < -75.135 & lng > -75.211 )

ggplot()+
  geom_sf(data=cc_licenses, size=2, alpha=0.2)

plot1<-leaflet(cc_licenses) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius=3,
                   opacity=0.1,
                   fillOpacity = 0.1,
                   color="darkred",
                   fillColor="darkred",
                   popup=~business_name) %>%
  addPolygons(data=zone_buffers,
              color="grey",
              fillColor="grey",
              opacity=0.5,
              fillOpacity = 0.5,
              popup=~SmartZoneName) %>%
  addCircleMarkers(data=grouped_bookings,
                   radius=1,
                   color="black",
                   opacity=1,
                   popup=~SmartZoneName)

plot1

#Try to join stuff
trip_advisor$Restaurant_Name <- tolower(trip_advisor$Restaurant_Name)

cc_licenses$business_name <- tolower(cc_licenses$business_name)

cc_licenses$business_name <- gsub("\\b(inc|llc|corp|t/a)\\b", "", cc_licenses$business_name)

cc_licenses$business_name <- gsub("\\b(inc|llc|corp|t/a)\\b", "", cc_licenses$business_name)

cc_licenses$business_name <- trimws(cc_licenses$business_name)

trip_advisor$Restaurant_Name <- trimws(trip_advisor$Restaurant_Name)

joined <- merge(cc_licenses, trip_advisor, by.x="business_name", by.y="Restaurant_Name") %>%
  filter(licensestatus == "Active")

plot2<-leaflet(joined) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius=~(Num_reviews/100),
                   color="darkred",
                   opacity=0.5,
                   popup=~business_name)

plot2


#Nearest neighbor for loading zones
zone_coords <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/zoneCoords.csv")

#view(zone_coords)

booking_data <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data(1).csv")

booking_data <- merge(booking_data, zone_coords, by="SmartZoneName")

booking_data <- separate(booking_data, coords, into = c("y", "x"), sep = ", ")

booking_data <- st_as_sf(booking_data, coords=c("x","y"), crs=4326, remove=FALSE)

booking_data <- separate(booking_data, SmartZoneName, into = c("StreetNum", "StreetName"), sep = " ", extra = "drop", remove = FALSE)

bookings <- booking_data %>%
  filter(EventType == "booking")

violations <- booking_data %>%
  filter(ViolationType == "not_authorized")

grouped_bookings <- bookings %>% 
  group_by(StreetName, StreetNum, SmartZoneName, y, x) %>% 
  summarize(events = n()) %>%
  mutate(type = "Booking")

library(FNN)
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

small_rest <- cc_licenses %>%
  filter(licensetype=="Food Establishment, Retail Permanent Location" | licensetype=="Food Preparing and Serving")

large_rest <- cc_licenses %>%
  filter(licensetype=="Food Preparing and Serving (30+ SEATS)" | licensetype=="Food Establishment, Retail Perm Location (Large)")

zone_buffers <- grouped_bookings %>% 
  st_buffer(25)

join <- st_join(small_rest, zone_buffers, join = st_within)

# Count the number of joined objects for each geometry in layer2
count_per_geometry <- table(join$SmartZoneName)

# Add the counts as a new column in layer2
zone_buffers$count_small_rest <- count_per_geometry[match(zone_buffers$SmartZoneName, names(count_per_geometry))]
zone_buffers$count_small_rest[is.na(zone_buffers$count_small_rest)] <- 0
zone_buffers$count_small_rest <- as.integer(zone_buffers$count_small_rest)

join <- st_join(large_rest, zone_buffers, join = st_within)

# Count the number of joined objects for each geometry in layer2
count_per_geometry <- table(join$SmartZoneName)

# Add the counts as a new column in layer2
zone_buffers$count_large_rest <- count_per_geometry[match(zone_buffers$SmartZoneName, names(count_per_geometry))]
zone_buffers$count_large_rest[is.na(zone_buffers$count_large_rest)] <- 0
zone_buffers$count_large_rest <- as.integer(zone_buffers$count_large_rest)


ggplot(data = zone_buffers, aes(x = count_small_rest, y = events)) +
  labs(x="# of small restaurants within 25 m",
       y="# of curb events")+
  ylim(-500,1500)+
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_minimal()

ggplot(data = zone_buffers, aes(x = count_large_rest, y = events)) +
  labs(x="# of large restaurants within 25 m",
       y="# of curb events")+
  ylim(-500,1500)+
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_minimal()

#DISTANCE FROM OTHER PILOT ZONES
# Calculate distance to nearest point
nearest_distance <- st_distance(grouped_bookings, grouped_bookings)

# Get the distance to the nearest point (excluding self)
nearest_distance <- apply(nearest_distance, 1, function(x) min(x[x > 0]))

# Add the nearest distance as a new column to the points geodataframe
grouped_bookings$nearest_zone <- nearest_distance

ggplot(data = grouped_bookings, aes(x = nearest_zone, y = events)) +
  labs(x="Distance to next smart zone (m)",
       y="# of curb events")+
  ylim(-500,1500)+
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  theme_minimal()




