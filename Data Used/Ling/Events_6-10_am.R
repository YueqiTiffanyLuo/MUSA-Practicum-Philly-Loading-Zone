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
library(lubridate)

zone_coords <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/zoneCoords.csv")

#view(zone_coords)

booking_data <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/events_export_CDS.csv")

all_events <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data_times.geojson")

all_events<-filter(all_events, EventType=="booking")

# Extract week of the year
all_events$new_week <- week(all_events$CreateTime)

# Extract day of the week
all_events$new_day_of_week <- wday(all_events$CreateTime, label = TRUE)

# Extract hour of the day
all_events$new_hour <- hour(all_events$CreateTime)

all_day_zones <- filter(all_events, SmartZoneNumber %in% c(21,20,19,18,17,16,13)) %>%
  filter(hour_of_day %in% c(6,7,8,9)) %>%
  filter(day_of_week %in% c("Mon","Tue","Wed","Thu","Fri"))
  

business_day_zones <- filter(all_events, SmartZoneNumber %in% c(15,14)) %>%
  mutate(hour_of_day=hour_of_day-1) %>%
  filter(hour_of_day %in% c(6,7,8,9)) %>%
  filter(day_of_week %in% c("Mon","Tue","Wed","Thu","Fri"))

morning_zones <- filter(all_events, SmartZoneNumber %in% c(12,11,10,9,8,7,6,5,4,3,2,1)) %>%
  filter(hour_of_day %in% c(6,7,8,9)) %>%
  filter(day_of_week %in% c("Mon","Tue","Wed","Thu","Fri"))

combined_zones <- rbind(all_day_zones, business_day_zones, morning_zones) %>%
  dplyr::select(c("SmartZoneName","new_week","day_of_week","hour_of_day")) %>%
  group_by(SmartZoneName, new_week, day_of_week, hour_of_day) %>%
  tally() %>%
  rename(Week = new_week,
         Events = n)

road_nn <- st_read("C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/bookings_road_class_nn.geojson")

variables <- read_csv("C:/Users/14145/Box/Practicum_Philly_2024/Ling/updated_04_01_panel.csv") %>%
  distinct(., SmartZoneName, .keep_all=TRUE)

panel <- merge(variables, road_nn, by="SmartZoneName") %>%
  merge(combined_zones, ., by="SmartZoneName") %>%
  rename(Road_class = CLASS_2) %>%
  dplyr::select(-c("curb_zone_id","week","Day","bookings","Weekend_dummy","Chestnut_St","Walnut_St","Sansom_St","Broad_St","Bike_Network_dummy","east_bound","west_bound","two_way_north_south","geometry.x","geometry.y"))

st_write(panel, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/morning_only_panel.geojson")


top_at_9 <- panel %>%
  group_by(hour_of_day, SmartZoneName) %>%
  summarize(Bookings = sum(Events)) %>%
  filter(hour_of_day==9) %>%
  arrange(., -Bookings) %>%
  slice(1:5)

top_at_8 <- panel %>%
  group_by(hour_of_day, SmartZoneName) %>%
  summarize(Bookings = sum(Events)) %>%
  filter(hour_of_day==8) %>%
  arrange(., -Bookings) %>%
  slice(1:5)

top_at_7 <- panel %>%
  group_by(hour_of_day, SmartZoneName) %>%
  summarize(Bookings = sum(Events)) %>%
  filter(hour_of_day==7) %>%
  arrange(., -Bookings) %>%
  slice(1:5)

top_at_6 <- panel %>%
  group_by(hour_of_day, SmartZoneName) %>%
  summarize(Bookings = sum(Events)) %>%
  filter(hour_of_day==6) %>%
  arrange(., -Bookings) %>%
  slice(1:5)

top_sites <- rbind(top_at_6, top_at_7, top_at_8, top_at_9) %>%
  group_by(SmartZoneName) %>%
  tally()

mapview(top_sites, cex='n', zcol='n', layer.name='Number of Hours as Busiest Zone', alpha=0.7)



















