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

zone_coords <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/zoneCoords.csv")

#view(zone_coords)

booking_data <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data(1).csv")

booking_data <- merge(booking_data, zone_coords, by="SmartZoneName")

booking_data <- separate(booking_data, coords, into = c("y", "x"), sep = ", ")

booking_data <- st_as_sf(booking_data, coords=c("x","y"), crs=4326, remove=FALSE)

booking_data <- separate(booking_data, SmartZoneName, into = c("StreetNum", "StreetName"), sep = " ", extra = "drop", remove = FALSE)

st_write(booking_data, "C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data_cleaned.geojson")

bookings <- booking_data %>%
  filter(EventType == "booking")

violations <- booking_data %>%
  filter(ViolationType == "not_authorized")

grouped_bookings <- bookings %>% 
  group_by(StreetName, StreetNum, SmartZoneName, y, x) %>% 
  summarize(events = n()) %>%
  mutate(type = "Booking")

grouped_scans <- booking_data %>% 
  filter(EventType == "scan") %>%
  group_by(StreetName, StreetNum, SmartZoneName, y, x) %>% 
  summarize(events = n()) %>%
  mutate(type = "Scan")

grouped_violations <- violations %>% 
  group_by(StreetName, StreetNum, SmartZoneName, y, x) %>% 
  summarize(events = n()) %>%
  mutate(type = "Violation")

all_events_long <- rbind(grouped_bookings, grouped_violations, grouped_scans)

all_events_wide <- spread(all_events, key="type", value="events")


binpal <- colorBin("viridis", all_events$Violation, 5, pretty = FALSE)

plot1<-leaflet(all_events) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(radius=~(Violation/20),
                   color=~binpal(Violation),
                   fillOpacity = 0.5,
                   group="Violations",
                   popup=~Violation) %>%
  addLegend(pal=binpal,
            values=~Violation,
            title="Violations",
            group="Violations",
            position="bottomleft") %>%
  addCircleMarkers(radius=~(Booking/20),
                   color=~binpal(Booking),
                   fillOpacity = 0.5,
                   group="Bookings") %>%
  addLegend(pal=binpal,
            values=~Booking,
            title="Bookings",
            group="Bookings",
            position="bottomleft") %>%
  addLayersControl(overlayGroups = c("Violations","Bookings"),
                   options = layersControlOptions(collapsed = TRUE))

plot1



#Compaer scans, bookings, and violations
plot2<-ggplot(data=all_events_long, aes(x=SmartZoneName, y=events, fill=type))+
  geom_bar(stat="identity", position="dodge")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplotly(plot2, tooltip=c("type","events"))


#Curb zones import
events_export <- read_csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/events_export_CDS.csv")

left_join(events_export, booking_data, by=c("curb_zone_id","OMFCurbZoneID")) %>%
  dplyr::select(session_type, event_session_id, event_time_start, event_time_end, curb_zone_id, vehicle_length, vehicle_type)


















