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

library(osmdata)

#Bookings
bookings <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data_cleaned.geojson")

bookings$CreateTime <- as.POSIXct(bookings$CreateTime)

# Create new columns
library(lubridate)
bookings$date <- as.Date(bookings$CreateTime)  # Extract date
bookings$day_of_week <- wday(bookings$CreateTime, label = TRUE)  # Extract day of the week
bookings$hour_of_day <- hour(bookings$CreateTime)  # Extract hour of the day

st_write(bookings, "C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data_times.geojson")


bookings %>%
  group_by(day_of_week, SmartZoneName) %>%
  summarize(events = n()) %>%
  ggplot(., aes(day_of_week, log(events), group=SmartZoneName, color=SmartZoneName))+
  geom_line(linewidth=1.5, alpha=0.5)+
  scale_color_viridis_d()+
  theme_minimal()+
  theme(legend.position = "none")
