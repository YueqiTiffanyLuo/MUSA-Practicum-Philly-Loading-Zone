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
library(FNN)
library(osmdata)

zones <- read_csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/zoneCoords.csv") %>%
  separate(., coords, into = c("latitude", "longitude"), sep = ", ", convert = TRUE) %>%
  st_as_sf(., coords=c("longitude", "latitude"), crs=4326)

OMF_shp <- st_read("C:/Users/14145/Downloads/regulations/regulations.shp")

OMF_shp <- st_as_sf(OMF_shp, sf_column_name="geometry", crs=4326)

nearest_indices <- st_nearest_feature(zones, OMF_shp)

# Extract the "ID" column from the nearest feature
zones_w_OMF <- OMF_shp[nearest_indices, ] %>%
  cbind(zones, .) %>%
  rename(OMF_curb_geometry = geometry.1) %>%
  mutate(OMF_curb_geometry = as.character(OMF_curb_geometry)) %>%
  cbind(., nearest_indices)

st_write(zones_w_OMF, "C:/Users/14145/Box/Practicum_Philly_2024/Data/zones_with_OMF_vars.geojson")
