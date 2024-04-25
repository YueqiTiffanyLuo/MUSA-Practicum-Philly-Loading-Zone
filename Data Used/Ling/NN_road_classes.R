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

#Bookings
bookings <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Data/Old_Data_for_Testing/Booking Data_cleaned.geojson") %>%
  st_transform(2272)
all_curbs <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/curbs_road_type/all_curbs_road_type.shp") %>%
  dplyr::select(TARGET_FID, end_st_, strt_s_, strt_nm, CLASS_2, geometry) %>%
  rename(Road_Class = CLASS_2) %>%
  st_transform(2272)

bookings <- bookings %>%
  group_by(SmartZoneName) %>%
  summarize(geometry=first(geometry)) %>%
  st_transform(2272)

road_classes <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/curbs_road_type/simple_road_classes.shp") %>%
  dplyr::select(CLASS, STNAME, geometry) %>%
  st_transform(2272)

roads_class_1 <- filter(road_classes, CLASS == 1)
roads_class_2 <- filter(road_classes, CLASS == 2)
roads_class_3 <- filter(road_classes, CLASS == 3)
roads_class_4 <- filter(road_classes, CLASS == 4)
roads_class_5 <- filter(road_classes, CLASS == 5)

#Nearest neighbor
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

all_curbs$dist_class_1 <- nn_function(st_coordinates(st_centroid(all_curbs)), st_coordinates(st_centroid(roads_class_1)), 1)
all_curbs$dist_class_2 <- nn_function(st_coordinates(st_centroid(all_curbs)), st_coordinates(st_centroid(roads_class_2)), 1)
all_curbs$dist_class_3 <- nn_function(st_coordinates(st_centroid(all_curbs)), st_coordinates(st_centroid(roads_class_3)), 1)
all_curbs$dist_class_4 <- nn_function(st_coordinates(st_centroid(all_curbs)), st_coordinates(st_centroid(roads_class_4)), 1)
all_curbs$dist_class_5 <- nn_function(st_coordinates(st_centroid(all_curbs)), st_coordinates(st_centroid(roads_class_5)), 1)




# Write to GeoJSON file
original_all_curbs <- read.csv2("C:/Users/14145/Box/Practicum_Philly_2024/Michael/all_curbs_with_road_class.csv", sep=",")

all_curbs <- merge(all_curbs, original_all_curbs, by="TARGET_FID") %>%
  dplyr::select(-c("CLASS","CLASS_2"))

st_write(all_curbs, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/all_curbs_0411.geojson")




#FOR pilot zones

bookings$dist_class_1 <- nn_function(st_coordinates(st_centroid(bookings)), st_coordinates(st_centroid(roads_class_1)), 1)
bookings$dist_class_2 <- nn_function(st_coordinates(st_centroid(bookings)), st_coordinates(st_centroid(roads_class_2)), 1)
bookings$dist_class_3 <- nn_function(st_coordinates(st_centroid(bookings)), st_coordinates(st_centroid(roads_class_3)), 1)
bookings$dist_class_4 <- nn_function(st_coordinates(st_centroid(bookings)), st_coordinates(st_centroid(roads_class_4)), 1)
bookings$dist_class_5 <- nn_function(st_coordinates(st_centroid(bookings)), st_coordinates(st_centroid(roads_class_5)), 1)

st_write(bookings, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/bookings_road_class_nn.geojson")

















