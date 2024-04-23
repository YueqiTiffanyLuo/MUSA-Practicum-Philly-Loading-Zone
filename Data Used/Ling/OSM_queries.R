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
curbs <- st_read("C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/all_curbs_0411.geojson")
offices <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/offices.geojson")

#Amenities
sf_offices <- data.frame(0,2)


x <- opq(bbox = c(-75.222702,39.935803,-75.130348,39.972647)) %>%
  add_osm_feature(key = 'office', value = offices[3]) %>%
  osmdata_sf ()

x <- bind_rows(x$osm_points, x$osm_lines, x$osm_polygons, x$osm_multipolygons, x$osm_multilines) %>%
  mutate(office_type = offices[3])

sf_offices <- bind_rows(sf_offices, x)

st_write(sf_offices, "C:/Users/14145/Box/Practicum_Philly_2024/Michael/offices.geojson")

sf_amenities <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/amenities.geojson")

sf_amenities <- sf_amenities %>%
  dplyr::select(amenity, geometry) %>%
  st_make_valid()

sf_amenities$geometry <- st_centroid(sf_amenities$geometry)

mapview(sf_amenities, zcol="amenity")

sf_buildings <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/buildings.geojson")

sf_buildings <- sf_buildings %>%
  dplyr::select(building, geometry) %>%
  st_make_valid()

sf_buildings$geometry <- st_centroid(sf_buildings$geometry)

mapview(sf_buildings, zcol="building")

sf_landuse <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/landuse.geojson")

sf_landuse <- sf_landuse %>%
  dplyr::select(land_use, geometry) %>%
  st_make_valid()

#sf_landuse$geometry <- st_centroid(sf_landuse$geometry)
sf_landuse <- sf_landuse[st_geometry_type(sf_landuse$geometry) %in% c("POLYGON", "MULTIPOLYGON"), ]

mapview(sf_landuse, zcol="land_use")

sf_shops <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/shops.geojson")

sf_shops <- sf_shops %>%
  dplyr::select(shop, geometry) %>%
  st_make_valid()

sf_shops$geometry <- st_centroid(sf_shops$geometry)

sf_offices <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/offices.geojson")

sf_offices <- sf_offices %>%
  dplyr::select(office_type, geometry) %>%
  st_make_valid()

sf_offices$geometry <- st_centroid(sf_offices$geometry)

ggplot()+
  geom_sf(data=sf_buildings, aes(color=building))+
  facet_wrap(~building)+
  geom_sf(data=bookings, color="black")

ggplot()+
  geom_sf(data=sf_amenities, aes(color=amenity))+
  facet_wrap(~amenity)+
  geom_sf(data=bookings, color="black")

ggplot()+
  geom_sf(data=sf_landuse, aes(fill=land_use, color="grey"), alpha=0.5)+
  facet_wrap(~land_use)+
  geom_sf(data=bookings, color="black")

ggplot()+
  geom_sf(data=sf_shops, aes(color=shop))+
  facet_wrap(~shop)+
  geom_sf(data=bookings, color="black")

ggplot()+
  geom_sf(data=sf_offices, aes(color=office_type))+
  facet_wrap(~office_type)+
  geom_sf(data=bookings, color="black")




#List of amenities
amenities <- c("bar","cafe","fast_food","pub","restaurant","college","school","university",
               "parking_space","bank","atm","clinic","hospital","pharmacy","community_centre",
               "conference_centre","nightclub","theatre","police","post_box","post_office",
               "place_of_worship")

buildings <- c("apartments","dormitory","hotel","commercial","office","retail","supermarket",
               "warehouse","church","college","government","hospital","public","school",
               "university")

landuse <- c("commercial","education","residential","retail","institutional")

offices <- c("company","consulting","courier","coworking","educational_institution",
             "financial","government","lawyer")

shops <- c("alcohol","bakery","beverages","coffee","convenience","deli","department_store",
           "general","supermarket","clothes","gift")


#Nearest neighbor
sf_amenities <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/amenities.geojson") %>%
  st_transform(2272)
sf_buildings <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/buildings.geojson") %>%
  st_transform(2272)
sf_shops <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/shops.geojson") %>%
  st_transform(2272) %>%
  mutate(shop = ifelse(shop == "coffee", "beverages", shop)) %>%
  mutate(shop = ifelse(shop == "general", "convenience", shop))

sf_offices <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/offices.geojson") %>%
  st_transform(2272)

curbs <- st_transform(curbs, 2272)

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

test <- curbs


# Iterate over unique amenities
unique_amenities <- unique(sf_amenities$amenity)
for (amenity in unique_amenities) {
  print(paste("Processing amenity:", amenity))
  
  # Filter sf_amenities by amenity type
  subset_amenities <- sf_amenities[sf_amenities[[10]] == amenity, ]
  
  print(paste("Number of amenities in subset:", nrow(subset_amenities)))
  
  # Calculate distances between current booking and all amenities of this type
  test[[paste0("nn1_",amenity,"_amenity")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 1)
  test[[paste0("nn2_",amenity,"_amenity")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 2)
  test[[paste0("nn3_",amenity,"_amenity")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 3)
}

#Iterate for unique building types
for (x in buildings) {
  print(paste("Processing building:", x))
  
  # Filter sf_buildings by building type
  subset_buildings <- filter(sf_buildings, building==x)
  
  # Check if the subset is empty
  if (nrow(subset_buildings) == 0) {
    cat("Skipping iteration: Subset is empty\n")
    next  # Skip to the next iteration
  }
  
  print(paste("Number of buildings in subset:", nrow(subset_buildings)))
  
  # Calculate distances between current booking and all amenities of this type
  test[[paste0("nn1_",x,"_building")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 1)
  test[[paste0("nn2_",x,"_building")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 2)
  test[[paste0("nn3_",x,"_building")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 3)
}

#Iterate for unique shop types
for (x in shops) {
  print(paste("Processing shop:", x))
  
  # Filter sf_buildings by shop type
  subset_shops <- filter(sf_shops, shop==x)
  
  # Check if the subset is empty
  if (nrow(subset_shops) == 0) {
    cat("Skipping iteration: Subset is empty\n")
    next  # Skip to the next iteration
  }
  
  print(paste("Number of shops in subset:", nrow(subset_shops)))
  
  # Calculate distances between current booking and all amenities of this type
  test[[paste0("nn1_",x,"_shop")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 1)
  test[[paste0("nn2_",x,"_shop")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 2)
  test[[paste0("nn3_",x,"_shop")]] <- nn_function(st_coordinates(st_centroid(st_make_valid(test))), st_coordinates(st_centroid(st_make_valid(subset_amenities))), 3)
}

#Iterate for unique office types - NOT ENOUGH OBSERVATIONS

#st_write(test, "C:/Users/14145/Box/Practicum_Philly_2024/Michael/all_curbs_nn.geojson")
test <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/zones_nn_3.geojson")

numeric <- test %>%
  dplyr::select(-c("SmartZoneName")) %>%
  st_drop_geometry(.) %>%
  mutate(log10events = log10(events))

#Correlation plot
corplot <- cor(numeric)

cor_df <- as.data.frame(corplot)

# Extract the column values and row names
column_name <- "log10events"  # Replace "mpg" with the column name you want to plot
column_values <- cor_df[[column_name]]
cor_df <- data.frame(variable = rownames(cor_df), correlation = column_values)

cor_df <- filter(cor_df, variable != "events" & variable != "log10events")

# Create a bar plot using ggplot2
library(ggthemes)
ggplot(cor_df, aes(x = reorder(variable, -correlation), y = correlation, fill=correlation)) +
  geom_bar(stat = "identity") +
  labs(title = "Relationships with Log10 Events",
       x = "Nearest Neighbor Variables",
       y = "Correlation") +
  ylim(-1,1)+
  scale_fill_viridis_c()+
  theme_wsj() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

filter(cor_df, abs(correlation) > 0.5) %>%
  ggplot(., aes(x = reorder(variable, -correlation), y = correlation, fill=correlation)) +
  geom_bar(stat = "identity") +
  labs(title = "Strongest Relationships (>0.5) with Log10 Events",
       x = "Nearest Neighbor Variables",
       y = "Correlation") +
  ylim(-1,1)+
  scale_fill_viridis_c()+
  theme_clean() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
ggplotly(p=ggplot2::last_plot())


coords <- matrix(c(
  -75.222702, 39.935803,
  -75.130348, 39.935803,
  -75.130348, 39.972647,
  -75.222702, 39.972647,
  -75.222702, 39.935803
), ncol = 2, byrow = TRUE)

# Create an sf polygon object
polygon <- st_polygon(list(coords))
polygon <- st_sfc(polygon, crs = 4326)  # Set the CRS (if necessary, replace 4326 with your desired CRS)

# Write to GeoJSON file
st_write(polygon, "C:/Users/14145/Box/Practicum_Philly_2024/Michael/OSM_bounding_box.geojson")




















