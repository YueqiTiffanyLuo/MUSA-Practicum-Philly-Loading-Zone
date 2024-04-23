#FOR PILOT ZONES
og_panel <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Michael/first_panel.csv")

new_cats <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Michael/new_categories.csv")

retail <- new_cats$Retail
quick_grocery <- new_cats$Quick.grocery
dining <- new_cats$Dining
housing <- new_cats$Housing
commercial <- new_cats$Commercial
industrial <- new_cats$Industrial
civic <- new_cats$Civic
school <- new_cats$School
healthcare <- new_cats$Healthcare
parking <- new_cats$Parking
attraction <- new_cats$Attraction

og_panel <- og_panel %>%
  mutate(rowID = seq.int(nrow(.)))

retail_only <- og_panel[ , names(og_panel) %in% attraction] %>%
  cbind(og_panel$SmartZoneName) %>%
  distinct(og_panel$SmartZoneName, .keep_all = TRUE)

rownames(retail_only) <- retail_only$`og_panel$SmartZoneName`
retail_only <- as.data.frame(t(retail_only)) %>%
  filter(row_number() <= n()-1)

stacked_df <- stack(retail_only)

# Add a unique identifier to each row within each column
stacked_df <- stacked_df %>%
  group_by(ind) %>%
  ungroup()

# Sort the values within each column and keep only the top three values
sorted_df <- stacked_df %>%
  arrange(ind, values) %>%
  group_by(ind) %>%
  slice(1:3) %>%
  mutate(rank = factor(row_number(), labels = c("attraction_nn1", "attraction_nn2", "attraction_nn3"))) %>%
  ungroup()

# Reshape the dataframe back to the original format
final_df <- spread(sorted_df, rank, values)

# retail_nn <- final_df
# quick_grocery_nn<-final_df
# dining_nn<-final_df
# housing_nn<-final_df
# commercial_nn<-final_df
# industrial_nn<-final_df
# civic<-final_df
# school<-final_df
# healthcare<-final_df
# parking<-final_df
# attraction<-final_df

smart_zones <- as.data.frame(unique(og_panel$SmartZoneName)) %>%
  rename(SmartZoneName = `unique(og_panel$SmartZoneName)`)

df_list <- list(retail_nn, quick_grocery_nn, dining_nn, housing_nn, commercial_nn, industrial_nn,
                civic, school, healthcare, parking, attraction)

merged_df <- smart_zones
for (df in df_list) {
  merged_df <- merge(merged_df, df, by.x = "SmartZoneName", by.y="ind", all.x = TRUE)
}

st_write(merged_df, "C:/Users/14145/Box/Practicum_Philly_2024/Data/zones_with_simple_nns.csv")

first_panel <- st_read("C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/finalpanel.geojson")
first_panel <- first_panel[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,156,157)]
first_panel<-mutate(first_panel,
                    log_events = log10(Total_Events))
first_panel<-merge(first_panel, merged_df, by="SmartZoneName")
st_write(first_panel, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/03-26_first_panel.csv")







zones <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Data/zones_with_simple_nns.csv")
zones$Bike_Network_dummy <- c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

panel_0326 <- read.csv("C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/03-26_first_panel.csv")

road_class <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Michael/first_panel.csv")

zone_road_class <- road_class %>%
  group_by(SmartZoneName) %>%
  summarize(CLASS_2 = first(CLASS_2))

long <- pivot_longer(panel_0326, cols = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), names_to="Day",values_to="bookings") %>%
  dplyr::select(c("SmartZoneName","curb_zone_id","week","Day","bookings"))

long$Weekend_dummy <- ifelse(long$Day %in% c("Sat", "Sun"), 1, 0)

panel_0326 <- panel_0326[,c(1,23:55)]

long <- panel_0326 %>%
  distinct() %>%
  merge(long, ., by="SmartZoneName", all.x=TRUE)

long <- merge(long, zone_road_class, by="SmartZoneName")

bikes <- dplyr::select(zones, c(SmartZoneName, Bike_Network_dummy))

long <- merge(long, bikes, by="SmartZoneName")

st_write(long, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/03_31_panel.csv")








og_panel <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Michael/first_panel.csv")

og_panel <- og_panel[,c(1,24:158)]

og_panel <- og_panel %>%
  distinct()

long <- long[,c(1,2,3,4,5,6,40,41)]

long <- merge(long, og_panel, by="SmartZoneName")

st_write(long, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/detailed_03_31_panel.csv")


















#FOR ALL CURBS
panel <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Michael/all_curbs_nn.geojson")

new_cats <- read.csv("C:/Users/14145/Box/Practicum_Philly_2024/Michael/new_categories.csv")

retail <- new_cats$Retail
quick_grocery <- new_cats$Quick.grocery
dining <- new_cats$Dining
housing <- new_cats$Housing
commercial <- new_cats$Commercial
industrial <- new_cats$Industrial
civic <- new_cats$Civic
school <- new_cats$School
healthcare <- new_cats$Healthcare
parking <- new_cats$Parking
attraction <- new_cats$Attraction

og_panel <- panel %>%
  mutate(rowID = seq.int(nrow(.)))

retail_only <- og_panel[ , names(og_panel) %in% attraction] %>%
  cbind(og_panel$TARGET_FID) %>%
  distinct(og_panel$TARGET_FID, .keep_all = TRUE)

rownames(retail_only) <- retail_only$`og_panel$TARGET_FID`
retail_only <- as.data.frame(t(retail_only)) %>%
  filter(row_number() <= n()-3)

stacked_df <- stack(retail_only)

# Add a unique identifier to each row within each column
# stacked_df <- stacked_df %>%
  # group_by(ind) %>%
  # ungroup()

# Sort the values within each column and keep only the top three values
sorted_df <- stacked_df %>%
  arrange(ind, values) %>%
  group_by(ind) %>%
  slice(1:3) %>%
  mutate(rank = factor(row_number(), labels = c("attraction_nn1", "attraction_nn2", "attraction_nn3"))) %>%
  ungroup()

# Reshape the dataframe back to the original format
final_df <- spread(sorted_df, rank, values)

# retail_nn <- final_df
# quick_grocery_nn<-final_df
# dining_nn<-final_df
# housing_nn<-final_df
# commercial_nn<-final_df
# industrial_nn<-final_df
# civic<-final_df
# school<-final_df
# healthcare<-final_df
# parking<-final_df
# attraction<-final_df

smart_zones <- panel[,c(1:28,164)]

df_list <- list(retail_nn, quick_grocery_nn, dining_nn, housing_nn, commercial_nn, industrial_nn,
                civic, school, healthcare, parking, attraction)

merged_df <- smart_zones
for (df in df_list) {
  merged_df <- merge(merged_df, df, by.x = "TARGET_FID", by.y="ind", all.x = TRUE)
}



st_write(merged_df, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/all_curbs_0415.geojson")

first_panel <- st_read("C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/finalpanel.geojson")
first_panel <- first_panel[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,156,157)]
first_panel<-mutate(first_panel,
                    log_events = log10(Total_Events))
first_panel<-merge(first_panel, merged_df, by="TARGET_FID")
st_write(first_panel, "C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/03-26_first_panel.csv")























