# Load libraries
library(sf)
library(tidyverse)
library(terra)
library(readxl)
library(malariaAtlas)
library(malariaAtlasCovariate)

# 1. 1. Check the elevation data for Tanzania
elevation_tza <- rast("Raw_data/landcover_Elevation/tza_elevation.tif")
res(elevation_tza) # It is 1kmx1km resolution
dim(elevation_tza) # contains one layer
plot(elevation_tza, main = "Tanzania Elevation")

# 1. 2. Check the elevation data for Zambia
elevation_zmb <- rast("Raw_data/landcover_Elevation/zmb_elevation.tif")
res(elevation_zmb) # It is 1kmx1km resolution
dim(elevation_zmb) # contains one layer
plot(elevation_zmb, main = "Zambia Elevation")

# 2. 1. Check the landcover data for Tanzania
landcover_tza <- rast("Raw_data/landcover_Elevation/tza_landcover.tif")
res(landcover_tza) # It is 1kmx1km resolution
dim(landcover_tza) # contains 17 layers (classes)
plot(landcover_tza, main = "Landcover")

# Convert fractional raster to categorical raster (dominant class per pixel)
dominant_landcover_tza <- which.max(landcover_tza)
res(dominant_landcover_tza)
dim(dominant_landcover_tza)
plot(dominant_landcover_tza)

# Assign 1 to vegetation group (forests, shrublands, savannas, grasslands, croplands, mosaic)
dominant_landcover_tza[dominant_landcover_tza %in% c(1:5, 6:7, 8:10, 12, 14)] <- 1

# Assign 2 to water group (wetlands and water)
dominant_landcover_tza[dominant_landcover_tza %in% c(11, 17)] <- 2
# Convert it to factors class
dominant_landcover_tza <- as.factor(dominant_landcover_tza)

# Get current values in the raster
unique_vals <- sort(unique(values(dominant_landcover_tza)))

# Assign new labels
new_labels <- data.frame(
  value = unique_vals,
  label = c(
    "vegetation",  # 1
    "water" ,               # 2
    "urban and built-up",  # 13
    "snow and ice",        # 15
    "barren or sparsely populated"    # 16
  )
)

# You can match the labels manually if needed
levels(dominant_landcover_tza) <- new_labels

# Plot the processed landcover
plot(dominant_landcover_tza, col = c("darkgreen", "lightblue", "gray", "white", "tan"), 
     main = "Tanzania Simplified Land Cover", axes = FALSE)




# 2.2. Check the landcover data for Zambia
landcover_zmb <- rast("Raw_data/landcover_Elevation/zmb_landcover.tif")
res(landcover_zmb) # It is 1kmx1km resolution
dim(landcover_zmb) # contains 17 layers (classes)
plot(landcover_zmb[[17]], main = "Landcover")

# Convert fractional raster to categorical raster (dominant class per pixel)
dominant_landcover_zmb <- which.max(landcover_zmb)
res(dominant_landcover_zmb)
dim(dominant_landcover_zmb)
plot(dominant_landcover_zmb)

# Assign 1 to vegetation group (forests, shrublands, savannas, grasslands, croplands, mosaic)
dominant_landcover_zmb[dominant_landcover_zmb %in% c(1:5, 6:7, 8:10, 12, 14)] <- 1

# Assign 2 to water group (wetlands and water)
dominant_landcover_zmb[dominant_landcover_zmb %in% c(11, 17)] <- 2
# Convert it to factors class
dominant_landcover_zmb <- as.factor(dominant_landcover_zmb)

# Get current values in the raster
unique_vals <- sort(unique(values(dominant_landcover_zmb)))

# Assign new labels
new_labels <- data.frame(
  value = unique_vals,
  label = c(
    "vegetation",  # 1
    "water" ,               # 2
    "urban and built-up",  # 13
    "barren or sparsely populated"    # 16
  )
)

# You can match the labels manually if needed
levels(dominant_landcover_zmb) <- new_labels

# Plot the processed landcover
plot(dominant_landcover_zmb, col = c("darkgreen", "lightblue", "gray", "tan"), 
     main = "Zambia Simplified Land Cover", axes = FALSE)

# Save the processed landcovers
writeRaster(dominant_landcover_tza, "Cleaned_data/landcover_tza2.tif", overwrite = TRUE)
writeRaster(dominant_landcover_zmb, "Cleaned_data/landcover_zmb2.tif", overwrite = TRUE)



# 3.1. Check the roads data for Tanzania
road_tza <- st_read("Raw_data/hotosm_tza_roads_lines_shp/hotosm_tza_roads_lines_shp.shp")
# 3. 2. Check the roads data for Zambia
road_zmb <- st_read("Raw_data/hotosm_zmb_roads_lines_shp/hotosm_zmb_roads_lines_shp.shp")



# 4. 1. check the population data for Tanzania
# NOTE: The population used is here is constrained (population only where buildings exist; source: HDX)
pop_tza <- rast("Raw_data/tza_pop_2025_CN_1km_R2024B_UA_v1.tif")
res(pop_tza) # It is 1kmx1km resolution
dim(pop_tza) # contains one layer (2025 population estimates)
plot(pop_tza, main = "Tanzania Population in 2025") 
# Aggregate the population to 1km resolution
# pop_tza_1km <- aggregate(pop_tza, fact = 10, fun = "sum", na.rm = TRUE)
#plot(pop_tza_1km, main ="Tanzania Population in 2024 (1km)")

# 4. 2. check the population data for Zambia
pop_zmb <- rast("Raw_data/zmb_pop_2025_CN_1km_R2024B_UA_v1.tif")
res(pop_zmb) # It is 1kmx1km resolution
dim(pop_zmb) # contains one layer (2025 population estimates)
plot(pop_zmb, main = "Zambia Population in 2025")
# Aggregate the population to 1km resolution
# pop_zmb_1km <- aggregate(pop_zmb, fact = 10, fun = "sum", na.rm = TRUE)
# plot(pop_zmb_1km, main = "Zambia Population in 2022 (1km)")


# 5. 1. Check Zambia health facilities data
zmb_fac <- read_excel("Raw_data/zmb_facilities.xlsx") %>% # There is 19 different types of facilities.
  select(`MFL Code`, Name, Province, District, Constituency, Ward, Location, Type,
         `Ownership type`, `Operational status`, Accesibility, Longitude, Latitude) %>%
  filter(`Operational status` %in% c("Functional", "Temporarily closure")) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude)) %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) # 115 facilities missed locations
# Change to sf object
zmb_fac_sf <- zmb_fac %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% 
  mutate(inside_zmb = st_within(geometry, zmb_shp, sparse = FALSE) %>% rowSums() > 0) %>% 
  #filter(inside_zmb) # 70 facilities are out of the country.
# Get shapefile for Zambia
zmb_shp <- getShp(ISO = "ZMB", admin_level = "admin2")

# Visualize the facilities in Zambia
ggplot() +
  geom_sf(data = zmb_shp, fill = "white", color = "black") +
  geom_sf(data = zmb_fac_sf, aes(color = `Ownership type`), alpha = 0.5) +
  scale_color_viridis_d()+
  theme_void() +
  labs(title = "Zambia Health Facilities",
       color = "Ownership",
       caption = "Only facilities with location coordinates")

# 5. 2. Check facilities for Tanzania
tza_fac <- read_excel("Raw_data/tza_facilities.xlsx") %>% # There is 19 different types of facilities.
  select(Fac_IDNumber, Name, Region, Council, Ward, Village, FacilityTypeGroup,
         FacilityType, OwnershipGroup, OperatingStatus, Longitude, Latitude) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude)) %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Get shapefile for Tanzania
tza_shp <- st_read("Raw_data/Shapefiles/District_TZ.shp") %>% 
  st_transform(crs = 4326)

# Change to sf object
tza_fac_sf <- tza_fac %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% 
  mutate(inside_tza = st_within(geometry, tza_shp, sparse = FALSE) %>% rowSums() > 0) %>% 
  filter(inside_tza) # 278 facilities are out of the country.



# Visualize the facilities in Tanzania
ggplot() +
  geom_sf(data = tza_shp, fill = "white", color = "black") +
  geom_sf(data = tza_fac_sf, aes(color = OwnershipGroup), alpha = 0.5) +
  scale_color_viridis_d()+
  theme_void() +
  labs(title = "Tanzania Health Facilities",
       color = "Ownership",
       caption = "Only facilities with location coordinates")
