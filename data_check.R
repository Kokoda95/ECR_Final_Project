# Load libraries
library(sf)
library(tidyverse)
library(terra)
library(readxl)
library(malariaAtlas)
library(raster)
library(dplyr)

# Check the elevation data for tanzania
srt_tza <- rast("Data Sources/Tanzania_SRTM30meters/Tanzania_SRTM30meters.tif")
res(srt_tza) # It is 30mx30m resolution
dim(srt_tza) # contains one layer
plot(srt_tza, main = "Elevation")

#We loaded and processed road network data for Tanzania and Zambia using OpenStreetMap shapefiles.
#  
# Assigned walking and motor speeds (in km/h) based on the highway type.
#
#Calculated travel friction as seconds per meter using the formula 3.6 / speed.
#
#Identified roads that become impassable during rain based on surface type, road classification, and smoothness.
#
#Updated walking and motor friction under rainy conditions by setting them to Inf for impassable roads.
#
#Reprojected the Tanzania dataset to EPSG:32736 (UTM Zone 36S). (Zambia should be reprojected to EPSG:32735.)

# Check the roads data for Tanzania

#We adjusted walking and motorized travel speeds by applying modifiers based on land cover, slope, and nighttime light availability.
#Starting with baseline speeds by road category, we reduced speeds in areas with challenging land cover types (e.g., forest or wetlands) and steep slopes, which slow movement. 
#To reflect night-time conditions, we further adjusted speeds using satellite-derived nighttime light intensity, reducing walking and motor speeds in poorly lit or unlit areas. 
#These combined adjustments allowed us to compute more realistic friction values (in seconds per meter) for both daytime and nighttime travel scenarios.

road_tza <- st_read("Data Sources/hotosm_tza_roads_lines_shp/hotosm_tza_roads_lines_shp.shp") %>% 
  mutate(
    #  speeds in km/h by highway type - customize as needed
    walk_speed = case_when(
      highway %in% c("path", "footway", "pedestrian", "track") ~ 5,
      highway %in% c("residential", "service") ~ 4,
      highway %in% c("primary", "secondary", "tertiary", "trunk") ~ 6,
      TRUE ~ 3
    ),
    motor_speed = case_when(
      highway %in% c("primary", "secondary", "tertiary", "trunk") ~ 50,
      highway %in% c("residential", "service") ~ 30,
      highway %in% c("track", "unclassified") ~ 15,
      highway %in% c("path", "footway", "pedestrian") ~ 5,
      TRUE ~ 10
    ),
    walk_friction = 3.6 / walk_speed,  # sec per meter
    motor_friction = 3.6 / motor_speed # sec per meter
  )%>%
  mutate(
    impassable_rain = case_when(
      surface %in% c("mud", "sand", "earth", "dirt", "grass", "unpaved", "unoaved") ~ TRUE,
      highway %in% c("track", "path", "abandoned", "construction") ~ TRUE,
      smoothness %in% c("very_bad", "horrible", "very_horrible") ~ TRUE,
      TRUE ~ FALSE
    )
  )%>%
  mutate(
    walk_friction_rain = ifelse(impassable_rain, Inf, walk_friction),
    motor_friction_rain = ifelse(impassable_rain, Inf, motor_friction)
  )

# 2. Transform to projected CRS (example: UTM Zone 36S EPSG:32736, adjust for your area)
road_tza_proj <- st_transform(road_tza, 32736)

#

# Check the roads data for Zambia
road_zmb <- st_read("Data Sources/hotosm_zmb_roads_lines_shp/hotosm_zmb_roads_lines_shp.shx")%>%
  mutate(
    walk_speed = case_when(
      highway %in% c("trunk", "primary") ~ 5,
      highway %in% c("secondary", "secondary_link") ~ 5,
      highway %in% c("tertiary", "tertiary_link") ~ 5,
      highway %in% c("residential") ~ 4.5,
      highway %in% c("track", "path", "footway", "pedestrian") ~ 3,
      highway %in% c("construction", "abandoned") ~ 0,
      TRUE ~ 3
    ),
    motor_speed = case_when(
      highway %in% c("trunk", "primary", "primary_link") ~ 80,
      highway %in% c("secondary", "secondary_link") ~ 60,
      highway %in% c("tertiary", "tertiary_link") ~ 40,
      highway %in% c("residential", "service") ~ 30,
      highway %in% c("track", "path") ~ 5,
      highway %in% c("footway", "pedestrian") ~ 0,
      highway %in% c("construction", "abandoned") ~ 0,
      TRUE ~ 10
    )
  )%>%
  mutate(
    walk_friction = 3.6 / walk_speed,  # sec per meter
    motor_friction = 3.6 / motor_speed # sec per meter
  )%>%
  mutate(
    impassable_rain = case_when(
      surface %in% c("mud", "sand", "earth", "dirt", "grass", "unpaved", "unoaved") ~ TRUE,
      highway %in% c("track", "path", "abandoned", "construction") ~ TRUE,
      smoothness %in% c("very_bad", "horrible", "very_horrible") ~ TRUE,
      TRUE ~ FALSE
    )
  )%>%
  mutate(
    walk_friction_rain = ifelse(impassable_rain, Inf, walk_friction),
    motor_friction_rain = ifelse(impassable_rain, Inf, motor_friction)
  )
library(sf)
library(dplyr)
library(terra)

# 1. Load road data (already includes base speeds per category)
road_data <- st_read("path_to/your_roads_data.shp")

# 2. Load land cover and DEM rasters
landcover_rast <- rast("path_to/landcover.tif")
dem_rast <- rast("path_to/dem.tif")

# 3. Calculate slope from DEM
slope_rast <- terrain(dem_rast, opt = "slope", unit = "degrees")

# 4. Extract raster values to road centroids
road_centroids <- st_centroid(road_data)
road_data$landcover <- terra::extract(landcover_rast, vect(road_centroids))[, 2]
road_data$slope_deg <- terra::extract(slope_rast, vect(road_centroids))[, 2]

# 5. Create land cover modifiers
landcover_mods <- tribble(
  ~landcover, ~landcover_walk_mod, ~landcover_motor_mod,
  1,           1.0,                 1.0,   # Urban
  2,           0.8,                 0.6,   # Grassland
  3,           0.6,                 0.3,   # Forest
  4,           0.9,                 0.7,   # Bare ground
  5,           0.1,                 0.0    # Water/wetland
)

# 6. Categorize slope and assign modifiers
road_data <- road_data %>%
  mutate(
    slope_class = case_when(
      slope_deg <= 5 ~ "flat",
      slope_deg <= 15 ~ "moderate",
      slope_deg <= 30 ~ "steep",
      TRUE ~ "very_steep"
    ),
    slope_walk_mod = case_when(
      slope_class == "flat" ~ 1.0,
      slope_class == "moderate" ~ 0.7,
      slope_class == "steep" ~ 0.4,
      slope_class == "very_steep" ~ 0.2
    ),
    slope_motor_mod = case_when(
      slope_class == "flat" ~ 1.0,
      slope_class == "moderate" ~ 0.8,
      slope_class == "steep" ~ 0.5,
      slope_class == "very_steep" ~ 0.3
    )
  )

# 7. Join land cover modifiers
road_data <- road_data %>%
  left_join(landcover_mods, by = "landcover")

# 8. Apply modifiers to base speeds
road_data <- road_data %>%
  mutate(
    adj_walk_speed = walking_speed * landcover_walk_mod * slope_walk_mod,
    adj_motor_speed = motorized_speed * landcover_motor_mod * slope_motor_mod,
    walk_friction = ifelse(adj_walk_speed > 0, 3.6 / adj_walk_speed, Inf),
    motor_friction = ifelse(adj_motor_speed > 0, 3.6 / adj_motor_speed, Inf)
  )
ntl_rast <- rast("path_to/viirs_ntl.tif")
road_centroids <- st_centroid(road_data)
road_data$ntl_value <- terra::extract(ntl_rast, vect(road_centroids))[, 2]

road_data <- road_data %>%
  mutate(
    ntl_class = case_when(
      ntl_value > 20 ~ "high",
      ntl_value > 10 ~ "medium",
      ntl_value > 1 ~ "low",
      TRUE ~ "very_low"
    ),
    ntl_walk_mod = case_when(
      ntl_class == "high" ~ 1.0,
      ntl_class == "medium" ~ 0.9,
      ntl_class == "low" ~ 0.7,
      ntl_class == "very_low" ~ 0.5
    ),
    ntl_motor_mod = case_when(
      ntl_class == "high" ~ 1.0,
      ntl_class == "medium" ~ 0.95,
      ntl_class == "low" ~ 0.9,
      ntl_class == "very_low" ~ 0.85
    )
  )




# 2. Transform to projected CRS (example: UTM Zone 36S EPSG:32736, adjust for your area)
road_tza_proj <- st_transform(road_tza, 32736)

# check the population data for Tanzania
pop_tza <- rast("Data Sources/tza_ppp_2020_UNadj_constrained.tif") %>% raster()
res(pop_tza) # It is 100mx100m resolution
dim(pop_tza) # contains one layer (2020 population estimates)
plot(pop_tza, main = "Tanzania Population in 2020")

# check the population data for Zambia
pop_zmb <- rast("Data Sources/zmb_ppp_2020_UNadj_constrained.tif")
res(pop_zmb) # It is 100mx100m resolution
dim(pop_zmb) # contains one layer (2020 population estimates)
plot(pop_zmb, main = "Zambia Population in 2020")

# Check Zambia health facilities data
zmb_fac <- read_excel("Data Sources/zmb_facilities.xlsx") %>% # There is 19 different types of facilities.
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
  filter(inside_zmb) # 70 facilities are out of the country.
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

# Check facilities for Tanzania
tza_fac <- read_excel("Data Sources/List of Operating Health Facilities AS OF 2025_Apr_25 (1).xlsx" )%>% # There is 19 different types of facilities.
  dplyr::select(Fac_IDNumber, Name, Region, Council, Ward, Village, FacilityTypeGroup,
         FacilityType, OwnershipGroup, OperatingStatus, Longitude, Latitude) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude)) %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Get shapefile for Tanzania
tza_shp <- st_read("Data Sources/Shapefiles/District_TZ.shp") %>% 
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
