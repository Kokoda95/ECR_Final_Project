# Load libraries
library(sf)
library(tidyverse)
library(terra)
library(readxl)
library(malariaAtlas)

# Check the elevation data for tanzania
srt_tza <- rast("Data Sources/Tanzania_SRTM30meters/Tanzania_SRTM30meters.tif")
res(srt_tza) # It is 30mx30m resolution
dim(srt_tza) # contains one layer
plot(srt_tza, main = "Elevation")

# Check the roads data for Tanzania
#road_tza <- st_read("Data Sources/hotosm_tza_roads_lines_shp/hotosm_tza_roads_lines_shp.shp")

# Check the roads data for Zambia
road_zmb <- st_read("Data Sources/hotosm_zmb_roads_lines_shp/hotosm_zmb_roads_lines_shp.shx")

# check the population data for Tanzania
pop_tza <- rast("Data Sources/tza_ppp_2020_UNadj_constrained.tif")
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
tza_fac <- read_excel("Data Sources/tza_facilities.xlsx") %>% # There is 19 different types of facilities.
  select(Fac_IDNumber, Name, Region, Council, Ward, Village, FacilityTypeGroup,
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
