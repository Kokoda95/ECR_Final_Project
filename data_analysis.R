# Load libraries
library(sf)
library(tidyverse)
library(terra)
library(readxl)
library(raster)
library(exactextractr)
library(malariaAtlas)

############### Tanzania ##########################################################

# Load the shapefile
tza_shp <- st_read("Raw_data/Shapefiles/District_TZ.shp")

# Read & clean primary HFs data 
tza_fac <- read_excel("Raw_data/Cleaned_Facility list_Tanzania.xlsx") %>% 
  mutate(Council = str_replace_all(Council, c("Dodoma CC" = "Dodoma MC",
                                              "Kahama MC" = "Kahama TC",
                                              "Kigoma Ujiji MC" = "Kigoma MC")),
         Longitude = as.numeric(Longitude),
         Latitude  = as.numeric(Latitude)) %>% 
  filter(!(Longitude == "null" | Latitude == "null" | is.na(Longitude) | is.na(Latitude))) # 323 facilities missed locations
  
# Change to sf object
tza_fac_sf <- tza_fac %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs(tza_shp)) %>% 
  mutate(inside_tza = st_within(geometry, tza_shp, sparse = FALSE) %>% rowSums() > 0) %>% 
  filter(inside_tza) # 164 facilities are out of the country.


# Read the population 
pop_tza <- rast("Raw_data/tza_pop_2025_CN_1km_R2024B_UA_v1.tif")

# Convert to data frame for ggplot (raster -> data frame)
pop_tza_df <- as.data.frame(pop_tza, xy = TRUE)
names(pop_tza_df)[3] <- "pop"

# Plot the distribution and health facilities
ggplot() +
  geom_tile(data = pop_tza_df, aes(x = x, y = y, fill = log10(pop))) +
  scale_fill_viridis(
    option = "inferno",
    name = "Population",
    breaks = log10(c(1, 10, 100, 1000, 10000)),   # values on log10 scale
    labels = c("1", "10", "100", "1K", "10K")     # displayed in original scale
  ) +
  geom_sf(data = tza_fac_sf, aes(color = OwnershipGroup), size = 1, alpha = 0.8) +
  #coord_sf(crs = st_crs(pop_raster)) +
  theme_void(base_size = 14) +
  labs(
    title = "Population and Primary Health Facilities",
    fill = "Population",
    color = "Ownership",
    caption = "Not all the facilities are geolocated"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(size = 10, face = "italic")
  )


# Add population of each council
tza_shp$pop <- exact_extract(pop_tza, tza_shp, "sum")

# Make the plot
tza_plot <- tza_fac %>%
  group_by(Region, Council) %>%
  summarise(numb_fac = n(), .groups = "drop") %>%
  left_join(tza_shp, by = "Council") %>%
  mutate(fac_per_capita = (numb_fac/pop)*10000) %>% 
  ggplot() +
  geom_sf(aes(fill = fac_per_capita, geometry = geometry), color = "grey70", size = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",         # or "magma", "viridis", "inferno"
    direction = -1,            # flip color scale (optional)
    #na.value = "#a6cee3",      # soft blue like water
    name = "Number of facilities\nper 10,000 people" # legend title
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  labs(
    title = "Primary Health Facilities by Council (Tanzania)",
    caption = "Operating facilities only"
  )



######## Zambia ####################################################
# Download the recent shapefile
zmb_shp <- getShp(ISO = "ZMB", admin_level = "admin2")

# Read and clean primary health facilities
zmb_fac <- read_excel("Raw_data/Zambia_Clean_data.xlsx") %>% 
  filter(`Operational status` %in% c("Functional", "Temporarily closure")) %>%
  mutate(District = recode(District,
                           "Chienge" = "Chiengi",
                           "Lavushi Manda" = "Lavushimanda",
                           "Shiwang'andu" = "Shiwang'Andu",
                           "Milenge" = "Milengi",
                           "Shang'ombo" = "Shangombo",
                           "Kapiri-Mposhi" = "Kapiri Mposhi",
                           "Chikankata" = "Chikankanta",
                           "Mushindano" = "Mushindamo"),
  
         Longitude = as.numeric(Longitude),
         Latitude  = as.numeric(Latitude)) %>% 
  filter(!(Longitude == "null" | Latitude == "null" | is.na(Longitude) | is.na(Latitude))) # 86 facilities missed locations

# Change to sf object
zmb_fac_sf <- zmb_fac %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% 
  mutate(inside_zmb = st_within(geometry, zmb_shp, sparse = FALSE) %>% rowSums() > 0) %>% 
  filter(inside_zmb) # 65 facilities are out of the country.

# Read the population 
pop_zmb <- rast("Raw_data/zmb_pop_2025_CN_1km_R2024B_UA_v1.tif")

# Convert to data frame for ggplot (raster -> data frame)
pop_zmb_df <- as.data.frame(pop_zmb, xy = TRUE)
names(pop_zmb_df)[3] <- "pop"

# Plot the distribution and health facilities
ggplot() +
  geom_tile(data = pop_zmb_df, aes(x = x, y = y, fill = pop)) +
  scale_fill_viridis_c(option = "inferno",  
                       #direction = -1,
                       trans = "log10") +
  geom_sf(data = zmb_fac_sf, aes(color = `Ownership type`), size = 1, alpha = 0.8) +
  theme_void(base_size = 14) +
  labs(
    title = "Population and Primary Health Facilities",
    fill = "Population",
    color = "Ownership",
    caption = "Not all the facilities are geolocated"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(size = 10, face = "italic")
  )


# Add population of each council
zmb_shp$pop <- exact_extract(pop_zmb, zmb_shp, "sum")

# Map the distribution of primary HFs in Zambia 
zmb_plot <- zmb_fac %>%
  group_by(Province, District) %>%
  summarise(numb_fac = n(), .groups = "drop") %>%
  left_join(zmb_shp, by = c("District" = "name_2")) %>%
  mutate(fac_per_capita = (numb_fac/pop)*10000) %>% 
  ggplot() +
  geom_sf(aes(fill = fac_per_capita, geometry = geometry), color = "grey70", size = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",         # or "magma", "viridis", "inferno"
    direction = -1,            # flip color scale (optional)
    #na.value = "#a6cee3",      # soft blue like water
    name = "Number of facilities\nper 10,000 people" # legend title

  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic")
  ) +
  labs(
    title = "Primary Health Facilities by District (Zambia)",
    caption = "Operating facilities only"
  )

#######################################################################################################
# Save the plots
ggsave(plot = tza_plot, "Outputs/tza_HFs.png")
ggsave(plot = zmb_plot, "Outputs/zmb_HFs.png")

          