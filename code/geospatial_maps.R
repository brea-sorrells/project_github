# Geospatial Data maps/graphics
# Last Edited: 27 July 2021
# Brea Sorrells & Anjel Iriaghomo

## Libraries --------------
library(tidyverse)
library(sf)
library(ggspatial)

## Reading in Data ----------
dat <- read_csv("outputs/wettable_acres_output.csv")
dat <- dat %>%
  filter(regulated_operation == "Swine") %>%
  filter(regulated_activity != c("Swine - Gilts", "Swine - Other"))

# County Shapefiles
counties <- read_sf("data/shapefiles/shapefiles/nc_counties.shp")
counties <- st_transform(counties, crs = 4269)
# HUC6 Shapefiles
huc6 <- read_sf("data/shapefiles/shapefiles/huc6_studies.shp")
huc6 <- st_transform(huc6, crs = 4269)
# HUC12 Shapefiles
huc12 <- read_sf("data/shapefiles/shapefiles/huc12_studies.shp")
huc12 <- st_transform(huc12, crs = 4269)

# Empty Test map
ggplot(counties, aes()) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl")


## County edits ---------
datc <- dat %>%
  filter(boundary == "county")

# creating spatial data
dat_spatial <- st_as_sf(datc, coords = c("lon", "lat"), crs = 4269)

dat_sums <- datc %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

# adding summaries to the spatial data
counties_sums <- left_join(counties, dat_sums, by = c("CO_NAME" = "name"))

# Creating ratios for mapping
counties_sums <- counties_sums %>%
  mutate("wettable_acres_ratio" = sum/ACRES)


## Huc 6 edits ---------
dat6 <- dat %>%
  filter(boundary == "HUC6")

# Creating Spatial Data
dat6_spatial <- st_as_sf(dat6, coords = c("lon", "lat"), crs = 4269)

dat6_sums <- dat6 %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

# Combining summaries with spatial data
huc6_sums <- left_join(huc6, dat6_sums, by = c("Name" = "name"))

# Creating ratios for mapping
huc6_sums <- huc6_sums %>%
  mutate("wettable_acres_ratio" = sum/AreaAcres)

## HUC12 edits -----------------
dat12 <- dat %>%
  filter(boundary == "HUC12")

# Creating spatial data
dat12_spatial <- st_as_sf(dat12, coords = c("lon", "lat"), crs = 4269)

dat12_sums <- dat12 %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

# Joining sums to spatial data
huc12_sums <- left_join(huc12, dat12_sums, by = c("Name" = "name"))

# Creating ratios for mapping
huc12_sums <- huc12_sums %>%
  mutate("wettable_acres_ratio" = sum/AreaAcres)

huc12_sums$sum


## Plots ------------------
# Eliminating NAs
dat_spatial <- dat_spatial %>%
  filter(regulated_operation == "Swine",
         regulated_activity != "Swine - Gilts",
         regulated_activity != "Swine - Other")

# Map of CAFOs colored by phase
ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = dat_spatial, aes(color = regulated_activity),
          alpha = 0.5, size = 2) +
  theme_bw() +
  annotation_scale(location = "bl") +
  labs(color = "") +
  scale_color_viridis_d() 

# Map of CAFOs faceted by phase, used on poster
ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = dat_spatial,
          alpha = 0.5, color = "blue4", size = 1.3) +
  theme_bw(base_size = 26) +
  theme(strip.background = element_rect(fill = "mediumaquamarine")) +
  annotation_scale(location = "bl") +
  labs(color = "") +
  facet_wrap(~ regulated_activity)

# Sum of total wettable acres by county
ggplot(counties_sums, aes(fill = sum)) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl")

# Ratio by county, used on poster
ggplot(counties_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c() +
  labs(title = "Ratio of Wettable Acres to County Area",
       fill = "")
# Finding maximum county ratio
which.max(counties_sums$wettable_acres_ratio)
counties_sums[85,]

# Same as above with larger lettering
ggplot(counties_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw(base_size = 22) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c() +
  labs(title = "Ratio of Wettable Acres to County Area",
       fill = "")



# HUC6 ratio map, used on poster
ggplot(huc6_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw(base_size = 22) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c() +
  labs(title = "Ratio of Wettable Acres to HUC 6 Watershed Area",
       fill = "")
# Finding max HUC6 ratio
which.max(huc6_sums$wettable_acres_ratio)
huc6_sums[4, 20]


# HUC12 ratio map, used on poster
ggplot(huc12_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw(base_size = 22) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(title = "Ratio of Wettable Acres to HUC 12 Watershed Area",
       fill = "")

# Finding max HUC12 ratio
which.max(huc12_sums$wettable_acres_ratio)
huc12_sums[167, 25]
