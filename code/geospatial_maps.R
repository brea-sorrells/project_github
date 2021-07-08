# Geospatial Data

library(tidyverse)
library(sf)
library(ggspatial)

dat <- read_csv("outputs/wettable_acres_output.csv")

counties <- read_sf("data/shapefiles/nc_counties.shp")

counties

counties <- st_transform(counties, crs = 4269)

ggplot(counties, aes()) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl")


# Plot farm Locations
dat <- dat %>%
  filter(boundary == "county")

dat_spatial <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4269)

ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = dat_spatial) +
  theme_bw() +
  annotation_scale(location = "bl")

dat_sums <- dat %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

counties_sums <- left_join(counties, dat_sums, by = c("CO_NAME" = "name"))

ggplot(counties_sums, aes(fill = sum)) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl")

counties_sums <- counties_sums %>%
  mutate("wettable_acres_ratio" = sum/ACRES)

ggplot(counties_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering())

