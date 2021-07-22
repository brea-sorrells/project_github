# Geospatial Data

library(tidyverse)
library(sf)
library(ggspatial)

dat <- read_csv("outputs/wettable_acres_output.csv")

dat <- dat %>%
  filter(regulated_operation == "Swine") %>%
  filter(regulated_activity != c("Swine - Gilts", "Swine - Other"))

counties <- read_sf("data/shapefiles/shapefiles/nc_counties.shp")

counties

counties <- st_transform(counties, crs = 4269)

huc6 <- read_sf("data/shapefiles/shapefiles/huc6_studies.shp")

huc6 <- st_transform(huc6, crs = 4269)

huc12 <- read_sf("data/shapefiles/shapefiles/huc12_studies.shp")

huc12 <- st_transform(huc12, crs = 4269)

ggplot(counties, aes()) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl")


# county
datc <- dat %>%
  filter(boundary == "county")

dat_spatial <- st_as_sf(datc, coords = c("lon", "lat"), crs = 4269)

dat_sums <- datc %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

counties_sums <- left_join(counties, dat_sums, by = c("CO_NAME" = "name"))

counties_sums <- counties_sums %>%
  mutate("wettable_acres_ratio" = sum/ACRES)


# Huc 6
dat6 <- dat %>%
  filter(boundary == "HUC6")

dat6_spatial <- st_as_sf(dat6, coords = c("lon", "lat"), crs = 4269)

dat6_sums <- dat6 %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

huc6_sums <- left_join(huc6, dat6_sums, by = c("Name" = "name"))

huc6_sums <- huc6_sums %>%
  mutate("wettable_acres_ratio" = sum/AreaAcres)

huc6_sums

#Huc 12
dat12 <- dat %>%
  filter(boundary == "HUC12")

dat12_spatial <- st_as_sf(dat12, coords = c("lon", "lat"), crs = 4269)

dat12_sums <- dat12 %>%
  group_by(name) %>%
  summarise("sum" = sum(avg_sprayable_acres, na.rm = TRUE))

huc12_sums <- left_join(huc12, dat12_sums, by = c("Name" = "name"))
huc12_sums <- huc12_sums %>%
  mutate("wettable_acres_ratio" = sum/AreaAcres)

huc12_sums$sum


# Plots
dat_spatial <- dat_spatial %>%
  filter(regulated_operation == "Swine",
         regulated_activity != "Swine - Gilts",
         regulated_activity != "Swine - Other")

ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = dat_spatial, aes(color = regulated_activity),
          alpha = 0.5, size = 2) +
  theme_bw() +
  annotation_scale(location = "bl") +
  labs(color = "") +
  scale_color_viridis_d() 

ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = dat_spatial,
          alpha = 0.5, color = "blue4", size = 1.3) +
  theme_bw(base_size = 26) +
  theme(strip.background = element_rect(fill = "mediumaquamarine")) +
  annotation_scale(location = "bl") +
  labs(color = "") +
  facet_wrap(~ regulated_activity)

ggplot(counties_sums, aes(fill = sum)) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl")

ggplot(counties_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw() +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c() +
  labs(title = "Ratio of Wettable Acres to County Area",
       fill = "")

which.max(counties_sums$wettable_acres_ratio)
counties_sums[85,]

ggplot(counties_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw(base_size = 22) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c() +
  labs(title = "Ratio of Wettable Acres to County Area",
       fill = "")


ggplot(huc6_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw(base_size = 22) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c() +
  labs(title = "Ratio of Wettable Acres to HUC 6 Watershed Area",
       fill = "")

which.max(huc6_sums$wettable_acres_ratio)
huc6_sums[4, 20]

ggplot(huc12_sums, aes(fill = wettable_acres_ratio)) +
  geom_sf() +
  theme_bw(base_size = 22) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location =  "br", height = unit(1, "cm"), 
                         width = unit(1, "cm"), style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(title = "Ratio of Wettable Acres to HUC 12 Watershed Area",
       fill = "")

which.max(huc12_sums$wettable_acres_ratio)
huc12_sums[167, 25]
