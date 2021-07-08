# Test calculations of N surplus using input files
# 25 June

# libraries --------------
library(tidyverse)
library(janitor)

# Csvs --------------
county <- read_csv("data/county_input_data.csv")
crop <- read_csv("data/crop_n_input_data.csv")
lagoon <- read_csv("data/lagoon_input_data.csv")
phase <- read_csv("data/phase_input_data.csv")
cafos <- read_csv("data/swine_cafos_info.csv")

county
crop
lagoon
phase
cafos

unique(cafos$year_cor)

cafos <- clean_names(cafos)

cafos_county <- cafos %>% filter(type == "county", regulated_operation == "Swine")
view(cafos_county)

#adding surface area -----
surface_area_ft2 <- c(373, NA, 316, 197, NA, NA, NA)
surface_area_ft2 <- surface_area_ft2 * (1000/2500)

phase <- phase %>%
mutate("surface_area_2ft" = surface_area_ft2)

# replacing county NAs with their mean ----
# finding means
hay_mean <- county %>%
  summarise(hay_mean = mean(hay_yield_tons_acre, na.rm = TRUE))
soy_mean <- county %>%
  summarise(soy_mean = mean(soybean_yield_bu_acre, na.rm = TRUE))
at_dep_mean <- county %>%
  summarise(at_dep_mean = mean(at_dep, na.rm = TRUE))


# replacing
county <- county %>% 
  mutate(hay_yield_tons_acre = 
           replace_na(hay_yield_tons_acre, replace = hay_mean))
county <- county %>% 
  mutate(soybean_yield_bu_acre = 
           replace_na(soybean_yield_bu_acre, replace = soy_mean))
county <- county %>% 
  mutate(at_dep = 
           replace_na(at_dep, replace = at_dep_mean))

county <- unnest(county, c(hay_yield_tons_acre, soybean_yield_bu_acre, at_dep))

# checking that all counties have all years -----
county %>% pivot_wider(names_from = county, 
                       values_from = c(hay_yield_tons_acre, soybean_yield_bu_acre, at_dep)) %>%
               View()

view(county)

# making a CONDENSED cafos_info
cafos_small <- cafos_county[, -c(3, 7:12, 14, 20:30)]
View(cafos_small)
unique(cafos_small$regulated_activity)

cafos_small %>%
  group_by(facility_name) %>%
  summarise(wowpigs = sum(allowable_count)) %>%
  View()


