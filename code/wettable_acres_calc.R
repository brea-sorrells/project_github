# Calculations of wettable acres
# 30 June 2021

#Libraries
library(tidyverse)
library(janitor)
library(lubridate)

#Read in data files --------
county <- read_csv("data/county_input_data.csv")
crop <- read_csv("data/crop_n_input_data.csv")
phase <- read_csv("data/phase_input_data.csv")
cafos <- read_csv("data/swine_cafos_info.csv")

View(county)
View(crop)
View(phase)
View(cafos)

# Manure Calcs ---------

wettable_acres <- cafos[, -c(3, 7:8, 11, 14, 15:16, 20:30)]

View(wettable_acres)
wettable_acres$county <- tolower(wettable_acres$county)
wettable_acres <- clean_names(wettable_acres)


wettable_acres <- left_join(wettable_acres, phase, by = c("regulated_activity" = "phase"))

wettable_acres$manure_produced_gal_yr <- (wettable_acres$manure_produced_gal_yr)*wettable_acres$allowable_count

wettable_acres <- wettable_acres %>%
  mutate("n_produced_lbs_yr" = ((manure_produced_gal_yr/1000)*manure_n_g_avg))


# Find county Avgs ------

county_avgs <- county %>%
  group_by(county) %>%
  summarise("avg_corn_y" = mean(corn_silage_yield_measured_in_tons_acre, na.rm = "TRUE"),
            "avg_wheat_y" = mean(wheat_yield_measured_in_bu_acre, na.rm = "TRUE"),
            "avg_soybean_y" = mean(soybean_yield_bu_acre, na.rm = "TRUE"),
            "avg_bermudagrass_y" = mean(bermudagrass_yield_tons_acre, na.rm = "TRUE"),
            "avg_tall fescue_y" = mean(fescue_yield_tons_acre, na.rm = "TRUE"),
            "avg_rye_y" = mean(rye_yield_bu_acre, na.rm = "TRUE")) %>%
  ungroup()

  #Filling NaNs with overall averages
avg_soy <- mean(county$soybean_yield_bu_acre, na.rm = TRUE)
avg_corn <- mean(county$corn_silage_yield_measured_in_tons_acre, na.rm = TRUE)
avg_wheat <- mean(county$wheat_yield_measured_in_bu_acre, na.rm = TRUE)

iter <- 1:nrow(county_avgs)
for (i in iter) {
  if (county_avgs$avg_soybean_y[i] == "NaN"){
    county_avgs$avg_soybean_y[i] = avg_soy
  }
  if (county_avgs$avg_corn_y[i] == "NaN"){
    county_avgs$avg_corn_y[i] = avg_corn
  }
  if (county_avgs$avg_wheat_y[i] == "NaN"){
    county_avgs$avg_wheat_y[i] = avg_wheat
  }
}

county_avgs <- pivot_longer(county_avgs, names_to = "crop_type",
                            values_to = "avg_yield", cols = 2:7)

county_avgs <- separate(county_avgs, col = "crop_type", sep = "_", into = c("avg", "crop", "y"))
county_avgs <- county_avgs[, -c(2,4)]

View(county_avgs)

# Combining w/nutrient info ---------
county_crop_n <- left_join(county_avgs, crop, by = c("crop" = "crop_type"))
View(county_crop_n)

county_crop_n <- county_crop_n %>%
  mutate("n_up_lb_acre_min" = avg_yield*min_uptake,
         "n_up_lb_acre_max" = avg_yield*max_uptake,
         "spray_date" = mdy(spray_begin))

county_crop_n

?format
         