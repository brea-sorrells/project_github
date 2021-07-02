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
cat <- read_csv("data/input_data_files - category_input_data.csv")

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
            "avg_wsc_y" = mean((soybean_yield_bu_acre + wheat_yield_measured_in_bu_acre 
                                + corn_silage_yield_measured_in_tons_acre)/3, na.rm = "TRUE"),
            "avg_bermudagrass_y" = mean(bermudagrass_yield_tons_acre, na.rm = "TRUE"),
            "avg_tall fescue_y" = mean(fescue_yield_tons_acre, na.rm = "TRUE"),
            "avg_rye_y" = mean(rye_yield_bu_acre, na.rm = "TRUE")) %>%
  ungroup()

?mean
  #Filling NaNs with overall averages
avg_soy <- mean(county$soybean_yield_bu_acre, na.rm = TRUE)
avg_corn <- mean(county$corn_silage_yield_measured_in_tons_acre, na.rm = TRUE)
avg_wheat <- mean(county$wheat_yield_measured_in_bu_acre, na.rm = TRUE)
avg_wsc <- mean((county$wheat_yield_measured_in_bu_acre + county$corn_silage_yield_measured_in_tons_acre
                 + county$soybean_yield_bu_acre)/3, na.rm = TRUE)

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
  if (county_avgs$avg_wsc_y[i] == "NaN"){
    county_avgs$avg_wsc_y[i] = avg_wsc
  }
}


county_avgs_l <- pivot_longer(county_avgs, names_to = "crop_type",
                            values_to = "avg_yield", cols = 5:7)

county_avgs_l <- separate(county_avgs_l, col = "crop_type", sep = "_", into = c("avg", "crop", "y"))
county_avgs_l <- county_avgs_l[, -c(2:6, 8)]

itera <- 1:nrow(county_avgs_l)
for (i in itera) {
  if (county_avgs_l$crop[i] == "wsc"){
    county_avgs_l$crop[i] = "corn_soybean_wheat"
  }
  if (county_avgs_l$crop[i] == "bermudagrass"){
    county_avgs_l$crop[i] = "bermudagrass_rye"
  }
  if (county_avgs_l$crop[i] == "tall fescue"){
    county_avgs_l$crop[i] = "tall_fescue"
  }
}


View(county_avgs_l)


# Combining w/nutrient info ---------
county_crop_n <- left_join(county_avgs_l, cat, by = c("crop" = "grouping"))
View(county_crop_n)

county_crop_n <- county_crop_n %>%
  mutate("n_up_lb_acre_min" = avg_yield*min_uptake,
         "n_up_lb_acre_max" = avg_yield*max_uptake)

county_crop_n

         