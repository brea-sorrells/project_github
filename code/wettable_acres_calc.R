## Calculations of Wettable Acres
## Created: 30 June 2021
## Updated: 27 July 2021
## Brea Sorrells & Anjel Iriaghomo

## Libraries -------------
library(tidyverse)
library(janitor)
library(lubridate)

## Read in data files --------
county <- read_csv("data/county_input_data.csv")
      #Contains info on crop yields for each NC county with CAFOs from
      # 1984-2020 (when possible), & atmospheric deposition (unused)
crop <- read_csv("data/crop_n_input_data.csv")
      #Contains info on crop Nitrogen uptake, sprayable days, & BNF Rate
phase <- read_csv("data/input_data_files - phase_input_data.csv")
      # Contains gallons of manure produced per year & kg per gallon of
      # nitrogen for each included phase
cafos <- read_csv("data/swine_cafos_info.csv")
      # Permit & Lagoon data, contains location, count, &
      # phase information, among other unused information
cat <- read_csv("data/input_data_files - category_input_data.csv")
      # Total spray days, min & max uptake values for crop combinations

# Cleaning permit data
wettable_acres <- cafos
wettable_acres$county <- tolower(wettable_acres$county)
wettable_acres <- clean_names(wettable_acres)

## Produced N Calculations ------------------
# Calculating produced manure (gallons)
wettable_acres <- left_join(wettable_acres, phase, by = c("regulated_activity" = "phase"))
wettable_acres$manure_produced_gal_yr <- (wettable_acres$manure_produced_gal_yr)*wettable_acres$allowable_count

# Calculating N produced for manure gallons
wettable_acres <- wettable_acres %>%
  mutate("n_produced_lbs_yr" = ((manure_produced_gal_yr/1000)*manure_n_g_avg))
          # *0.6 times manure_n_g_avg
          # Added (very) late in the process after noticing the conversion factor
  

## Filling in crop yield NAs ----------
# Finding group averages
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

# Corn/Wheat/soybean rotation
avg_soy <- mean(county$soybean_yield_bu_acre, na.rm = TRUE)
avg_corn <- mean(county$corn_silage_yield_measured_in_tons_acre, na.rm = TRUE)
avg_wheat <- mean(county$wheat_yield_measured_in_bu_acre, na.rm = TRUE)
avg_wsc <- mean((county$wheat_yield_measured_in_bu_acre + county$corn_silage_yield_measured_in_tons_acre
                 + county$soybean_yield_bu_acre)/3, na.rm = TRUE)

# Filling C/W/S NaNs
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

# Making filling possible for correct crops
county_avgs_l <- pivot_longer(county_avgs, names_to = "crop_type",
                            values_to = "avg_yield", cols = 5:7)
county_avgs_l <- separate(county_avgs_l, col = "crop_type", sep = "_", into = c("avg", "crop", "y"))
county_avgs_l <- county_avgs_l[, -c(2:6, 8)]

# Filling NAs with overall averages
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


## Combining yield w/nutrient info ---------
county_crop_n <- left_join(county_avgs_l, cat, by = c("crop" = "grouping"))

# Calculating uptake
county_crop_n <- county_crop_n %>%
  mutate("n_up_lb_acre_min" = avg_yield*min_uptake,
         "n_up_lb_acre_max" = avg_yield*max_uptake)

# Eliminating all but county, crop, min/max uptake, spray days
county_join_crops <- county_crop_n[, -c(3, 5:6)]
county_join_crops <- county_join_crops %>%
  pivot_wider(names_from = "crop", values_from = c(n_up_lb_acre_min, n_up_lb_acre_max, total_spray_days))

## Calculations -----------------
# permit + uptake information
wettable_acres <- inner_join(wettable_acres, county_join_crops, by = "county")

# Crop uptake per day
wettable_acres <- wettable_acres %>%
  mutate("manure_produced_gal_day" = manure_produced_gal_yr/365) %>%
  mutate("n_produced_lbs_day" = n_produced_lbs_yr/365,
         "n_up_br_max_day" = n_up_lb_acre_max_bermudagrass_rye/(total_spray_days_bermudagrass_rye),
         "n_up_br_min_day" = n_up_lb_acre_min_bermudagrass_rye/(total_spray_days_bermudagrass_rye),
         "n_up_cws_max_day" = n_up_lb_acre_max_corn_soybean_wheat/(total_spray_days_corn_soybean_wheat),
         "n_up_cws_min_day" = n_up_lb_acre_min_corn_soybean_wheat/(total_spray_days_corn_soybean_wheat),
         "n_up_tf_max_day" = n_up_lb_acre_max_tall_fescue/(total_spray_days_tall_fescue),
         "n_up_tf_min_day" = n_up_lb_acre_min_tall_fescue/(total_spray_days_tall_fescue))

# Acres max/min for each crop
wettable_acres <- wettable_acres %>%
  mutate("acres_spray_day" = manure_produced_gal_day/27154,
         "acres_br_min_day" = n_produced_lbs_day/n_up_br_max_day,
         "acres_br_max_day" = n_produced_lbs_day/n_up_br_min_day,
         "acres_cws_min_day" = n_produced_lbs_day/n_up_cws_max_day,
         "acres_cws_max_day" = n_produced_lbs_day/n_up_cws_min_day,
         "acres_tf_min_day" = n_produced_lbs_day/n_up_tf_max_day,
         "acres_tf_max_day" = n_produced_lbs_day/n_up_tf_min_day)

# cleaning into a smaller data frame
how_many_acres <- wettable_acres[, -c(31:51)]

# Overall max, min, avgs
how_many_acres <- how_many_acres %>%
  mutate("min_sprayable_acres" = NA,
         "max_sprayable_acres" = NA,
         "avg_sprayable_acres" = NA)
 for (i in 1:length(how_many_acres$min_sprayable_acres)){
    how_many_acres$min_sprayable_acres[i] <- min(how_many_acres[i, 31:36])
    how_many_acres$max_sprayable_acres[i] <- max(how_many_acres[i, 31:36])
    how_many_acres$avg_sprayable_acres[i] <- rowMeans(how_many_acres[i, 31:36])
 }

# cleaning for export
 how_many_acres <- how_many_acres[, -c(31:36)]
 
# Outputting data 
 write_csv(how_many_acres, "outputs/wettable_acres_output.csv")

         