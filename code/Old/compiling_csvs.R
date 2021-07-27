
# Just compiling csv files 24 June

library(tidyverse)
library(janitor)

one <- read_csv("data/county_input_data.csv") 
two <- read_csv("data/castnet_input.csv")

head(one)
head(two)


corn <- read_csv("data/corn.csv") 
wheat <- read_csv("data/wheat.csv")

View(corn)
View(wheat)

dat <- read_csv("input_data_files - county_input_data (1).csv")
View(dat)

wheat <- wheat[, -c(1, 3:9, 11:16, 18:19, 21)]
wheat <- wheat %>% clean_names()
wheat_w <- wheat %>% pivot_wider(names_from = "data_item", values_from = "value")
View(wheat_w)



corn <- corn[, -c(1, 3:9, 11:16, 18:19, 21)]
corn <- corn %>% clean_names()
corn_w <- corn %>% pivot_wider(names_from = "data_item", values_from = "value")
View(corn_w)

small_combo <- inner_join(corn_w, wheat_w, by = c("year", "county"))
View(small_combo_flat)
small_combo$county <- tolower(small_combo$county)
small_combo_flat <- filter(small_combo, small_combo$county != "other (combined) counties")
small_combo_flat <- unnest(small_combo_flat, cols = `CORN, SILAGE - YIELD, MEASURED IN TONS / ACRE`)
small_combo_flat <- unnest(small_combo_flat, cols = `WHEAT - YIELD, MEASURED IN BU / ACRE`)


big_combo <- left_join(dat, small_combo_flat, by = c("year", "county"))
View(big_combo)

big_combo <- clean_names(big_combo)



write_csv(big_combo, file = "combo_csv_crop.csv")

