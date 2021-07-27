## Figures and graphs (non-spatial)
## Last Edited 27 July 2021
## Brea Sorrells & Anjel Iriaghomo

## Libraries -------------
library(tidyverse)
library(janitor)

## Reading data file ---------
dat <- read_csv("outputs/wettable_acres_output.csv")
dat <- dat %>%
  filter(regulated_operation == "Swine",
         regulated_activity != "Swine - Gilts",
         regulated_activity != "Swine - Other")

# Creating separate data frames for each boundary type
dat_county <- dat %>%
  filter(boundary == "county")

dat_huc6 <- dat %>%
  filter(boundary == "HUC6")

dat_huc12 <- dat %>%
  filter(boundary == "HUC12")

# Creating sums for each area
dat_county_sums <- dat_county %>%
  group_by(name) %>%
  summarise(sum = sum(avg_sprayable_acres))

dat_6_sums <- dat_huc6 %>%
  group_by(name) %>%
  summarise(sum = sum(avg_sprayable_acres))

dat_12_sums <- dat_huc12 %>%
  group_by(name) %>%
  summarise(sum = sum(avg_sprayable_acres))

## Individual farm scatterplots ---------------

# Wettable acres vs Allowable Count w/error bars, faceted by phase
dat_county %>%
  ggplot(aes(x = allowable_count)) +
  geom_errorbar(aes(y = avg_sprayable_acres,
                    ymin = min_sprayable_acres,
                    ymax = max_sprayable_acres), alpha = 0.5, color = "blue2") +
  geom_point(aes(y = avg_sprayable_acres), alpha = 0.5, color = "blue4",
             size = 2) +
  facet_wrap(~regulated_activity,
             scales = "free") +
  labs(x = "Allowable Count (# Swine)",
       y = "Wettable Acres (with error bars)") +
  theme_bw(base_size = 26) +
  theme(strip.background = element_rect(fill = "mediumaquamarine"))

# Avg Wettable Acres vs allowable count
dat_huc6 %>%
  ggplot(aes(x = allowable_count)) +
  geom_point(aes(y = avg_sprayable_acres, fill = regulated_activity), alpha = 0.5,
             size = 8, shape = 21, color = "black") +
  labs(x = "Allowable Count (# Swine)",
       y = "Average Wettable Acres",
       fill = "") +
  scale_fill_viridis_d(end = 0.9) +
  theme_bw(base_size = 30)


## Boxplots ---------------

# Each phase, avg sprayable acres
dat_huc6 %>%
  ggplot(aes(x = regulated_activity, y = avg_sprayable_acres)) +
  geom_boxplot()


## Bar Graphs -------------
#Need to create this to put error bars on the bar graph correctly
sum_by_huc6 <- dat_huc6 %>%
  group_by(name) %>%
  summarise("summin" = sum(min_sprayable_acres),
            "summax" = sum(max_sprayable_acres))
dat6_w_ranges <-
  inner_join(dat_huc6, sum_by_huc6, by = "name")

# Huc6 location vs the number of acres included
  dat6_w_ranges %>% ggplot(aes(x = name, y = avg_sprayable_acres)) +
  geom_col(aes(fill = regulated_activity)) +
  geom_errorbar(aes(ymin = summin,
                     ymax = summax))

