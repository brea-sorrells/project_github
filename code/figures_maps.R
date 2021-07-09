# Figures and other outputs

#Libraries
library(tidyverse)
library(janitor)

# Next Steps
# add av, max, min of overall 3 scenarios, use for the next steps
# Make Graphs
#REMEMBER EACH FARM ATM HAS 3 ENTRIES
# Combine w/ the total Cafos doc somehow, then export, make graphs in a new folder.
# total wettable acres per county& watershed

#data file
dat <- read_csv("wettable_acres_output.csv")
dat <- dat %>%
  filter(regulated_operation == "Swine",
         regulated_activity != "Swine - Gilts",
         regulated_activity != "Swine - Other")

dat_county <- dat %>%
  filter(boundary == "county")

dat_huc6 <- dat %>%
  filter(boundary == "HUC6")

dat_huc12 <- dat %>%
  filter(boundary == "HUC12")

# individual farm scatterplots
dat_county %>%
  ggplot(aes(x = allowable_count)) +
  geom_point(aes(y = avg_sprayable_acres), alpha = 0.5, color = "slateblue3") +
  geom_errorbar(aes(y = avg_sprayable_acres,
                    ymin = min_sprayable_acres,
                    ymax = max_sprayable_acres), alpha = 0.5, color = "slateblue3") +
  facet_wrap(~regulated_activity,
             scales = "free") +
  labs(x = "Allowable Count (# Swine)",
       y = "Wettable Acres (with error bars)")

dat_huc6 %>%
  ggplot(aes(x = allowable_count, color = regulated_activity)) +
  geom_point(aes(y = avg_sprayable_acres), alpha = 0.5) +
  labs(x = "Allowable Count (# Swine)",
       y = "Average Wettable Acres",
       color = "") +
  scale_color_brewer(palette = "Paired")
?labs
 # geom_errorbar(aes(y = avg_sprayable_acres,
                   # ymin = min_sprayable_acres,
                  # ymax = max_sprayable_acres))

#boxplots
dat_huc6 %>%
  ggplot(aes(x = regulated_activity, y = avg_sprayable_acres)) +
  geom_boxplot()


#bar Graphs
sum_by_huc6 <- dat_huc6 %>%
  group_by(name) %>%
  summarise("summin" = sum(min_sprayable_acres),
            "summax" = sum(max_sprayable_acres))

dat6_w_ranges <-
  inner_join(dat_huc6, sum_by_huc6, by = "name")

  dat6_w_ranges %>% ggplot(aes(x = name, y = avg_sprayable_acres)) +
  geom_col(aes(fill = regulated_activity)) +
  geom_errorbar(aes(ymin = summin,
                     ymax = summax))

