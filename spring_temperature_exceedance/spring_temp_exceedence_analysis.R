library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)

#remotes::install_github("flowwest/waterYearType")
library(waterYearType)

# Load in secchi temp data and add water year, filter to look at correct regions 
secchi_temp <- read_csv("https://raw.githubusercontent.com/CSAMP/delta-secchi-temperature-data/temp-documentation/delta_water_quality_data_with_strata.csv") 

daily_mean_temp <- secchi_temp %>%
  mutate(date = as_date(Date),
         water_year = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>% 
  filter(Region == c("Yolo Bypass", "Sacramento River"), water_year >= 1995) %>% 
  arrange(water_year) %>% 
  group_by(water_year, region = Region, date) %>%
  summarise(mean_daily_temp = mean(Temperature, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(month = month(date), day = day(date)) %>% 
  glimpse()


water_years <- waterYearType::water_year_indices %>% 
  filter(location == "Sacramento Valley", WY >= 1995) %>%
  select(water_year = WY, water_year_type = Yr_type) %>% 
  glimpse()

# Join daily aveage temp and water_year data together 
# Group by water year, water_year_type, region, and month
temp_with_wy <- daily_mean_temp %>% 
  left_join(water_years) %>%
  filter(month %in% c(3, 4)) %>% 
  mutate(threshold = ifelse(month == 3, 15, 17), 
         exceeded_day = if_else(mean_daily_temp >= threshold, day, as.integer(100))) %>%  
  group_by(water_year, water_year_type, region, month) %>%
  summarise(prop_above_threshold = mean(mean_daily_temp >= threshold, na.rm = TRUE),
            n = n(), 
            min_temp = min(mean_daily_temp, na.rm = TRUE), 
            max_temp = max(mean_daily_temp, na.rm = TRUE), 
            mean_temp = mean(mean_daily_temp, na.rm = TRUE),
            first_day_exceeded = if_else(min(exceeded_day) == 100, as.integer(NA), min(exceeded_day))) %>%  
  gather(stat, value, prop_above_threshold:first_day_exceeded) %>%
  glimpse()
  
# View and write csv 
View(temp_with_wy)
write_csv(temp_with_wy, "cache_slough_temp_exceeding_thresholds_long.csv")
# write_csv(temp_with_wy, "cache_slough_temp_exceeding_thresholds_wide.csv")
