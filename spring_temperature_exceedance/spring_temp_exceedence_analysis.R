library(readr)
library(tidyverse)
library(dplyr)
library(naniar)

# Load in secchi temp data and water year rda from waterYearType repo 
secchi_temp <- read_csv("https://raw.githubusercontent.com/CSAMP/delta-secchi-temperature-data/temp-documentation/delta_water_quality_data_with_strata.csv") 
filtered_temp <- secchi_temp %>% 
  mutate(water_year = ifelse(Month >= 8, Year + 1, Year), 
         Date = lubridate::date(Date)) %>% 
  filter(Region == c("Yolo Bypass", "Sacramento River")) %>% glimpse() 

load("~/Git/delta-secchi-temperature-data/frequency_exceeding_temps/water_year_indices.rda") 

water_years <- water_year_indices %>% filter(location == "Sacramento Valley") %>%
  select(water_year = WY, water_year_type = Yr_type) %>% na.omit() %>% glimpse()

# Join them together
# Select strata and find proportion exceeding in march and april 
filtered_temp_with_daily_averages <- filtered_temp %>% 
  group_by(Region, Date) %>%
  mutate(mean_daily_temp = mean(Temperature, na.rm = TRUE), count = n()) %>%
  ungroup() 


temp_with_wy <- filtered_temp_with_daily_averages %>% 
  left_join(water_years) %>%
  filter(Month %in% c(3, 4)) %>%
  group_by(water_year, water_year_type, Region, Month) %>%
  mutate(threshold = ifelse(Month == 3, 15, 17)) %>%
  summarise(prop_above_threshold = mean(Temperature >= threshold),
            n = n(), 
            min_temp = min(Temperature), 
            max_temp = max(Temperature), 
            avg_temp = mean(Temperature),
            first_day_exceeded =  min(if_else(Temperature  > threshold, Date, ymd('3000-01-01')), na.rm = T)) %>% 
  mutate(first_day_exceeded = ifelse(first_day_exceeded == ymd('3000-01-01'), NA, day(first_day_exceeded))) %>%
  gather(stat, value, prop_above_threshold:first_day_exceeded) %>%
  glimpse()
  

View(temp_with_wy)
write_csv(temp_with_wy, "cache_slough_temp_exceeding_thresholds_long.csv")
# write_csv(temp_with_wy, "cache_slough_temp_exceeding_thresholds_wide.csv")
# write_csv(temp_with_wy, "frequency_exceeding_temps/cache_slough_strata_temp_exceeding_thresholds.csv")

# Checks a few months to see if proportion exceeding match up 
# explore_temp <- filtered_temp %>% filter(Year == 2008, Month == 4) %>% mutate(above_17 = sum(Temperature > 17)/n())
