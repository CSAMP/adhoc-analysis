library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)

#remotes::install_github("flowwest/waterYearType")
library(waterYearType)

# Load in secchi temp data and add water year, filter to look at correct regions 
secchi_temp <- read_csv("https://raw.githubusercontent.com/CSAMP/delta-secchi-temperature-data/temp-documentation/delta_water_quality_data_with_strata.csv") 

# Load in temp.txt to redo logic with this dataset
raw_temp <- read.table("temp.txt")[-1,] 
# Clean raw_temp to create daily_mean temp in the same format as original daily mean temp

daily_mean_temp <- raw_temp %>%
  select("year" = V1, "julian_day" = V2, "Sacramento River" = V3, "Yolo Bypass" = V14) %>%
  mutate(date = as.Date(as.numeric(julian_day), origin = paste0(as.character(as.numeric(year)-1), "-", "12", "-", "31")),
         water_year = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>%
  filter(water_year >= 1995) %>%
  gather(region, mean_daily_temp, `Sacramento River`:`Yolo Bypass`) %>%
  mutate(month = month(date), day = day(date), mean_daily_temp = as.numeric(mean_daily_temp)) %>%
  glimpse()


# daily_mean_temp <- secchi_temp %>%
#   mutate(date = as_date(Date),
#          water_year = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>% 
#   filter(Region == c("Yolo Bypass", "Sacramento River"), water_year >= 1995) %>% 
#   arrange(water_year) %>% 
#   group_by(water_year, region = Region, date) %>%
#   summarise(mean_daily_temp = mean(Temperature, na.rm = TRUE)) %>%
#   ungroup() %>% 
#   mutate(month = month(date), day = day(date)) %>% 
#   glimpse()

water_years <- waterYearType::water_year_indices %>% 
  filter(location == "Sacramento Valley", WY >= 1995) %>%
  select(water_year = WY, water_year_type = Yr_type) %>% 
  glimpse()

# Join daily average temp and water_year data together 
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
write_csv(temp_with_wy, "cache_slough_exceedance_from_txt_temp_file_long.csv")
# write_csv(temp_with_wy, "cache_slough_temp_exceeding_thresholds_wide.csv")
