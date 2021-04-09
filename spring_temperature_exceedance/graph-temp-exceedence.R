library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)

#remotes::install_github("flowwest/waterYearType")
library(waterYearType)

# load data
daily_mean_temp <- read_csv("daily_mean_temperatures.csv")

# get water year data
water_years <- waterYearType::water_year_indices %>% 
  filter(location == "Sacramento Valley", WY >= 1995) %>%
  select(water_year = WY, water_year_type = Yr_type) %>% 
  glimpse()

# Visualize data
# Prep data for data visualization
temps_to_graph <- daily_mean_temp %>%
  left_join(water_years) %>%
  filter(month %in% c(3, 4)) %>% 
  mutate(threshold = ifelse(month == 3, 15, 17), 
         exceeded_day = if_else(mean_daily_temp >= threshold, TRUE, FALSE)) %>%
  group_by(water_year, water_year_type, region, month) %>%
  mutate(n = n(), 
         exceeded_days_month = sum(exceeded_day)) %>%
  ungroup() %>%
  filter(n >= 10, exceeded_days_month > 0) %>%
  glimpse()

# Create function to generate graphs 
graph_temp_exceedence <- function(desired_month, desired_water_year) {
  
  water_year_type <- water_years[water_years$water_year == desired_water_year,][[2]]
  threshold <- ifelse(desired_month == 3, 15, 17)
  month_name <- ifelse(desired_month == 3, "March", "April")
  
  temps_to_graph %>%
    filter(water_year == desired_water_year, month == desired_month) %>% 
    ggplot(aes(x = day, y = mean_daily_temp, col = exceeded_day)) + 
    geom_point() +
    scale_color_manual(values=c("#003f5c", "#ff0a0a")) + 
    geom_hline(yintercept = threshold, linetype = "dashed", color = "gray") + 
    theme_minimal() +
    labs(x = "Day", 
         y = "Mean Daily Temperature", 
         title = paste0('Temperature exceeding ', threshold, " °C"),
         subtitle = paste0(month_name, ", ", desired_water_year, " (Water Year Type: ", water_year_type, ")"))
  ggsave(paste0('figures/temp-exceedence', month_name, desired_water_year,'.jpg'), device = 'jpeg', width = 16, height = 10, units = 'in')
}

# Find unique month year values to run through graph function 
month_year_combos <- temps_to_graph %>% 
  group_by(month, water_year) %>% 
  summarise(n()) %>%
  select(desired_month = month, desired_water_year = water_year)


purrr::pmap(month_year_combos, graph_temp_exceedence)
