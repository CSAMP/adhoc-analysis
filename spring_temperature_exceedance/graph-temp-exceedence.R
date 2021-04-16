library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)

#remotes::install_github("flowwest/waterYearType")
library(waterYearType)

# load data
# daily_mean_temp <- read_csv("daily_mean_temperatures.csv")

# Load in new temp data and combine regions to get mean daily temp 
daily_mean_temp <- read_csv("daily_mean_temperatures_from_temp_txt.csv") %>%
  group_by(date, water_year, month, day) %>%
  summarise(mean_daily_temp = mean(mean_daily_temp))
# get water year data
water_years <- waterYearType::water_year_indices %>% 
  filter(location == "Sacramento Valley", WY >= 1995) %>%
  select(water_year = WY, water_year_type = Yr_type) %>% 
  glimpse()

# Visualize data
## Filter by years where exceedance occurs in one of the months ----------------
# years_to_graph <- c(1997, 2001, 2002, 2003, 2005, 2007, 2008, 2009, 2010, 2013, 
                    # 2014, 2015, 2019)
years_to_graph <- c(1995:2010)
# Prep data for data visualization
temps_to_graph <- daily_mean_temp %>%
  left_join(water_years) %>%
  filter(month %in% c(3, 4)) %>% 
  mutate(threshold = ifelse(month == 3, 15, 17), 
         exceeded_day = if_else(mean_daily_temp >= threshold, TRUE, FALSE)) %>%
  group_by(water_year, water_year_type, month) %>%
  mutate(n = n(),
         exceeded_days_month = sum(exceeded_day)) %>%
  ungroup() %>%
  filter(n >= 10, water_year %in% years_to_graph, mean_daily_temp > 0) %>%
  glimpse()

# Create function to generate graphs 
graph_temp_exceedence <- function(desired_water_year) {
  
  water_year_type <- water_years[water_years$water_year == desired_water_year,][[2]]
  # threshold <- ifelse(month == 3, 15, 17)
  # month_name <- ifelse(month == 3, "March", "April")
  day <- lubridate::ymd(paste0(desired_water_year, "03", "04", sep = "-"))
  
  temps_to_graph %>%
    filter(water_year == desired_water_year) %>% 
    ggplot(aes(x = date, y = mean_daily_temp)) + 
    geom_line(color = "gray") +
    geom_point(size = 3, aes(col = exceeded_day)) +
    scale_color_manual(values=c("#003f5c", "#ff0a0a")) + 
    geom_hline(yintercept = 15, linetype = "dashed", color = "gray", size = 2) + 
    geom_hline(yintercept = 17, linetype = "dashed", color = "pink", size = 2) +
    annotate(geom = "text",
             label = c("March Threshold", "April Threshold"),
             x = c(day, day),
             y = c(15, 17),
             vjust = 1.5) +
    theme_minimal() +
    labs(x = "Day", 
         y = "Mean Daily Temperature (°C)", 
         title = paste0('Temperature exceeding 15 °C in March and 17 °C in April'),
         subtitle = paste0( desired_water_year, " (Water Year Type: ", water_year_type, ")"),
         colour = "Exceeds Threshold") +
    theme(text = element_text(size = 23),
          # legend.position = c(.90, .05),
          legend.position = "right",
          legend.box = "horizontal",
          legend.title = element_text(size = 17), 
          legend.text = element_text(size = 17),
          axis.title.x = element_text(vjust = -0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) 
  ggsave(paste0('figures/combined-months-temp-txt-dataset/cache-slough-temp-exceedence', desired_water_year,'.jpg'), device = 'jpeg', width = 16, height = 10, units = 'in')
}
years_to_graph <- list(1997, 2001, 2002, 2003, 2005, 2007, 2008, 2009, 2010, 2013, 
                    2014, 2015, 2019)
purrr::map(years_to_graph, graph_temp_exceedence)

