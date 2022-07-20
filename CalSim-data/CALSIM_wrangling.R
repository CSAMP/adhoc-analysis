library(tidyverse)
library(lubridate)


# Define node to location lookup based on calsim II schema: https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg
location_lookup <- c("Delta Outflow", "Freeport Flows", "OMR Flows", "SWP Export", "CVP Export", 
                     "Trinity Reservoir", "Shasta Reservoir", "Orville Reservoir", "Folsom Reservoir")

names(location_lookup) <- c("C407", "C400", "C408", "DEL_SWP_TOTAL", "DEL_CVP_TOTAL", 
                            "S1", "S4", "S6", "S8")


# LOAD IN CALSIM RESULTS FROM THE 2008 and 2009 BiOp --------------------------------------------------
col_names <- readxl::read_excel("CalSim-data/data-raw/2008_2009_Biop.xlsx", skip = 1) %>% 
  rename(date = "...2") %>%
  names()

BiOp_2008_2009 <- readxl::read_excel("CalSim-data/data-raw/2008_2009_Biop.xlsx", skip = 7, col_names = col_names) %>% 
  select(-B) %>% 
  mutate(date = as_date(date)) %>%
  filter(year(date) >= 1922, year(date) <= 2002) %>%
  gather(calsim_node, value, -date) %>%
  mutate(location = location_lookup[calsim_node], 
         units = ifelse(calsim_node %in% c("S1", "S4", "S6", "S8"), "taf", "cfs"),
         calsim_run = "2008 & 2009 BiOp") %>%
  glimpse

# save to csv
write_csv(BiOp_2008_2009, "CalSim-data/data/calsim_2008_2009_biop.csv")


# LOAD IN CALSIM RESULTS FROM THE 2019 Biop and 2020 Itp ----------------------------------------------
col_names_2 <- readxl::read_excel("CalSim-data/data-raw/2019_Biop_2020_ITP.xlsx", skip = 1) %>% 
  rename(date = "...2") %>%
  names()

BiOp_2019_Itp_2020 <- readxl::read_excel("CalSim-data/data-raw/2019_Biop_2020_ITP.xlsx", skip = 7, col_names = col_names_2) %>% 
  select(-B) %>% 
  mutate(date = as_date(date)) %>%
  filter(year(date) >= 1922, year(date) <= 2002) %>%
  gather(calsim_node, value, -date) %>%
  mutate(location = location_lookup[calsim_node], 
         units = ifelse(calsim_node %in% c("S1", "S4", "S6", "S8"), "taf", "cfs"),
         calsim_run = "2019 BiOp & 2020 ITP") %>%
  glimpse

View(BiOp_2008_2009)

# save to csv 
write_csv(BiOp_2019_Itp_2020, "CalSim-data/data/calsim_2019_biop_2020_itp.csv")


# View Modeled Results --------------------------------------------------------------------------------

# combine to make some basic visualzations

all_dat <- bind_rows(BiOp_2008_2009, BiOp_2019_Itp_2020)

# Resivor values
all_dat %>% 
  filter(!(calsim_node %in% c("S1", "S4", "S6", "S8")), 
         year(date) > 1980) %>% # filter time down for easier viewing 
  ggplot() + 
  geom_line(aes(x = date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") + 
  theme_minimal()

# Other nodes
all_dat %>% 
  filter(calsim_node %!in% c("S1", "S4", "S6", "S8"), 
         year(date) > 1980) %>% # filter time down for easier viewing 
  ggplot() + 
  geom_line(aes(x = date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") +
  theme_minimal()
