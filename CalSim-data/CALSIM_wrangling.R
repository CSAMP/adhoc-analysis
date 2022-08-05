library(tidyverse)
library(lubridate)


# Define node to location lookup based on calsim II schema: https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg
location_lookup <- c("Delta Outflow", "Freeport Flows", "OMR Flows", "SWP Export", "CVP Export", 
                     "Trinity Reservoir", "Shasta Reservoir", "Oroville Reservoir", "Folsom Reservoir")

names(location_lookup) <- c("C407", "C400", "C408", "DEL_SWP_TOTAL", "DEL_CVP_TOTAL", 
                            "S1", "S4", "S6", "S8")


# LOAD IN CALSIM RESULTS FROM THE 2008 and 2009 BiOp --------------------------------------------------
col_names <- readxl::read_excel("CalSim-data/data-raw/2008_2009_Biop.xlsx", skip = 1) %>% 
  rename(date = "...2") %>%
  names()

BiOp_2008_2009 <- readxl::read_excel("CalSim-data/data-raw/2008_2009_Biop.xlsx", skip = 7, col_names = col_names) %>% 
  select(-B) %>% 
  mutate(date = as_date(date)) %>%
  filter(year(date) >= 1922) %>%
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
  filter(year(date) >= 1922) %>%
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

all_dat <- bind_rows(BiOp_2008_2009, BiOp_2019_Itp_2020) %>% glimpse

# Other values
all_dat %>% 
  filter(!(calsim_node %in% c("S1", "S4", "S6", "S8")), 
         year(date) > 1998 & year(date) < 2000) %>% # filter time down for easier viewing 
  ggplot() + 
  geom_line(aes(x = date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") + 
  theme_minimal()

# Resivor values
all_dat %>% 
  filter(calsim_node %in% c("S1", "S4", "S6", "S8"), 
         year(date) > 1995) %>% # filter time down for easier viewing 
  ggplot() + 
  geom_line(aes(x = date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") +
  theme_minimal()



# 1995 - end of data
# Each node separate graph 
graph_node <- function(node){
  y_lab = ifelse(node %in% c("S1", "S4", "S6", "S8"), "Storage (total acre feet)", "Flow (cfs)" )
  title_lab = paste("2008 & 2009 BiOp and 2019 BiOp & 2020 ITP for", 
                    filter(all_dat, calsim_node == node) %>% pull(location), "Node")
  all_dat %>% 
    filter(calsim_node == node, 
           year(date) >= 1995) %>% # filter time down for easier viewing 
    ggplot() + 
    geom_line(aes(x = date, y = value, color = calsim_run)) +
    theme_minimal() +
    labs(title = title_lab,
         y = y_lab) + 
    theme(text = element_text(size = 14))
  
  ggsave(paste0('CalSim-data/figures/plot_calsim_node_', node,'.png'),
         device = 'png', width = 16, height = 10, units = 'in')
}
nodes = c("C407", "C400", "C408", "DEL_SWP_TOTAL", "DEL_CVP_TOTAL", 
          "S1", "S4", "S6", "S8")
purrr::map(nodes, graph_node)

# Faceted graphs zooming into one year wet (1998)
all_dat %>% 
  mutate(water_year = ifelse(month(date) >= 10, year(date) + 1, year(date)),
         month = month(date), 
         day = day(date),
         fake_date = ymd(paste0(year(date), "-", month, "-", day))) %>% 
  filter(!(calsim_node %in% c("S1", "S4", "S6", "S8")), 
         water_year == 1998) %>%  
  ggplot() + 
  geom_line(aes(x = fake_date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") + 
  theme_minimal() + 
  theme(text = element_text(size = 14)) +
  labs(title = "2008 & 2009 BiOp and 2019 BiOp & 2020 ITP for Water Year 1998 (Wet)",
       y = "Flow (cfs)",
       x = "Date") 

ggsave('CalSim-data/figures/wet_year_1998_calsim_nodes.png',
       device = 'png', width = 16, height = 10, units = 'in')

# Reservoir values
all_dat %>% 
  mutate(water_year = ifelse(month(date) >= 10, year(date) + 1, year(date)),
         month = month(date), 
         day = day(date),
         fake_date = ymd(paste0(year(date), "-", month, "-", day))) %>% 
  filter(calsim_node %in% c("S1", "S4", "S6", "S8"), 
         year(date) == 1998) %>% # filter time down for easier viewing 
  ggplot() + 
  geom_line(aes(x = date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") +
  theme_minimal() + 
  theme(text = element_text(size = 14)) +
  labs(title = "2008 & 2009 BiOp and 2019 BiOp & 2020 ITP for Water Year 1998 (Wet)",
       y = "Storage (total acre feet)",
       x = "Date") 

ggsave('CalSim-data/figures/wet_year_1998_calsim_storage_nodes.png',
       device = 'png', width = 16, height = 10, units = 'in')
# Faceted graphs zooming into one year dry (2001/2002 check)
all_dat %>% 
  mutate(water_year = ifelse(month(date) >= 10, year(date) + 1, year(date)),
         month = month(date), 
         day = day(date),
         fake_date = ymd(paste0(year(date), "-", month, "-", day))) %>% 
  filter(!(calsim_node %in% c("S1", "S4", "S6", "S8")), 
         water_year == 2001) %>%  
  ggplot() + 
  geom_line(aes(x = fake_date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") + 
  theme_minimal() + 
  theme(text = element_text(size = 14)) +
  labs(title = "2008 & 2009 BiOp and 2019 BiOp & 2020 ITP for Water Year 2001 (Dry)",
       y = "Flow (cfs)",
       x = "Date") 

ggsave('CalSim-data/figures/dry_year_2001_calsim_nodes.png',
       device = 'png', width = 16, height = 10, units = 'in')

# Reservoir values
all_dat %>% 
  mutate(water_year = ifelse(month(date) >= 10, year(date) + 1, year(date)),
         month = month(date), 
         day = day(date),
         fake_date = ymd(paste0(year(date), "-", month, "-", day))) %>% 
  filter(calsim_node %in% c("S1", "S4", "S6", "S8"), 
         year(date) == 2001) %>% # filter time down for easier viewing 
  ggplot() + 
  geom_line(aes(x = date, y = value, color = calsim_run)) +
  facet_wrap(~location, scales = "free") +
  theme_minimal() + 
  theme(text = element_text(size = 14)) +
  labs(title = "2008 & 2009 BiOp and 2019 BiOp & 2020 ITP for Water Year 2001 (Dry)",
       y = "Storage (total acre feet)",
       x = "Date") 

ggsave('CalSim-data/figures/dry_year_2001_calsim_storage_nodes.png',
       device = 'png', width = 16, height = 10, units = 'in')
