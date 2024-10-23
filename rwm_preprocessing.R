# RWM Preprocessing

rm(list = ls())
setwd("Z:/Shared/Shiny/2024/rwm_dash")

library(tidyverse)
library(sf)
library(arrow)

rwm<- readRDS("Z:/Shared/rechte Demos_v3/heatmap_data/data_kreise.rds") # already merged with district shapefiles


##  Create a complete geometry grid for all years
rwm1 <- rwm %>%
  group_by(year, geometry) %>%
  summarise(participants=sum(participants, na.rm = TRUE)+1,
            demo_count= n()+1) %>%
  ungroup()

# get the cross product of years and geometry, add kreise
YG_grid <- crossing(year = unique(rwm$year), geometry = rwm$geometry) %>% left_join(distinct(select(rwm, geometry, kreis)), by = "geometry")


rwm1 <- left_join(YG_grid, rwm1, by = c("year", "geometry"))%>%
  replace_na(list(participants = 1, demo_count = 1)) %>%
  st_as_sf()

saveRDS(rwm1, "data/rwm_shiny.rds")


## Aggregate the participants and demonstration count by year and federal state


# Generate all combinations of yearmonth and state
all_combinations <- rwm %>% 
  st_set_geometry(NULL) %>%
  select(year, state) %>%
  distinct() %>%
  complete(year = unique(rwm$year), state = unique(rwm$state)) %>% 
  na.omit()

# Aggregate the data
rwm2 <- rwm  %>%
  st_set_geometry(NULL) %>%
  group_by(year, state) %>%
  summarise(participants = sum(participants, na.rm = TRUE),
            demo_count = n()) %>%
  ungroup()  


# Merge with all_combinations to retain missing months/states
rwm2 <- all_combinations %>%
  left_join(rwm2, by = c("year", "state")) %>%
  mutate(participants = ifelse(is.na(participants), 0, participants),
         demo_count = ifelse(is.na(demo_count), 0, demo_count),
         year = substr(year, 1, 4))

# Save the data
write_parquet(rwm2, "data/rwm_stateyear.parquet")
getwd()
rwm3 <-  readRDS("Z:/Shared/rechte Demos_v3/data/data_clean23.rds")
write_parquet(rwm3, "data/data_clean23.parquet")

