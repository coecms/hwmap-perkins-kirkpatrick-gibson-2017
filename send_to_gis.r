# send_to_gis.r

library(readr)
library(dplyr)
library(tidyr)

# get input data
hw_stats =
  read_csv('src/sarah_stats.csv',
    col_types = cols(
      region_code = col_character(),
      hw_stat = col_character(),
      .default = col_double())) %>%
  gather(key = aspect, value = value, estimate:iqr) %>%
  unite(var, c('hw_stat', 'aspect')) %>%
  spread(var, value) %>%
  mutate_at(-1, funs(deg2 = . * 2, deg3 = . * 3))
    
  
  
regions =
  read_csv('src/regions.csv',
    col_types = cols(
      region_code = col_character(),
      region_name = col_character(),
      .default = col_double())) %>%
  mutate(
    wkt = paste0(
      'POLYGON ((',
      lon_min, ' ', lat_min, ', ',
      lon_min, ' ', lat_max, ', ',
      lon_max, ' ', lat_max, ', ',
      lon_max, ' ', lat_min, ', ',
      lon_min, ' ', lat_min, '))')) %>%
  inner_join(hw_stats, by = 'region_code') %>%
  write_csv('src/regions_loaded.csv')

