# generate map of world regions with some heatwave statistics

library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(geojsonio)
library(htmltools)
library(htmlwidgets)

day_str = function(x)
{
  format(round(x, 1), nsmall = 1)
}

# get input data
regions =
  read_csv('src/regions.csv',
    col_types = cols(
      region_code = col_character(),
      region_name = col_character(),
      .default = col_double()))
  # mutate(
    # wkt = paste0(
    #   '"POLYGON ((',
    #   lon_min, ' ', lat_min, ', ',
    #   lon_min, ' ', lat_max, ', ',
    #   lon_max, ' ', lat_max, ', ',
    #   lon_max, ' ', lat_min, ', ',
    #   lon_min, ' ', lat_min, '))'),
    # shape = lapply(
      # paste0('region_shapes/', region_code, '.geojson'),
      # geojson_read, what = 'sp'))
  
dummy =
  data_frame(
    region_code = c(regions$region_code),
    hw_days_est_1 = runif(length(region_code), min = 1, max = 6),
    hw_days_lo_1 = hw_days_est_1 - 0.5,
    hw_days_hi_1 = hw_days_est_1 + 0.5,
    hw_length_est_1 = round(runif(length(region_code), min = 0, max = 4)),
    hw_length_lo_1 = pmax(hw_length_est_1 - 1, 0),
    hw_length_hi_1 = pmax(hw_length_est_1 + 1, 0),
    hw_days_est_2 = runif(length(region_code), min = 4, max = 10),
    hw_days_lo_2 = hw_days_est_2 - 0.7,
    hw_days_hi_2 = hw_days_est_2 + 0.7,
    hw_length_est_2 = round(runif(length(region_code), min = 2, max = 6)),
    hw_length_lo_2 = pmax(hw_length_est_2 - 1.2, 0),
    hw_length_hi_2 = pmax(hw_length_est_2 + 1.2, 0),
    hw_days_est_3 = runif(length(region_code), min = 7, max = 15),
    hw_days_lo_3 = hw_days_est_3 - 1.3,
    hw_days_hi_3 = hw_days_est_3 + 1.3,
    hw_length_est_3 = round(runif(length(region_code), min = 6, max = 11)),
    hw_length_lo_3 = pmax(hw_length_est_3 - 1.8, 0),
    hw_length_hi_3 = pmax(hw_length_est_3 + 1.8, 0)) %>%
  inner_join(regions)
  #mutate(warming = as.numeric(warming))

# experiment #1: plot it with leaflet
hw_pal = colorNumeric(
  substr(viridis(n = 5, alpha = 1, begin = 1, end = 0.5, option = 'inferno'), 1, 7),
  domain = c(0, 15))

leaflet(dummy, options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addRectangles(group = '1 °C warming',
    lng1 = ~ lon_min, lng2 = ~ lon_max, lat1 = ~ lat_min, lat2 = ~ lat_max,
    fillColor = ~ hw_pal(hw_days_est_1), fillOpacity = 0.5,
    stroke = TRUE, weight = 1, color = 'black',
    popup = ~ paste0(
      '<strong>', region_name, ' (', region_code, ')</strong><br/><br/>',
      day_str(hw_days_est_1), ' more heatwave days with 1°C of global warming.')) %>%
  addRectangles(group = '2 °C warming',
    lng1 = ~ lon_min, lng2 = ~ lon_max, lat1 = ~ lat_min, lat2 = ~ lat_max,
    fillColor = ~ hw_pal(hw_days_est_2), fillOpacity = 0.5,
    stroke = TRUE, weight = 1, color = 'black',
    popup = ~ paste0(
      '<strong>', region_name, ' (', region_code, ')</strong><br/><br/>',
      day_str(hw_days_est_2), ' more heatwave days with 1°C of global warming.')) %>%
  addRectangles(group = '3 °C warming',
    lng1 = ~ lon_min, lng2 = ~ lon_max, lat1 = ~ lat_min, lat2 = ~ lat_max,
    fillColor = ~ hw_pal(hw_days_est_3), fillOpacity = 0.5,
    stroke = TRUE, weight = 1, color = 'black',
    popup = ~ paste0(
      '<strong>', region_name, ' (', region_code, ')</strong><br/><br/>',
      day_str(hw_days_est_3), ' more heatwave days with 3°C of global warming.')) %>%
  addLayersControl(baseGroups = c('1 °C warming', '2 °C warming', '3 °C warming'),
     options = layersControlOptions(collapsed = FALSE))
  # pipe to %>% saveWidget('test.html') to export!
  # next: pretty up with popupOptions and add real data



