# generate map of world regions with some heatwave statistics

library(readr)
library(dplyr)
library(tidyr)
library(viridis)
library(leaflet)
library(geojsonio)
library(htmltools)
library(htmlwidgets)

day_str = function(x)
{
  format(round(as.numeric(x), 1), nsmall = 1)
}
# nice yellow-to-purple colour palette
hw_pal = colorNumeric(
  substr(viridis(n = 6, alpha = 1, begin = 1, end = 0, option = 'inferno'), 1, 7),
  domain = c(0, 150))

# render leaflet map
# unfortunately, we have to add each region, for each degree of warming,
# one at a time. this is gonna be a lot of code.
hw_plot = leaflet(options = leafletOptions(minZoom = 1, maxZoom = 5)) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  setView(lat = 0, lng = 0, zoom = 2) %>%
  addLegend('bottomleft',
    colors = substr(viridis(n = 6, alpha = 1, begin = 1, end = 0, option = 'inferno'), 1, 7),
    title = "Additional heatwave<br/>days per year",
    labels = c('0', '30', '60', '90', '120', '150'),
    opacity = 0.75) %>%
  ### AUS
  # AUS, 1 degree
  addPolygons(data = geojson_read('region_shapes/AUS.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # AUS, 2 degrees
  addPolygons(data = geojson_read('region_shapes/AUS.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # AUS, 3 degrees
  addPolygons(data = geojson_read('region_shapes/AUS.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # AUS, 4 degrees
  addPolygons(data = geojson_read('region_shapes/AUS.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # AUS, 5 degrees
  addPolygons(data = geojson_read('region_shapes/AUS.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### AMZ
  # AMZ, 1 degree
  addPolygons(data = geojson_read('region_shapes/AMZ.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # AMZ, 2 degrees
  addPolygons(data = geojson_read('region_shapes/AMZ.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # AMZ, 3 degrees
  addPolygons(data = geojson_read('region_shapes/AMZ.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # AMZ, 4 degrees
  addPolygons(data = geojson_read('region_shapes/AMZ.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # AMZ, 5 degrees
  addPolygons(data = geojson_read('region_shapes/AMZ.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### SSA
  # SSA, 1 degree
  addPolygons(data = geojson_read('region_shapes/SSA.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # SSA, 2 degrees
  addPolygons(data = geojson_read('region_shapes/SSA.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # SSA, 3 degrees
  addPolygons(data = geojson_read('region_shapes/SSA.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # SSA, 4 degrees
  addPolygons(data = geojson_read('region_shapes/SSA.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # SSA, 5 degrees
  addPolygons(data = geojson_read('region_shapes/SSA.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### CAM
  # CAM, 1 degree
  addPolygons(data = geojson_read('region_shapes/CAM.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # CAM, 2 degrees
  addPolygons(data = geojson_read('region_shapes/CAM.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # CAM, 3 degrees
  addPolygons(data = geojson_read('region_shapes/CAM.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # CAM, 4 degrees
  addPolygons(data = geojson_read('region_shapes/CAM.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # CAM, 5 degrees
  addPolygons(data = geojson_read('region_shapes/CAM.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### WNA
  # WNA, 1 degree
  addPolygons(data = geojson_read('region_shapes/WNA.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # WNA, 2 degrees
  addPolygons(data = geojson_read('region_shapes/WNA.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # WNA, 3 degrees
  addPolygons(data = geojson_read('region_shapes/WNA.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # WNA, 4 degrees
  addPolygons(data = geojson_read('region_shapes/WNA.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # WNA, 5 degrees
  addPolygons(data = geojson_read('region_shapes/WNA.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### CNA
  # CNA, 1 degree
  addPolygons(data = geojson_read('region_shapes/CNA.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # CNA, 2 degrees
  addPolygons(data = geojson_read('region_shapes/CNA.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # CNA, 3 degrees
  addPolygons(data = geojson_read('region_shapes/CNA.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # CNA, 4 degrees
  addPolygons(data = geojson_read('region_shapes/CNA.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # CNA, 5 degrees
  addPolygons(data = geojson_read('region_shapes/CNA.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### ENA
  # ENA, 1 degree
  addPolygons(data = geojson_read('region_shapes/ENA.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # ENA, 2 degrees
  addPolygons(data = geojson_read('region_shapes/ENA.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # ENA, 3 degrees
  addPolygons(data = geojson_read('region_shapes/ENA.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # ENA, 4 degrees
  addPolygons(data = geojson_read('region_shapes/ENA.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # ENA, 5 degrees
  addPolygons(data = geojson_read('region_shapes/ENA.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### ALA
  # ALA, 1 degree
  addPolygons(data = geojson_read('region_shapes/ALA.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # ALA, 2 degrees
  addPolygons(data = geojson_read('region_shapes/ALA.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # ALA, 3 degrees
  addPolygons(data = geojson_read('region_shapes/ALA.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # ALA, 4 degrees
  addPolygons(data = geojson_read('region_shapes/ALA.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # ALA, 5 degrees
  addPolygons(data = geojson_read('region_shapes/ALA.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### GRL
  # GRL, 1 degree
  addPolygons(data = geojson_read('region_shapes/GRL.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # GRL, 2 degrees
  addPolygons(data = geojson_read('region_shapes/GRL.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # GRL, 3 degrees
  addPolygons(data = geojson_read('region_shapes/GRL.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # GRL, 4 degrees
  addPolygons(data = geojson_read('region_shapes/GRL.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # GRL, 5 degrees
  addPolygons(data = geojson_read('region_shapes/GRL.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### MED
  # MED, 1 degree
  addPolygons(data = geojson_read('region_shapes/MED.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # MED, 2 degrees
  addPolygons(data = geojson_read('region_shapes/MED.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # MED, 3 degrees
  addPolygons(data = geojson_read('region_shapes/MED.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # MED, 4 degrees
  addPolygons(data = geojson_read('region_shapes/MED.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # MED, 5 degrees
  addPolygons(data = geojson_read('region_shapes/MED.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### NEU
  # NEU, 1 degree
  addPolygons(data = geojson_read('region_shapes/NEU.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # NEU, 2 degrees
  addPolygons(data = geojson_read('region_shapes/NEU.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # NEU, 3 degrees
  addPolygons(data = geojson_read('region_shapes/NEU.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # NEU, 4 degrees
  addPolygons(data = geojson_read('region_shapes/NEU.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # NEU, 5 degrees
  addPolygons(data = geojson_read('region_shapes/NEU.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### WAF
  # WAF, 1 degree
  addPolygons(data = geojson_read('region_shapes/WAF.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # WAF, 2 degrees
  addPolygons(data = geojson_read('region_shapes/WAF.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # WAF, 3 degrees
  addPolygons(data = geojson_read('region_shapes/WAF.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # WAF, 4 degrees
  addPolygons(data = geojson_read('region_shapes/WAF.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # WAF, 5 degrees
  addPolygons(data = geojson_read('region_shapes/WAF.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### EAF
  # EAF, 1 degree
  addPolygons(data = geojson_read('region_shapes/EAF.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # EAF, 2 degrees
  addPolygons(data = geojson_read('region_shapes/EAF.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # EAF, 3 degrees
  addPolygons(data = geojson_read('region_shapes/EAF.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # EAF, 4 degrees
  addPolygons(data = geojson_read('region_shapes/EAF.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # EAF, 5 degrees
  addPolygons(data = geojson_read('region_shapes/EAF.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### SAF
  # SAF, 1 degree
  addPolygons(data = geojson_read('region_shapes/SAF.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # SAF, 2 degrees
  addPolygons(data = geojson_read('region_shapes/SAF.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # SAF, 3 degrees
  addPolygons(data = geojson_read('region_shapes/SAF.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # SAF, 4 degrees
  addPolygons(data = geojson_read('region_shapes/SAF.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # SAF, 5 degrees
  addPolygons(data = geojson_read('region_shapes/SAF.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### SAH
  # SAH, 1 degree
  addPolygons(data = geojson_read('region_shapes/SAH.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # SAH, 2 degrees
  addPolygons(data = geojson_read('region_shapes/SAH.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # SAH, 3 degrees
  addPolygons(data = geojson_read('region_shapes/SAH.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # SAH, 4 degrees
  addPolygons(data = geojson_read('region_shapes/SAH.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # SAH, 5 degrees
  addPolygons(data = geojson_read('region_shapes/SAH.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### SEA
  # SEA, 1 degree
  addPolygons(data = geojson_read('region_shapes/SEA.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # SEA, 2 degrees
  addPolygons(data = geojson_read('region_shapes/SEA.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # SEA, 3 degrees
  addPolygons(data = geojson_read('region_shapes/SEA.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # SEA, 4 degrees
  addPolygons(data = geojson_read('region_shapes/SEA.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # SEA, 5 degrees
  addPolygons(data = geojson_read('region_shapes/SEA.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### EAS
  # EAS, 1 degree
  addPolygons(data = geojson_read('region_shapes/EAS.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # EAS, 2 degrees
  addPolygons(data = geojson_read('region_shapes/EAS.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # EAS, 3 degrees
  addPolygons(data = geojson_read('region_shapes/EAS.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # EAS, 4 degrees
  addPolygons(data = geojson_read('region_shapes/EAS.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # EAS, 5 degrees
  addPolygons(data = geojson_read('region_shapes/EAS.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### SAS
  # SAS, 1 degree
  addPolygons(data = geojson_read('region_shapes/SAS.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # SAS, 2 degrees
  addPolygons(data = geojson_read('region_shapes/SAS.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # SAS, 3 degrees
  addPolygons(data = geojson_read('region_shapes/SAS.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # SAS, 4 degrees
  addPolygons(data = geojson_read('region_shapes/SAS.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # SAS, 5 degrees
  addPolygons(data = geojson_read('region_shapes/SAS.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### CAS
  # CAS, 1 degree
  addPolygons(data = geojson_read('region_shapes/CAS.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # CAS, 2 degrees
  addPolygons(data = geojson_read('region_shapes/CAS.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # CAS, 3 degrees
  addPolygons(data = geojson_read('region_shapes/CAS.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # CAS, 4 degrees
  addPolygons(data = geojson_read('region_shapes/CAS.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # CAS, 5 degrees
  addPolygons(data = geojson_read('region_shapes/CAS.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### TIB
  # TIB, 1 degree
  addPolygons(data = geojson_read('region_shapes/TIB.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # TIB, 2 degrees
  addPolygons(data = geojson_read('region_shapes/TIB.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # TIB, 3 degrees
  addPolygons(data = geojson_read('region_shapes/TIB.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # TIB, 4 degrees
  addPolygons(data = geojson_read('region_shapes/TIB.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # TIB, 5 degrees
  addPolygons(data = geojson_read('region_shapes/TIB.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  ### NAS
  # NAS, 1 degree
  addPolygons(data = geojson_read('region_shapes/NAS.geojson', what = 'sp'), group = '1 °C warming',
    fillColor = ~ hw_pal(HWF_estimate), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 1 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate), '</strong> more heatwave days (',
      day_str(HWF_cilow), ' to ', day_str(HWF_cihigh), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate), '</strong> days longer (',
      day_str(HWD_cilow), ' to ', day_str(HWD_cihigh), ').<br/>',
      '<strong>', day_str(HWM_estimate), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow), ' to ', day_str(HWM_cihigh), ').<br/>',
      '<strong>', day_str(HWA_estimate), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow), ' to ', day_str(HWA_cihigh), ').<br/>')) %>%
  # NAS, 2 degrees
  addPolygons(data = geojson_read('region_shapes/NAS.geojson', what = 'sp'), group = '2 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg2), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 2 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg2), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg2), ' to ', day_str(HWF_cihigh_deg2), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg2), '</strong> days longer (',
      day_str(HWD_cilow_deg2), ' to ', day_str(HWD_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg2), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg2), ' to ', day_str(HWM_cihigh_deg2), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg2), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg2), ' to ', day_str(HWA_cihigh_deg2), ').<br/>')) %>%
  # NAS, 3 degrees
  addPolygons(data = geojson_read('region_shapes/NAS.geojson', what = 'sp'), group = '3 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg3), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 3 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg3), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg3), ' to ', day_str(HWF_cihigh_deg3), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg3), '</strong> days longer (',
      day_str(HWD_cilow_deg3), ' to ', day_str(HWD_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg3), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg3), ' to ', day_str(HWM_cihigh_deg3), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg3), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg3), ' to ', day_str(HWA_cihigh_deg3), ').<br/>')) %>%
  # NAS, 4 degrees
  addPolygons(data = geojson_read('region_shapes/NAS.geojson', what = 'sp'), group = '4 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg4), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 4 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg4), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg4), ' to ', day_str(HWF_cihigh_deg4), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg4), '</strong> days longer (',
      day_str(HWD_cilow_deg4), ' to ', day_str(HWD_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg4), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg4), ' to ', day_str(HWM_cihigh_deg4), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg4), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg4), ' to ', day_str(HWA_cihigh_deg4), ').<br/>')) %>%
  # NAS, 5 degrees
  addPolygons(data = geojson_read('region_shapes/NAS.geojson', what = 'sp'), group = '5 °C warming',
    fillColor = ~ hw_pal(HWF_estimate_deg5), fillOpacity = 0.75, stroke = FALSE,
    popup = ~ paste0(
      '<strong>Annual heatwave statistics for ', region_name,
      ' (', region_code, ') under 5 °C of warming</strong><br/><br/>',
      '<strong>', day_str(HWF_estimate_deg5), '</strong> more heatwave days (',
      day_str(HWF_cilow_deg5), ' to ', day_str(HWF_cihigh_deg5), ').<br/>',
      'Longest heatwave is <strong>', day_str(HWD_estimate_deg5), '</strong> days longer (',
      day_str(HWD_cilow_deg5), ' to ', day_str(HWD_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWM_estimate_deg5), ' °C</strong> increase in mean temperature across all heatwaves (',
      day_str(HWM_cilow_deg5), ' to ', day_str(HWM_cihigh_deg5), ').<br/>',
      '<strong>', day_str(HWA_estimate_deg5), ' °C</strong> increase peak heatwave temperature (',
      day_str(HWA_cilow_deg5), ' to ', day_str(HWA_cihigh_deg5), ').<br/>')) %>%
  # switch warming level in corner
  addLayersControl(
    baseGroups =
      c('1 °C warming', '2 °C warming', '3 °C warming', '4 °C warming', '5 °C warming'),
    options = layersControlOptions(collapsed = TRUE))
  
# save it or export it
saveWidget(hw_plot, 'index.html')

