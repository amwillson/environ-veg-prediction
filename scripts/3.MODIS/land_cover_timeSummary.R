## MODIS land cover analysis

rm(list = ls())

# Load in formatted MODIS data
# This contains a land type classification for each pixel
# in each year
load('/Volumes/FileBackup/SDM_bigdata/MODIS/modis_format.RData')

# For each pixel, find the most frequent land
# type classification over time
modis_timesum <- modis |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(LC_1 = names(which.max(table(LC_Type1))),
                   LC_2 = names(which.max(table(LC_Type2))),
                   LC_3 = names(which.max(table(LC_Type3))))

# Map of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Land Cover Type 1
modis_timesum |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~LC_1) +
  ggplot2::theme_void()
