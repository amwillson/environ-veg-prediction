#### STEP 1-2

## Combining climate, soil and vegetation data
## Individual dataframes aggregated to the 8 x 8 km grid
## are simply combined for running the models
## All variables are plotted

## 1. Vegetation
## 2. Soil
## 3. Climate

## Input: data/processed/PLS/gridded_fcomp_density.RData
## PLS-era total stem density and fractional composition in each grid cell
## Created in step 1-1

## Input: data/raw/soils/gridded_soil.RData
## Gridded soil variables from gSSURGO
## Created in separate repository: amwillson/historic-modern-environment

## Input: data/raw/climate/gridded_climate.RData
## Gridded climate variables from PRISM
## Created in separate repository: amwillson/historic-modern-environment

## Output: data/processed/PLS/xydata.RData
## Gridded soil, climate, and vegetation variables
## on 8 x 8 km grid where each row is a grid cell
## Used in 1.3.Split_data.R

rm(list = ls())

#### 1. Vegetation ####

# Load data
load('data/processed/PLS/gridded_fcomp_density.RData')

# State outlines
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))

states <- sf::st_transform(states, crs = 'EPSG:3175')

# Production plots of data
density_fcomp_df |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                 direction = 1,
                                 na.value = '#00000000',
                                 name = 'stems/ha') +
  ggplot2::ggtitle('Total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

density_fcomp_df |>
  tidyr::pivot_longer(cols = Ash:`Poplar/tulip poplar`,
                      names_to = 'taxon',
                      values_to = 'fcomp') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fcomp)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'D', direction = -1,
                                na.value = '#00000000',
                                limits = c(0, 1),
                                name = 'Fraction total\nstems') +
  ggplot2::ggtitle('Relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### 2. Soil ####

# Load gridded soil data products
load('data/raw/soils/gridded_soil.RData')

# Join with vegetation data
density_fcomp_soil <- density_fcomp_df |>
  dplyr::left_join(y = dplyr::select(veg_unique_grid, -loc),
                   by = c('x', 'y'))

# Plot soil variables
density_fcomp_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                 name = '% clay',
                                 limits = c(0, 100),
                                direction = 1) +
  ggplot2::ggtitle('Soil clay content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

density_fcomp_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% sand',
                                limits = c(0, 100),
                                direction = 1) +
  ggplot2::ggtitle('Soil sand content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

density_fcomp_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% silt',
                                limits = c(0, 100),
                                direction = 1) +
  ggplot2::ggtitle('Soil silt content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

density_fcomp_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = expression(paste('% CaC', O[3])),
                                direction = 1) +
  ggplot2::ggtitle('Soil calcium carbonate concentration') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

density_fcomp_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = 'cm/cm',
                                direction = 1) +
  ggplot2::ggtitle('Available water content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

density_fcomp_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = flood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = 'Fraction of\ngrid cell',
                                limits = c(0, 1),
                                direction = 1) +
  ggplot2::ggtitle('Fraction of grid cell in a floodplain') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### 3. Climate ####

# Load gridded climate data products
load('data/raw/climate/gridded_climate.RData')

# Join climate to vegetation and soil data
xydata <- density_fcomp_soil |>
  dplyr::left_join(y = dplyr::select(veg_unique_grid, -loc),
                   by = c('x', 'y'))

# Plot climate variables
xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_sum)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'D',
                                name = 'mm/year',
                                direction = -1) +
  ggplot2::ggtitle('Total annual precipitaiton') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'A',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'B',
                                name = 'mm/year') +
  ggplot2::ggtitle('Standard deviation of total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  dplyr::mutate(ppt_cv = ppt_cv * 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'B',
                                name = '%') +
  ggplot2::ggtitle('Coefficient of variation of total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'C',
                                name = '°C') +
  ggplot2::ggtitle('Standard deviation of average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  dplyr::mutate(tmean_cv = tmean_cv * 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'C',
                                name = '%') +
  ggplot2::ggtitle('Coefficient of variation of average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'E',
                                name = '°C') +
  ggplot2::ggtitle('Minimum annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'F',
                                name = '°C') +
  ggplot2::ggtitle('Maximum annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = vpdmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G',
                                name = 'hPa') +
  ggplot2::ggtitle('Maximum annual vapor pressure deficit') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Save
save(xydata,
     file = 'data/processed/PLS/xydata.RData')
