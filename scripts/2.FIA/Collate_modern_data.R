## Collate modern data

rm(list = ls())

#### 1. Vegetation ####

# Load data
load('data/processed/FIA/gridded_all_plots.RData')

# Take only necessary columns from stem density
fia_density <- stem_density_agg2 |>
  dplyr::select(cell, x, y, total_stem_density) |>
  dplyr::distinct()

# Pivot fractional composition to match PLS format
fia_fc <- fractional_composition_agg2 |>
  dplyr::select(cell, x, y, taxon, fcomp) |>
  tidyr::pivot_wider(names_from = 'taxon',
                     values_from = fcomp)

# Combine
fia_density_fcomp <- fia_density |>
  dplyr::full_join(y = fia_fc,
                   by = c('cell', 'x', 'y')) |>
  # Replace NA with very small number because that's how
  # PLS works (PLS estimates can't be exactly 0)
  dplyr::mutate(Ash = dplyr::if_else(is.na(Ash), 1e-5, Ash),
                Birch = dplyr::if_else(is.na(Birch), 1e-5, Birch),
                `Cedar/juniper` = dplyr::if_else(is.na(`Cedar/juniper`), 1e-5, `Cedar/juniper`),
                Fir = dplyr::if_else(is.na(Fir), 1e-5, Fir),
                `Poplar/tulip poplar` = dplyr::if_else(is.na(`Poplar/tulip poplar`), 1e-5, `Poplar/tulip poplar`),
                Spruce = dplyr::if_else(is.na(Spruce), 1e-5, Spruce),
                Tamarack = dplyr::if_else(is.na(Tamarack), 1e-5, Tamarack),
                Oak = dplyr::if_else(is.na(Oak), 1e-5, Oak),
                Elm = dplyr::if_else(is.na(Elm), 1e-5, Elm),
                Pine = dplyr::if_else(is.na(Pine), 1e-5, Pine),
                Maple = dplyr::if_else(is.na(Maple), 1e-5, Maple),
                Basswood = dplyr::if_else(is.na(Basswood), 1e-5, Basswood),
                `Other hardwood` = dplyr::if_else(is.na(`Other hardwood`), 1e-5, `Other hardwood`),
                Cherry = dplyr::if_else(is.na(Cherry), 1e-5, Cherry),
                Ironwood = dplyr::if_else(is.na(Ironwood), 1e-5, Ironwood),
                Walnut = dplyr::if_else(is.na(Walnut), 1e-5, Walnut),
                Hemlock = dplyr::if_else(is.na(Hemlock), 1e-5, Hemlock),
                Hickory = dplyr::if_else(is.na(Hickory), 1e-5, Hickory),
                Beech = dplyr::if_else(is.na(Beech), 1e-5, Beech),
                `Black gum/sweet gum` = dplyr::if_else(is.na(`Black gum/sweet gum`), 1e-5, `Black gum/sweet gum`),
                Dogwood = dplyr::if_else(is.na(Dogwood), 1e-5, Dogwood))

# States outlines
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Production plots of data
fia_density_fcomp |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'stems/ha') +
  ggplot2::ggtitle('Total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

fia_density_fcomp |>
  tidyr::pivot_longer(cols = Ash:Dogwood,
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
density_fcomp_soil <- fia_density_fcomp |>
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
load('data/raw/climate/gridded_climate_modern.Rdata')

# Join climate to vegetation and soil data
xydata_modern <- density_fcomp_soil |>
  dplyr::left_join(y = dplyr::select(veg_unique_grid, -loc),
                   by = c('x', 'y'))

# Plot climate variables
xydata_modern |>
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

xydata_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'A',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'B',
                                name = 'mm/year') +
  ggplot2::ggtitle('Standard deviation of total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata_modern |>
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

xydata_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'C',
                                name = '°C') +
  ggplot2::ggtitle('Standard deviation of average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata_modern |>
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

xydata_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'E',
                                name = '°C') +
  ggplot2::ggtitle('Minimum annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'F',
                                name = '°C') +
  ggplot2::ggtitle('Maximum annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xydata_modern |>
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
save(xydata_modern,
     file = 'data/processed/FIA/xydata.RData')
