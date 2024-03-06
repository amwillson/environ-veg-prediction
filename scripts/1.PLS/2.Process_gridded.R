## Formatting 8 km data

rm(list = ls())

# Load gridded data
fc <- ncdf4::nc_open('data/raw/gridded-composition/SetTreeComp_Level2_v1.0.nc')
dens <- ncdf4::nc_open('data/raw/gridded-density/PLS_Density_Point_Level2_v1.0.nc')

# Get dimensions
fc_x <- ncdf4::ncvar_get(fc, 'x')
dens_x <- ncdf4::ncvar_get(dens, 'x')
fc_y <- ncdf4::ncvar_get(fc, 'y')
dens_y <- ncdf4::ncvar_get(dens, 'y')
fc_samp <- ncdf4::ncvar_get(fc, 'sample')

# Get total density data
tot_dens <- ncdf4::ncvar_get(dens, 'Total')
# Add coordinates as column and row names
dimnames(tot_dens) <- list(dens_x, dens_y)
# Melt to dataframe
dens_melt <- reshape2::melt(tot_dens)
# Replace column names
colnames(dens_melt) <- c('x', 'y', 'density')
# Remove NAs, which are locations that don't exist
dens_melt <- dens_melt |>
  tidyr::drop_na()

# Plot of states
states <- sf::st_as_sf(maps::map('state', region = c('indiana', 'illinois',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot density to make sure things look correct
dens_melt |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1) +
  ggplot2::theme_void()

# Repeat cleaning steps for fractional composition
# We want to be able to match the locations from each data product
# we don't use cedar or chestnut because they're outside our
# region of interest
ash <- ncdf4::ncvar_get(fc, 'Ash')
basswood <- ncdf4::ncvar_get(fc, 'Basswood')
beech <- ncdf4::ncvar_get(fc, 'Beech')
birch <- ncdf4::ncvar_get(fc, 'Birch')
cherry <- ncdf4::ncvar_get(fc, 'Cherry')
dogwood <- ncdf4::ncvar_get(fc, 'Dogwood')
elm <- ncdf4::ncvar_get(fc, 'Elm')
fir <- ncdf4::ncvar_get(fc, 'Fir')
hemlock <- ncdf4::ncvar_get(fc, 'Hemlock')
hickory <- ncdf4::ncvar_get(fc, 'Hickory')
ironwood <- ncdf4::ncvar_get(fc, 'Ironwood')
maple <- ncdf4::ncvar_get(fc, 'Maple')
oak <- ncdf4::ncvar_get(fc, 'Oak')
pine <- ncdf4::ncvar_get(fc, 'Pine')
spruce <- ncdf4::ncvar_get(fc, 'Spruce')
tamarack <- ncdf4::ncvar_get(fc, 'Tamarack')
walnut <- ncdf4::ncvar_get(fc, 'Walnut')
other_hardwood <- ncdf4::ncvar_get(fc, 'Other hardwood')
gum <- ncdf4::ncvar_get(fc, 'Black gum/sweet gum')
juniper <- ncdf4::ncvar_get(fc, 'Cedar/juniper')
poplar <- ncdf4::ncvar_get(fc, 'Poplar/tulip poplar')

# Add coordinates
dimnames(ash) <- list(fc_x, fc_y, fc_samp)
dimnames(cedar) <- list(fc_x, fc_y, fc_samp)
dimnames(basswood) <- list(fc_x, fc_y, fc_samp)
dimnames(beech) <- list(fc_x, fc_y, fc_samp)
dimnames(birch) <- list(fc_x, fc_y, fc_samp)
dimnames(cherry) <- list(fc_x, fc_y, fc_samp)
dimnames(chestnut) <- list(fc_x, fc_y, fc_samp)
dimnames(dogwood) <- list(fc_x, fc_y, fc_samp)
dimnames(elm) <- list(fc_x, fc_y, fc_samp)
dimnames(fir) <- list(fc_x, fc_y, fc_samp)
dimnames(hemlock) <- list(fc_x, fc_y, fc_samp)
dimnames(hickory) <- list(fc_x, fc_y, fc_samp)
dimnames(ironwood) <- list(fc_x, fc_y, fc_samp)
dimnames(maple) <- list(fc_x, fc_y, fc_samp)
dimnames(oak) <- list(fc_x, fc_y, fc_samp)
dimnames(pine) <- list(fc_x, fc_y, fc_samp)
dimnames(spruce) <- list(fc_x, fc_y, fc_samp)
dimnames(tamarack) <- list(fc_x, fc_y, fc_samp)
dimnames(walnut) <- list(fc_x, fc_y, fc_samp)
dimnames(other_hardwood) <- list(fc_x, fc_y, fc_samp)
dimnames(gum) <- list(fc_x, fc_y, fc_samp)
dimnames(juniper) <- list(fc_x, fc_y, fc_samp)
dimnames(poplar) <- list(fc_x, fc_y, fc_samp)

# Summarize over samples
ash <- apply(ash, 1:2, mean)
basswood <- apply(basswood, 1:2, mean)
beech <- apply(beech, 1:2, mean)
birch <- apply(birch, 1:2, mean)
cherry <- apply(cherry, 1:2, mean)
dogwood <- apply(dogwood, 1:2, mean)
elm <- apply(elm, 1:2, mean)
fir <- apply(fir, 1:2, mean)
hemlock <- apply(hemlock, 1:2, mean)
hickory <- apply(hickory, 1:2, mean)
ironwood <- apply(ironwood, 1:2, mean)
maple <- apply(maple, 1:2, mean)
oak <- apply(oak, 1:2, mean)
pine <- apply(pine, 1:2, mean)
spruce <- apply(spruce, 1:2, mean)
tamarack <- apply(tamarack, 1:2, mean)
walnut <- apply(walnut, 1:2, mean)
other_hardwood <- apply(other_hardwood, 1:2, mean)
gum <- apply(gum, 1:2, mean)
juniper <- apply(juniper, 1:2, mean)
poplar <- apply(poplar, 1:2, mean)

# Melt
ash <- reshape2::melt(ash)
basswood <- reshape2::melt(basswood)
beech <- reshape2::melt(beech)
birch <- reshape2::melt(birch)
cherry <- reshape2::melt(cherry)
dogwood <- reshape2::melt(dogwood)
elm <- reshape2::melt(elm)
fir <- reshape2::melt(fir)
hemlock <- reshape2::melt(hemlock)
hickory <- reshape2::melt(hickory)
ironwood <- reshape2::melt(ironwood)
maple <- reshape2::melt(maple)
oak <- reshape2::melt(oak)
pine <- reshape2::melt(pine)
spruce <- reshape2::melt(spruce)
tamarack <- reshape2::melt(tamarack)
walnut <- reshape2::melt(walnut)
other_hardwood <- reshape2::melt(other_hardwood)
gum <- reshape2::melt(gum)
juniper <- reshape2::melt(juniper)
poplar <- reshape2::melt(poplar)

# Rename columns
colnames(ash) <- c('x', 'y', 'ash')
colnames(basswood) <- c('x', 'y', 'basswood')
colnames(beech) <- c('x', 'y', 'beech')
colnames(birch) <- c('x', 'y', 'birch')
colnames(cherry) <- c('x', 'y', 'cherry')
colnames(dogwood) <- c('x', 'y', 'dogwood')
colnames(elm) <- c('x', 'y', 'elm')
colnames(fir) <- c('x', 'y', 'fir')
colnames(hemlock) <- c('x', 'y', 'hemlock')
colnames(hickory) <- c('x', 'y', 'hickory')
colnames(ironwood) <- c('x', 'y', 'ironwood')
colnames(maple) <- c('x', 'y', 'maple')
colnames(oak) <- c('x', 'y', 'oak')
colnames(pine) <- c('x', 'y', 'pine')
colnames(spruce) <- c('x', 'y', 'spruce')
colnames(tamarack) <- c('x', 'y', 'tamarack')
colnames(walnut) <- c('x', 'y', 'walnut')
colnames(other_hardwood) <- c('x', 'y', 'other_hardwood')
colnames(gum) <- c('x', 'y', 'gum')
colnames(juniper) <- c('x', 'y', 'juniper')
colnames(poplar) <- c('x', 'y', 'poplar')

# Remove NAs
ash <- tidyr::drop_na(ash)
basswood <- tidyr::drop_na(basswood)
beech <- tidyr::drop_na(beech)
birch <- tidyr::drop_na(birch)
cherry <- tidyr::drop_na(cherry)
dogwood <- tidyr::drop_na(dogwood)
elm <- tidyr::drop_na(elm)
fir <- tidyr::drop_na(fir)
hemlock <- tidyr::drop_na(hemlock)
hickory <- tidyr::drop_na(hickory)
ironwood <- tidyr::drop_na(ironwood)
maple <- tidyr::drop_na(maple)
oak <- tidyr::drop_na(oak)
pine <- tidyr::drop_na(pine)
spruce <- tidyr::drop_na(spruce)
tamarack <- tidyr::drop_na(tamarack)
walnut <- tidyr::drop_na(walnut)
other_hardwood <- tidyr::drop_na(other_hardwood)
gum <- tidyr::drop_na(gum)
juniper <- tidyr::drop_na(juniper)
poplar <- tidyr::drop_na(poplar)

# Put together
comp <- ash |>
  dplyr::left_join(basswood, by = c('x', 'y')) |>
  dplyr::left_join(beech, by = c('x', 'y')) |>
  dplyr::left_join(birch, by = c('x', 'y')) |>
  dplyr::left_join(cherry, by = c('x', 'y')) |>
  dplyr::left_join(dogwood, by = c('x', 'y')) |>
  dplyr::left_join(elm, by = c('x', 'y')) |>
  dplyr::left_join(fir, by = c('x', 'y')) |>
  dplyr::left_join(hemlock, by = c('x', 'y')) |>
  dplyr::left_join(hickory, by = c('x', 'y')) |>
  dplyr::left_join(ironwood, by = c('x', 'y')) |>
  dplyr::left_join(maple, by = c('x', 'y')) |>
  dplyr::left_join(oak, by = c('x', 'y')) |>
  dplyr::left_join(pine, by = c('x', 'y')) |>
  dplyr::left_join(spruce, by = c('x', 'y')) |>
  dplyr::left_join(tamarack, by = c('x', 'y')) |>
  dplyr::left_join(walnut, by = c('x', 'y')) |>
  dplyr::left_join(other_hardwood, by = c('x', 'y')) |>
  dplyr::left_join(gum, by = c('x', 'y')) |>
  dplyr::left_join(poplar, by = c('x', 'y'))

# Combine with density
comp_dens <- dens_melt |>
  dplyr::left_join(comp, by = c('x', 'y'))

# Plot a couple things to make sure it looks right
comp_dens |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1)

comp_dens |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1)

comp_dens |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1)

comp_dens |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1)

comp_dens |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1)

# For each grid cell (x,y pair), make a unique id number
comp_dens <- comp_dens |>
  dplyr::mutate(id = dplyr::row_number())

# Save
save(comp_dens, file = 'data/processed/PLS/8km.RData')
