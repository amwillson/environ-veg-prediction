#### STEP 1-1

## Formatting PLS gridded data products
## These are previous PalEON data products publicly archived

## 1. Total stem density
## 2. Fractional composition
## 3. Combine
## 4. Save

## Input: data/raw/gridded_density/PLS_Density_Point_Level2_v1.0.nc
## Estimated stem density from PLS period from DOI:
## https://doi.org/10.6073/pasta/1b2632d48fc79b370740a7c20a70b4b0

## Input: data/raw/gridded-composition/SetTreeComp_Level2_v1.0.nc
## Estimated fractional composition/relative abundance from PLS period from DOI:
## https://doi.org/10.6073/pasta/8544e091b64db26fdbbbafd0699fa4f9

## Output: data/intermediate/PLS/gridded_fcomp_density.RData
## Total stem density and relative abundances for each taxon
## in each 8 x 8 km grid cell (rows) for the PLS period in one dataframe
## Used in 1.2.Collate_data.R

rm(list = ls())

#### 1. Total stem density ####

# Load mean stem density
density <- ncdf4::nc_open('data/raw/gridded-density/PLS_Density_Point_Level2_v1.0.nc')

# Extract total density
total_density <- ncdf4::ncvar_get(density, 'Total')

# Extract coordinates
x <- ncdf4::ncvar_get(density, 'x')
y <- ncdf4::ncvar_get(density, 'y')

# Apply coordinates to total density
colnames(total_density) <- y
rownames(total_density) <- x

# Melt to dataframe
total_density_df <- reshape2::melt(total_density)

# Add column names
colnames(total_density_df) <- c('x', 'y', 'total_density')

# Make map of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana', 'michigan', 'minnesota', 'wisconsin'),
                                 fill = TRUE, plot = FALSE))
# Convert  coordinate system
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot total density
total_density_df |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Greens',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 750)) +
  ggplot2::ggtitle('Historical total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/historical_total_stem_density.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/historical_total_stem_density.svg',
                height = 12, width = 12, units = 'cm')

# Plot ecosystem classification according to total stem density
total_density_df |>
  dplyr::mutate(Ecosystem = dplyr::if_else(total_density < 0.5, 'Prairie', NA),
                Ecosystem = dplyr::if_else(total_density >= 0.5 & total_density <= 47, 'Savanna', Ecosystem),
                Ecosystem = dplyr::if_else(total_density > 47, 'Forest', Ecosystem)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ecosystem)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(values = c('#f8da1c', '#94bd46', '#1a5200'),
                             limits = c('Prairie', 'Savanna', 'Forest')) +
  ggplot2::ggtitle('Historical ecosystem state') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/historical_ecosystem.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/historical_ecosystems.svg',
                height = 12, width = 12, units = 'cm')

#### 2. Fractional composition ####

# Load fractional composition 
fcomp <- ncdf4::nc_open('data/raw/gridded-composition/SetTreeComp_Level2_v1.0.nc')

# Extract each taxon's array
# Skipping Atlantic white cedar and chestnut because they're only in the eastern
# Part of the domain
fcomp_ash <- ncdf4::ncvar_get(fcomp, 'Ash')
fcomp_basswood <- ncdf4::ncvar_get(fcomp, 'Basswood')
fcomp_beech <- ncdf4::ncvar_get(fcomp, 'Beech')
fcomp_birch <- ncdf4::ncvar_get(fcomp, 'Birch')
fcomp_cherry <- ncdf4::ncvar_get(fcomp, 'Cherry')
fcomp_dogwood <- ncdf4::ncvar_get(fcomp, 'Dogwood')
fcomp_elm <- ncdf4::ncvar_get(fcomp, 'Elm')
fcomp_fir <- ncdf4::ncvar_get(fcomp, 'Fir')
fcomp_hemlock <- ncdf4::ncvar_get(fcomp, 'Hemlock')
fcomp_hickory <- ncdf4::ncvar_get(fcomp, 'Hickory')
fcomp_ironwood <- ncdf4::ncvar_get(fcomp, 'Ironwood')
fcomp_maple <- ncdf4::ncvar_get(fcomp, 'Maple')
fcomp_oak <- ncdf4::ncvar_get(fcomp, 'Oak')
fcomp_pine <- ncdf4::ncvar_get(fcomp, 'Pine')
fcomp_spruce <- ncdf4::ncvar_get(fcomp, 'Spruce')
fcomp_tamarack <- ncdf4::ncvar_get(fcomp, 'Tamarack')
fcomp_walnut <- ncdf4::ncvar_get(fcomp, 'Walnut')
fcomp_oh <- ncdf4::ncvar_get(fcomp, 'Other hardwood')
fcomp_gum <- ncdf4::ncvar_get(fcomp, 'Black gum/sweet gum')
fcomp_cedar <- ncdf4::ncvar_get(fcomp, 'Cedar/juniper')
fcomp_poplar <- ncdf4::ncvar_get(fcomp, 'Poplar/tulip poplar')

# Extract dimensions
fcomp_x <- ncdf4::ncvar_get(fcomp, 'x')
fcomp_y <- ncdf4::ncvar_get(fcomp, 'y')
fcomp_sample <- ncdf4::ncvar_get(fcomp, 'sample')

# Apply dimension names
dimnames(fcomp_ash) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_basswood) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_beech) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_birch) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_cherry) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_dogwood) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_elm) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_fir) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_hemlock) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_hickory) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_ironwood) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_maple) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_oak) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_pine) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_spruce) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_tamarack) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_walnut) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_oh) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_gum) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_cedar) <- list(fcomp_x, fcomp_y, fcomp_sample)
dimnames(fcomp_poplar) <- list(fcomp_x, fcomp_y, fcomp_sample)

# Average over posterior samples
fcomp_ash_mean <- apply(fcomp_ash, 1:2, mean)
fcomp_basswood_mean <- apply(fcomp_basswood, 1:2, mean)
fcomp_beech_mean <- apply(fcomp_beech, 1:2, mean)
fcomp_birch_mean <- apply(fcomp_birch, 1:2, mean)
fcomp_cherry_mean <- apply(fcomp_cherry, 1:2, mean)
fcomp_dogwood_mean <- apply(fcomp_dogwood, 1:2, mean)
fcomp_elm_mean <- apply(fcomp_elm, 1:2, mean)
fcomp_fir_mean <- apply(fcomp_fir, 1:2, mean)
fcomp_hemlock_mean <- apply(fcomp_hemlock, 1:2, mean)
fcomp_hickory_mean <- apply(fcomp_hickory, 1:2, mean)
fcomp_ironwood_mean <- apply(fcomp_ironwood, 1:2, mean)
fcomp_maple_mean <- apply(fcomp_maple, 1:2, mean)
fcomp_oak_mean <- apply(fcomp_oak, 1:2, mean)
fcomp_pine_mean <- apply(fcomp_pine, 1:2, mean)
fcomp_spruce_mean <- apply(fcomp_spruce, 1:2, mean)
fcomp_tamarack_mean <- apply(fcomp_tamarack, 1:2, mean)
fcomp_walnut_mean <- apply(fcomp_walnut, 1:2, mean)
fcomp_oh_mean <- apply(fcomp_oh, 1:2, mean)
fcomp_gum_mean <- apply(fcomp_gum, 1:2, mean)
fcomp_cedar_mean <- apply(fcomp_cedar, 1:2, mean)
fcomp_poplar_mean <- apply(fcomp_poplar, 1:2, mean)

# Combining fcomp into one array
fcomp_mean <- array(, dim = c(296, 180, 21))

# Adding taxa into one array
fcomp_mean[,,1] <- fcomp_ash_mean
fcomp_mean[,,2] <- fcomp_basswood_mean
fcomp_mean[,,3] <- fcomp_beech_mean
fcomp_mean[,,4] <- fcomp_birch_mean
fcomp_mean[,,5] <- fcomp_cherry_mean
fcomp_mean[,,6] <- fcomp_dogwood_mean
fcomp_mean[,,7] <- fcomp_elm_mean
fcomp_mean[,,8] <- fcomp_fir_mean
fcomp_mean[,,9] <- fcomp_hemlock_mean
fcomp_mean[,,10] <- fcomp_hickory_mean
fcomp_mean[,,11] <- fcomp_ironwood_mean
fcomp_mean[,,12] <- fcomp_maple_mean
fcomp_mean[,,13] <- fcomp_oak_mean
fcomp_mean[,,14] <- fcomp_pine_mean
fcomp_mean[,,15] <- fcomp_spruce_mean
fcomp_mean[,,16] <- fcomp_tamarack_mean
fcomp_mean[,,17] <- fcomp_walnut_mean
fcomp_mean[,,18] <- fcomp_oh_mean
fcomp_mean[,,19] <- fcomp_gum_mean
fcomp_mean[,,20] <- fcomp_cedar_mean
fcomp_mean[,,21] <- fcomp_poplar_mean

# Add dimension names
dimnames(fcomp_mean)[[1]] <- dimnames(fcomp_ash_mean)[[1]]
dimnames(fcomp_mean)[[2]] <- dimnames(fcomp_ash_mean)[[2]]
dimnames(fcomp_mean)[[3]] <- c('Ash', 'Basswood', 'Beech', 'Birch',
                               'Cherry', 'Dogwood', 'Elm', 'Fir',
                               'Hemlock', 'Hickory', 'Ironwood', 'Maple',
                               'Oak', 'Pine', 'Spruce', 'Tamarack',
                               'Walnut', 'Other hardwood', 'Black gum/sweet gum',
                               'Cedar/juniper', 'Poplar/tulip poplar')

# Melt to dataframe
fcomp_df <- reshape2::melt(fcomp_mean)

# Add column names
colnames(fcomp_df) <- c('x', 'y', 'taxon', 'fcomp')

# Plot
fcomp_df |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fcomp)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'D', direction = -1,
                                limits = c(0, 1),
                                name = 'Fraction total\nstems',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::ggtitle('Relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### 3. Combine ####

# Pivot fcomp wider
fcomp_df <- tidyr::pivot_wider(fcomp_df, names_from = 'taxon', values_from = 'fcomp')

# Combine fcomp and total density
density_fcomp_df <- total_density_df |>
  dplyr::full_join(y = fcomp_df,
                   by = c('x', 'y'))

# Limit fcomp to domain of total density
## There are extra states in the fcomp data product. This keeps only the
## states in the UMW plus islands: minnesota, wisconsin,
## michigan, illinois, indiana
density_fcomp_df <- density_fcomp_df |>
  dplyr::mutate(Ash = dplyr::if_else(is.na(total_density), NA, Ash),
                Basswood = dplyr::if_else(is.na(total_density), NA, Basswood),
                Beech = dplyr::if_else(is.na(total_density), NA, Beech),
                Birch = dplyr::if_else(is.na(total_density), NA, Birch),
                Cherry = dplyr::if_else(is.na(total_density), NA, Cherry),
                Dogwood = dplyr::if_else(is.na(total_density), NA, Dogwood),
                Elm = dplyr::if_else(is.na(total_density), NA, Elm),
                Fir = dplyr::if_else(is.na(total_density), NA, Fir),
                Hemlock = dplyr::if_else(is.na(total_density), NA, Hemlock),
                Hickory = dplyr::if_else(is.na(total_density), NA, Hickory),
                Ironwood = dplyr::if_else(is.na(total_density), NA, Ironwood),
                Maple = dplyr::if_else(is.na(total_density), NA, Maple),
                Oak = dplyr::if_else(is.na(total_density), NA, Oak),
                Pine = dplyr::if_else(is.na(total_density), NA, Pine),
                Spruce = dplyr::if_else(is.na(total_density), NA, Spruce),
                Tamarack = dplyr::if_else(is.na(total_density), NA, Tamarack),
                Walnut = dplyr::if_else(is.na(total_density), NA, Walnut),
                `Other hardwood` = dplyr::if_else(is.na(total_density), NA, `Other hardwood`),
                `Black gum/sweet gum` = dplyr::if_else(is.na(total_density), NA, `Black gum/sweet gum`),
                `Cedar/juniper` = dplyr::if_else(is.na(total_density), NA, `Cedar/juniper`),
                `Poplar/tulip poplar` = dplyr::if_else(is.na(total_density), NA, `Poplar/tulip poplar`))

# Plot total density
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

# Filter out all the NAs
density_fcomp_df <- tidyr::drop_na(density_fcomp_df)

# Plot total density again to make sure nothing changed from dropping NAs
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
                                limits = c(0, 1),
                                name = 'Fraction total\nstems',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot oak relative abundance
density_fcomp_df |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(limits = c(0, 1),
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                palette = 'Blues',
                                direction = 1) +
  ggplot2::ggtitle('Historical oak relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/historical_oak_abundance.svg',
                height = 12, width = 12, units = 'cm')

#### 4. Save ####

save(density_fcomp_df,
     file = 'data/intermediate/PLS/gridded_fcomp_density.RData')
