## Mapping land cover class from NLCD 1985

rm(list = ls())

#### 1. Map of study region ####

# Map of study region
states <- sf::st_as_sf(maps::map(database = 'state', 
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))

# Change projection
states <- sf::st_transform(states, crs = 'EPSG:3175')

#### 2. Load NLCD rasters ####

## Loading in land cover data in 7 sections because
## this was the maximum spatial extent I could download data at

# Load .tiff with raster package
rast1 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_4huSzSwuyTP2jbX8It5J/Annual_NLCD_LndCov_1985_CU_C1V0_4huSzSwuyTP2jbX8It5J.tiff')
# Convert to SpatRaster (raster is deprecated in favor of terra)
rast1b <- terra::rast(rast1)
# Specify that the land cover values should be read as factors
rast1c <- terra::as.factor(rast1b)

# Repeat for second .tiff
rast2 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_9ZXc4bOuLO4vdXXqNbSO/Annual_NLCD_LndCov_1985_CU_C1V0_9ZXc4bOuLO4vdXXqNbSO.tiff')
rast2b <- terra::rast(rast2)
rast2c <- terra::as.factor(rast2b)

# Repeat for third .tiff
rast3 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_b5GgTPUIDunbqFkYYgzP/Annual_NLCD_LndCov_1985_CU_C1V0_b5GgTPUIDunbqFkYYgzP.tiff')
rast3b <- terra::rast(rast3)
rast3c <- terra::as.factor(rast3b)

# Repeat for fourth .tiff
rast4 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_1985_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff')
rast4b <- terra::rast(rast4)
rast4c <- terra::as.factor(rast4b)

# Repeat for fifth .tiff
rast5 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/Annual_NLCD_LndCov_1985_CU_C1V0_PbV7RYKpbYgOXQRMcA2D.tiff')
rast5b <- terra::rast(rast5)
rast5c <- terra::as.factor(rast5b)

# Repeat for sixth .tiff
rast6 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_To6BNjHHiRaLtuUjkReu/Annual_NLCD_LndCov_1985_CU_C1V0_To6BNjHHiRaLtuUjkReu.tiff')
rast6b <- terra::rast(rast6)
rast6c <- terra::as.factor(rast6b)

# Repeat for seventh .tiff
rast7 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_v4ffXdyI2mi2ILtcwIQW/Annual_NLCD_LndCov_1985_CU_C1V0_v4ffXdyI2mi2ILtcwIQW.tiff')
rast7b <- terra::rast(rast7)
rast7c <- terra::as.factor(rast7b)

#### 3. Combine and format ####

# Iteratively combine two rasters together until all have been combined
# fun = 'first' means to only use the land cover class from the
# first raster where there is overlap. This is fine because it
# is the exact same data, I just overlapped when clipping the data
# for download sometimes
comb1 <- terra::mosaic(x = rast1c, y = rast2c, fun = 'first')
comb2 <- terra::mosaic(x = comb1, y = rast3c, fun = 'first')
comb3 <- terra::mosaic(x = comb2, y = rast4c, fun = 'first')
comb4 <- terra::mosaic(x = comb3, y = rast5c, fun = 'first')
comb5 <- terra::mosaic(x = comb4, y = rast6c, fun = 'first')
comb6 <- terra::mosaic(x = comb5, y = rast7c, fun = 'first')

# Reproject to EPSG:3175 (all PalEON data in this projection)
comb6d <- terra::project(comb6, 'EPSG:3175')
# Reduce the resolution of the data (fewer grid cells)
# by a factor of 10: 300 x 300 m resolution
# fun = modal means using mode to scale up
comb6e <- terra::aggregate(comb6d, fact = 10,
                           fun = 'modal')
# Convert to dataframe
comb6f <- terra::as.data.frame(comb6e, xy = TRUE)
# Change column names
colnames(comb6f) <- c('x', 'y', 'ID')
# Convert to simple features
comb6g <- sf::st_as_sf(comb6f, coords = c('x', 'y'),
                       crs = 'EPSG:3175')
# Remove any grid cells not occurring within outline of states
comb6h <- sf::st_intersection(comb6g, states)
# Convert back to dataframe
lc_1985 <- sfheaders::sf_to_df(comb6h, fill = TRUE)

# Save
save(lc_1985,
     file = 'data/processed/nlcd/nlcd_lc_1985.RData')

#### 4. Plot #### 

# Color palette associatd with NLCD data
legend <- FedData::pal_nlcd()
legend <- legend |>
  # Take only land cover types in our dataset
  dplyr::filter(ID %in% lc_1985$ID) |>
  # Convert ID to factor to match NLCD data
  dplyr::mutate(ID = as.factor(ID))

# Add actual land cover class to NLCD data
# instead of just the ID number
lc_1985 <- dplyr::left_join(x = lc_1985,
                            y = dplyr::select(legend, ID, Class),
                            by = 'ID')

# Plot all land cover classes
lc_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend$Class,
                             values = legend$Color) +
  ggplot2::ggtitle('Land cover, 1985') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/all_cover_1985.png',
                height = 15, width = 16, units = 'cm')

# Legend of only classes of interest
legend_sub <- dplyr::filter(legend,
                            Class %in% c('Deciduous Forest',
                                         'Evergreen Forest',
                                         'Mixed Forest',
                                         'Shrub/Scrub',
                                         'Grassland/Herbaceous',
                                         'Woody Wetlands'))

# Plot only land cover classes corresponding to our ecosystems of interest
lc_1985 |>
  dplyr::filter(Class %in% c('Deciduous Forest',
                             'Evergreen Forest',
                             'Mixed Forest',
                             'Shrub/Scrub',
                             'Grassland/Herbaceous',
                             'Woody Wetlands')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend_sub$Class,
                             values = legend_sub$Color) +
  ggplot2::ggtitle('Land cover, 1985') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/red_cover_1985.png',
                height = 15, width = 16, units = 'cm')

#### 5. Total area in higher resolution ####

# Reduce the resolution of the data (fewer grid cells)
# by a factor of 5: 150 x 150 m resolution
# fun = modal means using mode to scale up
comb6e <- terra::aggregate(comb6d, fact = 5,
                           fun = 'modal')
# Convert to dataframe
comb6f <- terra::as.data.frame(comb6e, xy = TRUE)
# Change column names
colnames(comb6f) <- c('x', 'y', 'ID')
# Convert to simple features
comb6g <- sf::st_as_sf(comb6f, coords = c('x', 'y'),
                       crs = 'EPSG:3175')
# Remove any grid cells not occurring within outline of states
comb6h <- sf::st_intersection(comb6g, states)
# Convert back to dataframe
lc_1985 <- sfheaders::sf_to_df(comb6h, fill = TRUE)

# Save
save(lc_1985,
     file = 'data/processed/nlcd/nlcd_lc_1985.RData')

#### 4. Plot higher resolution #### 

# Color palette associatd with NLCD data
legend <- FedData::pal_nlcd()
legend <- legend |>
  # Take only land cover types in our dataset
  dplyr::filter(ID %in% lc_1985$ID) |>
  # Convert ID to factor to match NLCD data
  dplyr::mutate(ID = as.factor(ID))

# Add actual land cover class to NLCD data
# instead of just the ID number
lc_1985 <- dplyr::left_join(x = lc_1985,
                            y = dplyr::select(legend, ID, Class),
                            by = 'ID')

# Plot all land cover classes
lc_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend$Class,
                             values = legend$Color) +
  ggplot2::ggtitle('Land cover, 1985') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/all_cover_1985.png',
                height = 15, width = 16, units = 'cm')

# Legend of only classes of interest
legend_sub <- dplyr::filter(legend,
                            Class %in% c('Deciduous Forest',
                                         'Evergreen Forest',
                                         'Mixed Forest',
                                         'Shrub/Scrub',
                                         'Grassland/Herbaceous',
                                         'Woody Wetlands'))

# Plot only land cover classes corresponding to our ecosystems of interest
lc_1985 |>
  dplyr::filter(Class %in% c('Deciduous Forest',
                             'Evergreen Forest',
                             'Mixed Forest',
                             'Shrub/Scrub',
                             'Grassland/Herbaceous',
                             'Woody Wetlands')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend_sub$Class,
                             values = legend_sub$Color) +
  ggplot2::ggtitle('Land cover, 1985') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/red_cover_1985.png',
                height = 15, width = 16, units = 'cm')
