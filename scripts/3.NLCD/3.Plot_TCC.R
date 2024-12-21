## Mapping % tree cover from NLCD 2011

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
rast1 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_4huSzSwuyTP2jbX8It5J/nlcd_tcc_conus_2011_v2021-4_4huSzSwuyTP2jbX8It5J.tiff')
# Convert to SpatRaster (raster is deprecated in favor of terra)
rast1b <- terra::rast(rast1)
# Make a copy of raster to apply mask
rmask <- rast1b
# Make 254 (non-processing area) and 255 (background) NA in mask
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
# Apply mask to original raster
rast1c <- terra::mask(x = rast1b, mask = rmask)

# Repeat for second .tiff
rast2 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_9ZXc4bOuLO4vdXXqNbSO/nlcd_tcc_conus_2011_v2021-4_9ZXc4bOuLO4vdXXqNbSO.tiff')
rast2b <- terra::rast(rast2)
rmask <- rast2b
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
rast2c <- terra::mask(x = rast2b, mask = rmask)

# Repeat for third .tiff
rast3 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_b5GgTPUIDunbqFkYYgzP/nlcd_tcc_conus_2011_v2021-4_b5GgTPUIDunbqFkYYgzP.tiff')
rast3b <- terra::rast(rast3)
rmask <- rast3b
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
rast3c <- terra::mask(x = rast3b, mask = rmask)

# Repeat for fourth .tiff
rast4 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/nlcd_tcc_conus_2011_v2021-4_eVPLkoltuvcg6gKKypSU.tiff')
rast4b <- terra::rast(rast4)
rmask <- rast4b
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
rast4c <- terra::mask(x = rast4b, mask = rmask)

# Repeat for fifth .tiff
rast5 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/nlcd_tcc_conus_2011_v2021-4_PbV7RYKpbYgOXQRMcA2D.tiff')
rast5b <- terra::rast(rast5)
rmask <- rast5b
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
rast5c <- terra::mask(x = rast5b, mask = rmask)

# Repeat for sixth .tiff
rast6 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_To6BNjHHiRaLtuUjkReu/nlcd_tcc_conus_2011_v2021-4_To6BNjHHiRaLtuUjkReu.tiff')
rast6b <- terra::rast(rast6)
rmask <- rast6b
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
rast6c <- terra::mask(x = rast6b, mask = rmask)

# Repeat for seventh .tiff
rast7 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_v4ffXdyI2mi2ILtcwIQW/nlcd_tcc_conus_2011_v2021-4_v4ffXdyI2mi2ILtcwIQW.tiff')
rast7b <- terra::rast(rast7)
rmask <- rast7b
rmask[rmask == 254] <- NA
rmask[rmask == 255] <- NA
rast7c <- terra::mask(x = rast7b, mask = rmask)

#### 3. Combine and format ####

# Iteratively combine two rasters together until all have been combined
# fun = 'first' means to only use the land cover class from the
# first raster where there is overlap. This is fine because it
# is the exact same data, I just overlapped when clipping the data
# for download sometimes
comb1 <- terra::mosaic(x = rast1b, y = rast2b, fun = 'max')
comb2 <- terra::mosaic(x = comb1, y = rast3b, fun = 'max')
comb3 <- terra::mosaic(x = comb2, y = rast4b, fun = 'max')
comb4 <- terra::mosaic(x = comb3, y = rast5b, fun = 'max')
comb5 <- terra::mosaic(x = comb4, y = rast6b, fun = 'max')
comb6 <- terra::mosaic(x = comb5, y = rast7b, fun = 'max')

# Reproject to EPSG:3175 (all PalEON data in this projection)
comb6c <- terra::project(comb6, 'EPSG:3175')
# Reduce the resolution of the data (fewer grid cells)
# by a factor of 10: 300 x 300 m resolution
# fun = mean means using mean to scale up
comb6d <- terra::aggregate(comb6c, fact = 10,
                           fun = 'median')
# Convert to dataframe
comb6e <- terra::as.data.frame(comb6d, xy = TRUE)
# Change column names
colnames(comb6e) <- c('x', 'y', 'cover')
# Convert to simple features
comb6f <- sf::st_as_sf(comb6e, coords = c('x', 'y'),
                       crs = 'EPSG:3175')
# Remove any grid cells not occurring within outline of states
comb6g <- sf::st_intersection(comb6f, states)
# Convert back to dataframe
tcc_2011 <- sfheaders::sf_to_df(comb6g, fill = TRUE)

# Save
save(tcc_2011,
     file = 'data/processed/nlcd/nlcd_tcc_2011.RData')

#### 4. Plot ####

# Plot all land cover classes
tcc_2011 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::ggtitle('Tree cover') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/tree_cover_2011.png',
                height = 15, width = 16, units = 'cm')

# River database
# From https://www.hydrosheds.org/products/hydrorivers
rivers_sf <- sf::st_read(file.path("data/raw/HydroRIVERS_v10_na.gdb", "HydroRIVERS_v10_na.gdb"))

# Transform CRS
rivers_sf <- sf::st_transform(rivers_sf, crs = 'EPSG:3175')

# Subset for region of interest
rivers_cropped <- sf::st_intersection(rivers_sf, states)

# Take only rivers of order 1-3 (larger rivers)
rr <- dplyr::filter(rivers_cropped, ORD_CLAS %in% 1:3)

# Plot all land cover classes with rivers
tcc_2011 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_sf(data = rr, color = 'blue', alpha = 0.5,
                   size = 1) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::ggtitle('Tree cover, 2011') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/tree_cover_rivers_2011.png',
                height = 15, width = 16, units = 'cm')

# Take only rivers of order 1-4 (some smaller rivers)
rr <- dplyr::filter(rivers_cropped, ORD_CLAS %in% 1:4)

# Plot all land cover classes with rivers
tcc_2011 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_sf(data = rr, color = 'blue', alpha = 0.5,
                   size = 1) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::ggtitle('Tree cover, 2011') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/tree_cover_rivers4_2011.png',
                height = 15, width = 16, units = 'cm')
