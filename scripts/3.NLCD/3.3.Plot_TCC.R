#### STEP 3-3

## Mapping % tree cover from NLCD 2021
## Choosing 2021 because it is the most recent year available

## 1. Map of study region
## 2. Load NLCD rasters
## 3. Combine and format
## 4. Plot

## Input: data/raw/HydroRIVERS_v10_na.gdb/HydroRIVERS_v10_na.gdb
## Geodatabase of river locations
## Downloaded from https://www.hydrosheds.org/products/hydrorivers

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_4huSzSwuyTP2jbX8It5J/nlcd_tcc_conus_2021_v2021-4_4huSzSwuyTP2jbX8It5J.tiff
## NLCD tree cover raster for Upper Michigan
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_9ZXc4bOuLO4vdXXqNbSO/nlcd_tcc_conus_2021_v2021-4_9ZXc4bOuLO4vdXXqNbSO.tiff
## NLCD tree cover raster for northern Minnesota
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_b5GgTPUIDunbqFkYYgzP/nlcd_tcc_conus_2021_v2021-4_b5GgTPUIDunbqFkYYgzP.tiff
## NLCD tree cover raster for Indiana
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/nlcd_tcc_conus_2021_v2021-4_eVPLkoltuvcg6gKKypSU.tiff
## NLCD tree cover raster for Wisconsin
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/nlcd_tcc_conus_2021_v2021-4_PbV7RYKpbYgOXQRMcA2D.tiff
## NLCD tree cover raster for Illinois
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_To6BNjHHiRaLtuUjkReu/nlcd_tcc_conus_2021_v2021-4_To6BNjHHiRaLtuUjkReu.tiff
## NLCD tree cover raster for Lower Michigan
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_v4ffXdyI2mi2ILtcwIQW/nlcd_tcc_conus_2021_v2021-4_v4ffXdyI2mi2ILtcwIQW.tiff
## NLCD tree cover raster for southern Minnesota
## Downloaded from https://www.mrlc.gov/viewer/

## Output: data/processed/nlcd/nlcd_tcc_2021.RData
## Processed NLCD tree cover at 300 m resolution for all 5 states
## Not used again, only saved so that figures can be reproduced quickly

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

# River database
# From https://www.hydrosheds.org/products/hydrorivers
rivers_sf <- sf::st_read(file.path("data/raw/HydroRIVERS_v10_na.gdb", "HydroRIVERS_v10_na.gdb"))

# Transform CRS
rivers_sf <- sf::st_transform(rivers_sf, crs = 'EPSG:3175')

# Subset for region of interest
rivers_cropped <- sf::st_intersection(rivers_sf, states)

#### 2. Load NLCD rasters ####

## Loading in land cover data in 7 sections because
## this was the maximum spatial extent I could download data at

# Load .tiff with raster package
rast1 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_4huSzSwuyTP2jbX8It5J/nlcd_tcc_conus_2021_v2021-4_4huSzSwuyTP2jbX8It5J.tiff')
# Convert to SpatRaster (raster is deprecated in favor of terra)
rast1b <- terra::rast(rast1)

# Repeat for second .tiff
rast2 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_9ZXc4bOuLO4vdXXqNbSO/nlcd_tcc_conus_2021_v2021-4_9ZXc4bOuLO4vdXXqNbSO.tiff')
rast2b <- terra::rast(rast2)

# Repeat for third .tiff
rast3 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_b5GgTPUIDunbqFkYYgzP/nlcd_tcc_conus_2021_v2021-4_b5GgTPUIDunbqFkYYgzP.tiff')
rast3b <- terra::rast(rast3)

# Repeat for fourth .tiff
rast4 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/nlcd_tcc_conus_2021_v2021-4_eVPLkoltuvcg6gKKypSU.tiff')
rast4b <- terra::rast(rast4)

# Repeat for fifth .tiff
rast5 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/nlcd_tcc_conus_2021_v2021-4_PbV7RYKpbYgOXQRMcA2D.tiff')
rast5b <- terra::rast(rast5)

# Repeat for sixth .tiff
rast6 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_To6BNjHHiRaLtuUjkReu/nlcd_tcc_conus_2021_v2021-4_To6BNjHHiRaLtuUjkReu.tiff')
rast6b <- terra::rast(rast6)

# Repeat for seventh .tiff
rast7 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_v4ffXdyI2mi2ILtcwIQW/nlcd_tcc_conus_2021_v2021-4_v4ffXdyI2mi2ILtcwIQW.tiff')
rast7b <- terra::rast(rast7)

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
tcc_2021 <- sfheaders::sf_to_df(comb6g, fill = TRUE)

# Save
save(tcc_2021,
     file = 'data/processed/nlcd/nlcd_tcc_2021.RData')

#### 4. Plot ####

# Plot all land cover classes
tcc_2021 |>
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
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/tree_cover_2021.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/tree_cover_2021.svg',
                height = 10, width = 10, units = 'cm')

# Take only rivers of order 1-3 (larger rivers)
rr <- dplyr::filter(rivers_cropped, ORD_CLAS %in% 1:3)

# Plot all land cover classes with rivers
tcc_2021 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_sf(data = rr, color = 'blue', size = 0.05) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::ggtitle('Tree cover, 2021') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/tree_cover_rivers_2021.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/tree_cover_rivers_2021.svg',
                height = 10, width = 10, units = 'cm')

# Take only rivers of order 1-4 (some smaller rivers)
rr <- dplyr::filter(rivers_cropped, ORD_CLAS %in% 1:4)

# Plot all land cover classes with rivers
tcc_2021 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_sf(data = rr, color = 'blue', size = 0.05) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::ggtitle('Tree cover, 2021') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/tree_cover_rivers4_2021.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/tree_cover_rivers4_2021.png',
                height = 10, width = 10, units = 'cm')
