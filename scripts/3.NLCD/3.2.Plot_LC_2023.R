#### STEP 3-3

## Mapping land cover class from NLCD 2023
## Choosing 2023 simply because it's the latest year
## of the time series

## NOTE that the NLCD database is too large to store locally
## I saved on an external hard drive
## File paths should be changed according to your local file structure

## 1. Map of study region
## 2. Load NLCD rasters
## 3. Combine and format
## 4. Plot 300 m resolution

## Input: data/raw/HydroRIVERS_v10_na.gdb/HydroRIVERS_v10_na.gdb
## Geodatabase of river locations
## Downloaded from https://www.hydrosheds.org/products/hydrorivers

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_4huSzSwuyTP2jbX8It5J/Annual_NLCD_LndCov_2023_CU_C1V0_4huSzSwuyTP2jbX8It5J.tiff
## NLCD raster for Upper Michigan
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_9ZXc4bOuLO4vdXXqNbSO/Annual_NLCD_LndCov_2023_CU_C1V0_9ZXc4bOuLO4vdXXqNbSO.tiff
## NLCD raster for northern Minnesota
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_b5GgTPUIDunbqFkYYgzP/Annual_NLCD_LndCov_2023_CU_C1V0_b5GgTPUIDunbqFkYYgzP.tiff
## NLCD raster for Indiana
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_2023_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff
## NLCD raster for Wisconsin
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/Annual_NLCD_LndCov_2023_CU_C1V0_PbV7RYKpbYgOXQRMcA2D.tiff
## NLCD raster for Illinois
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_To6BNjHHiRaLtuUjkReu/Annual_NLCD_LndCov_2023_CU_C1V0_To6BNjHHiRaLtuUjkReu.tiff
## NLCD raster for Lower Michigan
## Downloaded from https://www.mrlc.gov/viewer/

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_v4ffXdyI2mi2ILtcwIQW/Annual_NLCD_LndCov_2023_CU_C1V0_v4ffXdyI2mi2ILtcwIQW.tiff
## NLCD raster for southern Minnesota
## Downloaded from https://www.mrlc.gov/viewer/

## Output: data/processed/nlcd/nlcd_lc_2023.RData
## Processed NLCD data at 300 m resolution for all 5 states
## Not used again, only saved so that figures can be reproduced quickly

rm(list = ls())

#### 1. Maps of study region ####

# Outline of states
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
rast1 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_4huSzSwuyTP2jbX8It5J/Annual_NLCD_LndCov_2023_CU_C1V0_4huSzSwuyTP2jbX8It5J.tiff')
# Convert to SpatRaster (raster is deprecated in favor of terra)
rast1b <- terra::rast(rast1)
# Specify that the land cover values should be read as factors
rast1c <- terra::as.factor(rast1b)

# Repeat for second .tiff
rast2 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_9ZXc4bOuLO4vdXXqNbSO/Annual_NLCD_LndCov_2023_CU_C1V0_9ZXc4bOuLO4vdXXqNbSO.tiff')
rast2b <- terra::rast(rast2)
rast2c <- terra::as.factor(rast2b)

# Repeat for third .tiff
rast3 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_b5GgTPUIDunbqFkYYgzP/Annual_NLCD_LndCov_2023_CU_C1V0_b5GgTPUIDunbqFkYYgzP.tiff')
rast3b <- terra::rast(rast3)
rast3c <- terra::as.factor(rast3b)

# Repeat for fourth .tiff
rast4 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_2023_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff')
rast4b <- terra::rast(rast4)
rast4c <- terra::as.factor(rast4b)

# Repeat for fifth .tiff
rast5 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/Annual_NLCD_LndCov_2023_CU_C1V0_PbV7RYKpbYgOXQRMcA2D.tiff')
rast5b <- terra::rast(rast5)
rast5c <- terra::as.factor(rast5b)

# Repeat for sixth .tiff
rast6 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_To6BNjHHiRaLtuUjkReu/Annual_NLCD_LndCov_2023_CU_C1V0_To6BNjHHiRaLtuUjkReu.tiff')
rast6b <- terra::rast(rast6)
rast6c <- terra::as.factor(rast6b)

# Repeat for seventh .tiff
rast7 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_v4ffXdyI2mi2ILtcwIQW/Annual_NLCD_LndCov_2023_CU_C1V0_v4ffXdyI2mi2ILtcwIQW.tiff')
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
lc_2023 <- sfheaders::sf_to_df(comb6h, fill = TRUE)

# Color palette associatd with NLCD data
legend <- FedData::pal_nlcd()
legend <- legend |>
  # Take only land cover types in our dataset
  dplyr::filter(ID %in% lc_2023$ID) |>
  # Convert ID to factor to match NLCD data
  dplyr::mutate(ID = as.factor(ID)) |>
  # Make labels sentence case
  dplyr::mutate(Class = stringr::str_to_sentence(Class))

# Add actual land cover class to NLCD data
# instead of just the ID number
lc_2023 <- dplyr::left_join(x = lc_2023,
                            y = dplyr::select(legend, ID, Class),
                            by = 'ID')

# Save
save(lc_2023,
     file = 'data/processed/nlcd/nlcd_lc_2023.RData')

#### 4. Plot 300 m resolution #### 

# Plot all land cover classes
p1 <- lc_2023 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend$Class,
                             values = legend$Color) +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))
p1

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/all_cover_2023.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/all_cover_2023.svg',
                height = 10, width = 10, units = 'cm')

# Take only rivers of order 1-3
rr <- dplyr::filter(rivers_cropped, ORD_CLAS %in% 1:3)

# Add rivers into above plot
lc_2023 |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = rr, color = 'blue', size = 0.05) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend$Class,
                             values = legend$Color) +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))

# Save (saving at larger size because it will be supplementary)
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/all_cover_rivers_2023.png',
                height = 8.26, width = 7.65, units = 'in')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/all_cover_rivers_2023.svg',
                height = 8.26, width = 7.65, units = 'in')

# Legend of only classes of interest
legend_sub <- dplyr::filter(legend,
                            Class %in% c('Deciduous forest',
                                         'Evergreen forest',
                                         'Mixed forest',
                                         'Shrub/scrub',
                                         'Grassland/herbaceous',
                                         'Woody wetlands'))

# Plot only land cover classes corresponding to our ecosystems of interest
lc_2023 |>
  dplyr::filter(Class %in% c('Deciduous forest',
                             'Evergreen forest',
                             'Mixed forest',
                             'Shrub/scrub',
                             'Grassland/herbaceous',
                             'Woody wetlands')) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend_sub$Class,
                             values = legend_sub$Color) +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/red_cover_2023.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/red_cover_2023.svg',
                height = 10, width = 10, units = 'cm')

# Add in rivers
lc_2023 |>
  dplyr::filter(Class %in% c('Deciduous forest',
                             'Evergreen forest',
                             'Mixed forest',
                             'Shrub/scrub',
                             'Grassland/herbaceous',
                             'Woody wetlands')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::geom_sf(data = rr, color = 'blue', size = 0.05) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend_sub$Class,
                             values = legend_sub$Color) +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/red_cover_rivers_2023.png')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/red_cover_rivers_2023.svg')

# Add smaller rivers (especially for Illinois)
rr <- dplyr::filter(rivers_cropped, ORD_CLAS %in% 1:4)

# Add in smaller rivers
lc_2023 |>
  dplyr::filter(Class %in% c('Deciduous forest',
                             'Evergreen forest',
                             'Mixed forest',
                             'Shrub/scrub',
                             'Grassland/herbaceous',
                             'Woody wetlands')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  
  ggplot2::geom_sf(data = rr, color = 'blue', size = 0.05) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_manual(limits = legend_sub$Class,
                             values = legend_sub$Color) +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/red_cover_rivers4_2023.png')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/red_cover_rivers4_2023.svg')
