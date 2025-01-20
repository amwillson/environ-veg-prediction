#### STEP 3-1

## Mapping central Illinois at 30 m resolution
## Same as step 3.1.Plot_LC_2023.R but only for a portion of
## Illinois and with no aggregation

## NOTE that the NLCD database is too large to store locally
## I saved on an external hard drive
## File paths should be changed according to your local file structure

## 1. Maps of study region
## 2. Load and format NLCD raster
## 3. Plot Wisconsin subset at 30 m resolution

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/Annual_NLCD_LndCov_2023_CU_C1V0_PbV7RYKpbYgOXQRMcA2D.tiff
## NLCD raster for Illinois
## Downloaded from https://www.mrlc.gov/viewer/

## Output: data/processed/nlcd/nlcd_illinois_lc_2023.RData
## Processed NLCD data at 30 m resolution for southwestern Illinois
## Not used again, only saved so that figures can be reproduced quickly

rm(list = ls())

#### 1. Maps of study region ####

# Outline of state
illinois <- sf::st_as_sf(maps::map(database = 'state', 
                                   region = 'illinois',
                                   plot = FALSE, fill = TRUE))

# Change projection
illinois <- sf::st_transform(illinois, crs = 'EPSG:3175')

#### 2. Load and format NLCD raster ####

# Load and format raster of Illinois
rast5 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/Annual_NLCD_LndCov_2023_CU_C1V0_PbV7RYKpbYgOXQRMcA2D.tiff')
rast5b <- terra::rast(rast5)
rast5c <- terra::as.factor(rast5b)

# Reproject to EPSG:3175
rast5d <- terra::project(rast5c, 'EPSG:3175')

# Create bounding box for region of interest
il_box <- terra::ext(c(-90.763550, -89.038696,
                       39.457403, 41.368564))
# Reproject to EPSG:3175
il_box <- terra::project(il_box, 'EPSG:3175', from = 'EPSG:4326')

# Crop to extent of box
rast5e <- terra::crop(x = rast5d, y = il_box)

# Convert to dataframe
il_lc_2023 <- terra::as.data.frame(rast5e, xy = TRUE)
# Change column names
colnames(il_lc_2023) <- c('x', 'y', 'ID')

# Color palette associatd with NLCD data
legend <- FedData::pal_nlcd()
legend <- legend |>
  # Take only land cover types in our dataset
  dplyr::filter(ID %in% il_lc_2023$ID) |>
  # Convert ID to factor to match NLCD data
  dplyr::mutate(ID = as.factor(ID)) |>
  # Make labels sentence case
  dplyr::mutate(Class = stringr::str_to_sentence(Class))

# Add actual land cover class to NLCD data
# instead of just the ID number
il_lc_2023 <- dplyr::left_join(x = il_lc_2023,
                               y = dplyr::select(legend, ID, Class),
                               by = 'ID')

# Save
save(il_lc_2023,
     file = 'data/processed/nlcd/nlcd_illinois_lc_2023.RData')

#### 3. Plot Wisconsin subset at 30 m resolution ####

# Plot all land cover classes
il_lc_2023 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::scale_fill_manual(limits = legend$Class,
                             values = legend$Color) +
  ggplot2::coord_sf(xlim = c(351898.018970391, 515598.789959032),
                    ylim = c(339134.739332346, 562302.686714322),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/illinois_all_cover_2023.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/illinois_all_cover_2023.svg',
                height = 10, width = 10, units = 'cm')

# Legend of only classes of interest
legend_sub <- dplyr::filter(legend,
                            Class %in% c('Deciduous forest',
                                         'Evergreen forest',
                                         'Mixed forest',
                                         'Shrub/scrub',
                                         'Grassland/herbaceous',
                                         'Woody wetlands',
                                         'Open water'))

# Plot only land cover classes corresponding to our ecosystems of interest
il_lc_2023 |>
  dplyr::filter(Class %in% c('Deciduous forest',
                             'Evergreen forest',
                             'Mixed forest',
                             'Shrub/scrub',
                             'Grassland/herbaceous',
                             'Woody wetlands',
                             'Open water')) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::scale_fill_manual(limits = legend_sub$Class,
                             values = legend_sub$Color) +
  ggplot2::coord_sf(xlim = c(351898.018970391, 515598.789959032),
                    ylim = c(339134.739332346, 562302.686714322),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Land cover, 2023') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/illinois_red_cover_2023.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/illinois_red_cover_2023.svg',
                height = 10, width = 10, units = 'cm')
