#### STEP 3-1

## Mapping southwestern Wisconsin at 30 m resolution
## Same as step 3.1.Plot_LC_1985.R but only for a portion of
## Wisconsin and with no aggregation

## NOTE that the NLCD database is too large to store locally
## I saved on an external hard drive
## File paths should be changed according to your local file structure

## 1. Maps of study region
## 2. Load and format NLCD raster
## 3. Plot Wisconsin cutout
## 4. Plot Wisconsin subset at 30 m resolution

## Input: Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_1985_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff
## NLCD raster for Wisconsin
## Downloaded from https://www.mrlc.gov/viewer/

## Output: data/processed/nlcd/nlcd_wisconsin_lc_1985.RData
## Processed NLCD data at 30 m resolution for southwestern Wisconsin
## Not used again, only saved so that figures can be reproduced quickly

rm(list = ls())

#### 1. Maps of study region ####

# Outline of state
wisconsin <- sf::st_as_sf(maps::map(database = 'state', 
                                    region = 'wisconsin',
                                    plot = FALSE, fill = TRUE))

# Change projection
wisconsin <- sf::st_transform(wisconsin, crs = 'EPSG:3175')

#### 2. Load and format NLCD raster ####

# Load and format raster of Wisconsin
rast4 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_1985_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff')
rast4b <- terra::rast(rast4)
rast4c <- terra::as.factor(rast4b)

# Reproject to EPSG:3175
rast4d <- terra::project(rast4c, 'EPSG:3175')

# Create bounding box for region of interest
wi_box <- terra::ext(c(-91.020484, -89.610972,
                       43.034439, 44.590284))
# Reproject to EPSG:3175
wi_box <- terra::project(wi_box, 'EPSG:3175', from = 'EPSG:4326')

# Crop to extent of box
rast4e <- terra::crop(x = rast4d, y = wi_box)

# Convert to dataframe
wi_lc_1985 <- terra::as.data.frame(rast4e, xy = TRUE)
# Change column names
colnames(wi_lc_1985) <- c('x', 'y', 'ID')

# Color palette associatd with NLCD data
legend <- FedData::pal_nlcd()
legend <- legend |>
  # Take only land cover types in our dataset
  dplyr::filter(ID %in% wi_lc_1985$ID) |>
  # Convert ID to factor to match NLCD data
  dplyr::mutate(ID = as.factor(ID)) |>
  # Make labels sentence case
  dplyr::mutate(Class = stringr::str_to_sentence(Class))

# Add actual land cover class to NLCD data
# instead of just the ID number
wi_lc_1985 <- dplyr::left_join(x = wi_lc_1985,
                               y = dplyr::select(legend, ID, Class),
                               by = 'ID')

# Save
save(wi_lc_1985,
     file = 'data/processed/nlcd/nlcd_wisconsin_lc_1985.RData')

#### 3. Plot Wisconsin cutout ####

# Plot Wisconsin with bounding box to show area of interest
ggplot2::ggplot() +
  ggplot2::geom_sf(data = wisconsin, color = NA, fill = 'grey85') +
  ggplot2::geom_rect(ggplot2::aes(xmin = 368116.503817479,
                                  xmax = 496135.608409622,
                                  ymin = 738452.190059921,
                                  ymax = 920808.745062025),
                     color = 'red', fill = 'red', alpha = 0.5) +
  ggplot2::geom_sf(data = wisconsin, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/wi_subset.png',
                height = 5, width = 5, units = 'cm')

#### 4. Plot Wisconsin subset at 30 m resolution ####

# Plot all land cover classes
wi_lc_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Class)) +
  ggplot2::scale_fill_manual(limits = legend$Class,
                             values = legend$Color) +
  ggplot2::coord_sf(xlim = c(368116.503817479, 496135.608409622),
                    ylim = c(738452.190059921, 920808.745062025),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Land cover, 1985') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 6))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/wisconsin_all_cover_1985.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/wisconsin_all_cover_1985.svg',
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
wi_lc_1985 |>
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
  ggplot2::coord_sf(xlim = c(368116.503817479, 496135.608409622),
                    ylim = c(738452.190059921, 920808.745062025),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Land cover, 1985') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/wisconsin_red_cover_1985.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/wisconsin_red_cover_1985.svg',
                height = 10, width = 10, units = 'cm')
