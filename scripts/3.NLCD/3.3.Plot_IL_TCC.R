#### STEP 3-3

## Mapping southwestern Illinois at 30 m resolution
## Same as step 3.3.Plot_TCC.R but only for a portion of
## Illinois and with no aggregation

## NOTE that the NLCD database is too large to store locally
## I saved on an external hard drive
## File paths should be changed according to your local file structure

## 1. Maps of study region
## 2. Load and format NLCD raster
## 3. Plot Illinois subset at 30 m resolution

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_2021_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff
## NLCD tree cover raster for Illinois
## Downloaded from https://www.mrlc.gov/viewer/

## Output: data/processed/nlcd/nlcd_illinois_tcc_2021.RData
## Processed NLCD tree cover at 30 m resolution for southwestern Illinois
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

# Load and format raster of illinois
rast5 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_PbV7RYKpbYgOXQRMcA2D/nlcd_tcc_conus_2021_v2021-4_PbV7RYKpbYgOXQRMcA2D.tiff')
rast5b <- terra::rast(rast5)
# Change to integer
# for some reason it's reading as a factor
rast5c <- terra::as.int(rast5b)

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
il_tcc_2021 <- terra::as.data.frame(rast5e, xy = TRUE)
# Change column names
colnames(il_tcc_2021) <- c('x', 'y', 'cover')

# Save
save(il_tcc_2021,
     file = 'data/processed/nlcd/nlcd_illinois_tcc_2021.RData')

#### 3. Plot Illinois subset at 30 m resolution ####

# Plot all land cover classes
il_tcc_2021 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::coord_sf(xlim = c(351898.018970391, 515598.789959032),
                    ylim = c(339134.739332346, 562302.686714322),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Tree cover') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/illinois_tree_cover_2021.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/illinois_tree_cover_2021.svg',
                height = 10, width = 10, units = 'cm')

# Load land cover raster
load('data/processed/nlcd/nlcd_illinois_lc_2023.RData')

# Add open water locations to plot
# Shows location of rivers
il_tcc_2021 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_tile(data = dplyr::filter(il_lc_2023, Class == 'Open water'),
                     ggplot2::aes(x = x, y = y), fill = '#5475A8') +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::coord_sf(xlim = c(351898.018970391, 515598.789959032),
                    ylim = c(339134.739332346, 562302.686714322),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Tree cover') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/illinois_tree_cover_rivers_2021.png',
                width = 10, height = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/illinois_tree_cover_rivers_2021.svg',
                width = 10, height = 10, units = 'cm')
