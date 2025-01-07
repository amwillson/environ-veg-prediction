#### STEP 3-3

## Mapping southwestern Wisconsin at 30 m resolution
## Same as step 3.3.Plot_TCC.R but only for a portion of
## Wisconsin and with no aggregation

## NOTE that the NLCD database is too large to store locally
## I saved on an external hard drive
## File paths should be changed according to your local file structure

## 1. Maps of study region
## 2. Load and format NLCD raster
## 3. Plot Wisconsin subset at 30 m resolution

## Input: /Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/Annual_NLCD_LndCov_2021_CU_C1V0_eVPLkoltuvcg6gKKypSU.tiff
## NLCD tree cover raster for Wisconsin
## Downloaded from https://www.mrlc.gov/viewer/

## Output: data/processed/nlcd/nlcd_wisconsin_tcc_2021.RData
## Processed NLCD tree cover at 30 m resolution for southwestern Wisconsin
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
rast4 <- raster::raster('/Volumes/FileBackup/SDM_bigdata/NLCD_eVPLkoltuvcg6gKKypSU/nlcd_tcc_conus_2021_v2021-4_eVPLkoltuvcg6gKKypSU.tiff')
rast4b <- terra::rast(rast4)
# Change to integer
# for some reason it's reading as a factor
rast4c <- terra::as.int(rast4b)

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
wi_tcc_2021 <- terra::as.data.frame(rast4e, xy = TRUE)
# Change column names
colnames(wi_tcc_2021) <- c('x', 'y', 'cover')

# Save
save(wi_tcc_2021,
     file = 'data/processed/nlcd/nlcd_wisconsin_tcc_2021.RData')

#### 3. Plot Wisconsin subset at 30 m resolution ####

# Plot all land cover classes
wi_tcc_2021 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::coord_sf(xlim = c(368116.503817479, 496135.608409622),
                    ylim = c(738452.190059921, 920808.745062025),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Tree cover') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = 'figures/nlcd/wisconsin_tree_cover_2021.png',
                height = 10, width = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot() + ggplot2::theme(legend.position = 'none'),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/wisconsin_tree_cover_2021.svg',
                height = 10, width = 10, units = 'cm')

# Load land cover raster
load('data/processed/nlcd/nlcd_wisconsin_lc_2023.RData')

# Add open water locations to plot
# Shows location of rivers
wi_tcc_2021 |>
  # Filter for possible tree covers
  # Takes out any remnant cells with 254/255 values
  dplyr::filter(cover <= 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cover)) +
  ggplot2::geom_tile(data = dplyr::filter(wi_lc_2023, Class == 'Open water'),
                     ggplot2::aes(x = x, y = y), fill = '#5475A8') +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                name = '%',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::coord_sf(xlim = c(368116.503817479, 496135.608409622),
                    ylim = c(738452.190059921, 920808.745062025),
                    crs = 'EPSG:3175') +
  ggplot2::ggtitle('Tree cover') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/nlcd/wisconsin_tree_cover_rivers_2021.png',
                width = 10, height = 10, units = 'cm')
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = '/Volumes/FileBackup/SDM_bigdata/nlcd_svgs/wisconsin_tree_cover_rivers_2021.svg',
                width = 10, height = 10, units = 'cm')
