## Tree cover using NLCD option (downloaded from FIA database)
# Web page: https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/#table1

rm(list = ls())

# Load data from tiff
trees <- stars::read_stars('/Volumes/FileBackup/SDM_bigdata/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif')
plot(trees)

states <- sf::st_as_sf(maps::map('state',
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = sf::st_crs(trees))

states2 <- sfheaders::sf_to_df(states, fill = TRUE)

range(states2$x)
range(states2$y)

library(stars)
library(dplyr)

trees2 <- trees %>% filter(x >= -90859.82, x <= 1095521.03,
                           y >= 1572383, y <= 2930559)
trees2
plot(trees2)

# Load PLS data
load('data/processed/PLS/xydata.RData')

# Make into spatial object
pls_spat <- sf::st_as_sf(xydata, coords = c('x', 'y'), crs = 'EPSG:3175')
pls_spat <- stars::st_as_stars(pls_spat)
pls_spat <- sf::st_transform(pls_spat, crs = sf::st_crs(trees2))

# Aggregate NLCD to PLS grid
trees3 <- stars::st_warp(src = trees2,
                         dest = pls_spat)
