## Tree cover using NLCD option (downloaded from FIA database)
# Web page: https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/#table1

rm(list = ls())

# Load data from tiff
trees <- terra::rast('/Volumes/FileBackup/SDM_bigdata/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif')
#trees <- stars::read_stars('/Volumes/FileBackup/SDM_bigdata/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif')
#plot(trees)

states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 plot = FALSE, fill = TRUE))

states <- sf::st_transform(states, crs = sf::st_crs(trees))

states2 <- terra::rast(states)

trees2 <- terra::crop(x = trees,
                      y = states2)

terra::plot(trees2)
trees3 <- terra::aggregate(trees2, factor = 10)

trees_aggregated <- terra::as.data.frame(trees3)

