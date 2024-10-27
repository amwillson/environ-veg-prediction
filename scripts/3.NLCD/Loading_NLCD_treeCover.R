## Tree cover using NLCD option (downloaded from FIA database)
# Web page: https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/#table1

rm(list = ls())

# Load data from tiff
trees <- stars::read_stars('/Volumes/FileBackup/SDM_bigdata/nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif')

# Convert to simple features
trees2 <- sf::st_as_sf(trees)
