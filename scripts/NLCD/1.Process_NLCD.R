## Trimming NLCD data to midwest extent

rm(list = ls())

# Helper function for dividing the state into four quadrants
source('scripts/1.soils/define_bounds.R')

# Load in raster layers from downloads
# Weird CRS that we need to convert our bounding boxes to
nlcd_2001 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2001_land_cover_l48_20210604/nlcd_2001_land_cover_l48_20210604.img')
nlcd_2004 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2004_land_cover_l48_20210604/nlcd_2004_land_cover_l48_20210604.img')
nlcd_2006 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2006_land_cover_l48_20210604/nlcd_2006_land_cover_l48_20210604.img')
nlcd_2008 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2008_land_cover_l48_20210604/nlcd_2008_land_cover_l48_20210604.img')
nlcd_2011 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2011_land_cover_l48_20210604/nlcd_2011_land_cover_l48_20210604.img')
nlcd_2013 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2013_land_cover_l48_20210604/nlcd_2013_land_cover_l48_20210604.img')
nlcd_2016 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2016_land_cover_l48_20210604/nlcd_2016_land_cover_l48_20210604.img')
nlcd_2019 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img')
nlcd_2021 <- terra::rast('/Volumes/FileBackup/SDM_bigdata/NLCD/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img')

## Bounding boxes by states from the following link: 
## https://observablehq.com/@rdmurphy/u-s-state-bounding-boxes
## in WGS84 = EPSG:4326

## Illinois

# Split IL into four quadrants
IL <- define_quadrants(xmin = -91.514, xmax = -87.4947,
                       ymin = 36.9704, ymax = 42.5088)

# Extract min & max lat/long coords for each quadrant
IL1 <- IL[[1]]
IL2 <- IL[[2]]
IL3 <- IL[[3]]
IL4 <- IL[[4]]

# Define bounding boxes based on above coords
a_IL1 <- sf::st_bbox(
  c(xmin = IL1[1], xmax = IL1[2],
    ymin = IL1[3], ymax = IL1[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_IL2 <- sf::st_bbox(
  c(xmin = IL2[1], xmax = IL2[2],
    ymin = IL2[3], ymax = IL2[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_IL3 <- sf::st_bbox(
  c(xmin = IL3[1], xmax = IL3[2],
    ymin = IL3[3], ymax = IL3[4]),
  crs = sf::st_crs(4326)
)|>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_IL4 <- sf::st_bbox(
  c(xmin = IL4[1], xmax = IL4[2],
    ymin = IL4[3], ymax = IL4[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

## Indiana

IN <- define_quadrants(xmin = -88.0977, xmax = -84.7848,
                       ymin = 37.7719, ymax = 41.7605)

IN1 <- IN[[1]]
IN2 <- IN[[2]]
IN3 <- IN[[3]]
IN4 <- IN[[4]]

a_IN1 <- sf::st_bbox(
  c(xmin = IN1[1], xmax = IN1[2],
    ymin = IN1[3], ymax = IN1[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_IN2 <- sf::st_bbox(
  c(xmin = IN2[1], xmax = IN2[2],
    ymin = IN2[3], ymax = IN2[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_IN3 <- sf::st_bbox(
  c(xmin = IN3[1], xmax = IN3[2],
    ymin = IN3[3], ymax = IN3[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_IN4 <- sf::st_bbox(
  c(xmin = IN4[1], xmax = IN4[2],
    ymin = IN4[3], ymax = IN4[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

## Michigan

MI <- define_quadrants(xmin = -90.4164, xmax = -82.4159,
                       ymin = 41.6961, ymax = 48.1906)

MI1 <- MI[[1]]
MI2 <- MI[[2]]
MI3 <- MI[[3]]
MI4 <- MI[[4]]

a_MI1 <- sf::st_bbox(
  c(xmin = MI1[1], xmax = MI1[2],
    ymin = MI1[3], ymax = MI1[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_MI2 <- sf::st_bbox(
  c(xmin = MI2[1], xmax = MI2[2],
    ymin = MI2[3], ymax = MI2[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_MI3 <- sf::st_bbox(
  c(xmin = MI3[1], xmax = MI3[2],
    ymin = MI3[3], ymax = MI3[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_MI4 <- sf::st_bbox(
  c(xmin = MI4[1], xmax = MI4[2],
    ymin = MI4[3], ymax = MI4[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

## Minnesota
MN <- define_quadrants(xmin = -97.2387, xmax = -89.4904,
                       ymin = 43.4993, ymax = 49.3847)

MN1 <- MN[[1]]
MN2 <- MN[[2]]
MN3 <- MN[[3]]
MN4 <- MN[[4]]

a_MN1 <- sf::st_bbox(
  c(xmin = MN1[1], xmax = MN1[2],
    ymin = MN1[3], ymax = MN1[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_MN2 <- sf::st_bbox(
  c(xmin = MN2[1], xmax = MN2[2],
    ymin = MN2[3], ymax = MN2[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_MN3 <- sf::st_bbox(
  c(xmin = MN3[1], xmax = MN3[2],
    ymin = MN3[3], ymax = MN3[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_MN4 <- sf::st_bbox(
  c(xmin = MN4[1], xmax = MN4[2],
    ymin = MN4[3], ymax = MN4[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

## Wisconsin
WI <- define_quadrants(xmin = -92.8894, xmax = -86.8235,
                       ymin = 42.4916, ymax = 47.0772)

WI1 <- WI[[1]]
WI2 <- WI[[2]]
WI3 <- WI[[3]]
WI4 <- WI[[4]]

a_WI1 <- sf::st_bbox(
  c(xmin = WI1[1], xmax = WI1[2],
    ymin = WI1[3], ymax = WI1[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_WI2 <- sf::st_bbox(
  c(xmin = WI2[1], xmax = WI2[2],
    ymin = WI2[3], ymax = WI2[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_WI3 <- sf::st_bbox(
  c(xmin = WI3[1], xmax = WI3[2],
    ymin = WI3[3], ymax = WI3[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

a_WI4 <- sf::st_bbox(
  c(xmin = WI4[1], xmax = WI4[2],
    ymin = WI4[3], ymax = WI4[4]),
  crs = sf::st_crs(4326)
) |>
  sf::st_as_sfc() |>
  sf::st_transform(crs = sf::st_crs(nlcd_2001))

# Clip nlcd_2001
nlcd_2001_IL1 <- raster::crop(x = nlcd_2001, y = a_IL1)
nlcd_2001_IL2 <- raster::crop(x = nlcd_2001, y = a_IL2)
nlcd_2001_IL3 <- raster::crop(x = nlcd_2001, y = a_IL3)
nlcd_2001_IL4 <- raster::crop(x = nlcd_2001, y = a_IL4)
nlcd_2001_IN1 <- raster::crop(x = nlcd_2001, y = a_IN1)
nlcd_2001_IN2 <- raster::crop(x = nlcd_2001, y = a_IN2)
nlcd_2001_IN3 <- raster::crop(x = nlcd_2001, y = a_IN3)
nlcd_2001_IN4 <- raster::crop(x = nlcd_2001, y = a_IN4)
nlcd_2001_MI1 <- raster::crop(x = nlcd_2001, y = a_MI1)
nlcd_2001_MI2 <- raster::crop(x = nlcd_2001, y = a_MI2)
nlcd_2001_MI3 <- raster::crop(x = nlcd_2001, y = a_MI3)
nlcd_2001_MI4 <- raster::crop(x = nlcd_2001, y = a_MI4)
nlcd_2001_MN1 <- raster::crop(x = nlcd_2001, y = a_MN1)
nlcd_2001_MN2 <- raster::crop(x = nlcd_2001, y = a_MN2)
nlcd_2001_MN3 <- raster::crop(x = nlcd_2001, y = a_MN3)
nlcd_2001_MN4 <- raster::crop(x = nlcd_2001, y = a_MN4)
nlcd_2001_WI1 <- raster::crop(x = nlcd_2001, y = a_WI1)
nlcd_2001_WI2 <- raster::crop(x = nlcd_2001, y = a_WI2)
nlcd_2001_WI3 <- raster::crop(x = nlcd_2001, y = a_WI3)
nlcd_2001_WI4 <- raster::crop(x = nlcd_2001, y = a_WI4)

# Convert to data frame
nlcd_2001_IL1 <- raster::as.data.frame(nlcd_2001_IL1, xy = TRUE)
nlcd_2001_IL2 <- raster::as.data.frame(nlcd_2001_IL2, xy = TRUE)
nlcd_2001_IL3 <- raster::as.data.frame(nlcd_2001_IL3, xy = TRUE)
nlcd_2001_IL4 <- raster::as.data.frame(nlcd_2001_IL4, xy = TRUE)
nlcd_2001_IN1 <- raster::as.data.frame(nlcd_2001_IN1, xy = TRUE)
nlcd_2001_IN2 <- raster::as.data.frame(nlcd_2001_IN2, xy = TRUE)
nlcd_2001_IN3 <- raster::as.data.frame(nlcd_2001_IN3, xy = TRUE)
nlcd_2001_IN4 <- raster::as.data.frame(nlcd_2001_IN4, xy = TRUE)
nlcd_2001_MI1 <- raster::as.data.frame(nlcd_2001_MI1, xy = TRUE)
nlcd_2001_MI2 <- raster::as.data.frame(nlcd_2001_MI2, xy = TRUE)
nlcd_2001_MI3 <- raster::as.data.frame(nlcd_2001_MI3, xy = TRUE)
nlcd_2001_MI4 <- raster::as.data.frame(nlcd_2001_MI4, xy = TRUE)
nlcd_2001_MN1 <- raster::as.data.frame(nlcd_2001_MN1, xy = TRUE)
nlcd_2001_MN2 <- raster::as.data.frame(nlcd_2001_MN2, xy = TRUE)
nlcd_2001_MN3 <- raster::as.data.frame(nlcd_2001_MN3, xy = TRUE)
nlcd_2001_MN4 <- raster::as.data.frame(nlcd_2001_MN4, xy = TRUE)
nlcd_2001_WI1 <- raster::as.data.frame(nlcd_2001_WI1, xy = TRUE)
nlcd_2001_WI2 <- raster::as.data.frame(nlcd_2001_WI2, xy = TRUE)
nlcd_2001_WI3 <- raster::as.data.frame(nlcd_2001_WI3, xy = TRUE)
nlcd_2001_WI4 <- raster::as.data.frame(nlcd_2001_WI4, xy = TRUE)

# Save 2001 data for further processing on VM
save(nlcd_2001_IL1, nlcd_2001_IL2, nlcd_2001_IL3, nlcd_2001_IL4,
     nlcd_2001_IN1, nlcd_2001_IN2, nlcd_2001_IN3, nlcd_2001_IN4,
     nlcd_2001_MI1, nlcd_2001_MI2, nlcd_2001_MI3, nlcd_2001_MI4,
     nlcd_2001_MN1, nlcd_2001_MN2, nlcd_2001_MN3, nlcd_2001_MN4,
     nlcd_2001_WI1, nlcd_2001_WI2, nlcd_2001_WI3, nlcd_2001_WI4,
     file = 'data/raw/NLCD/nlcd_2001_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2001, nlcd_2001_IL1, nlcd_2001_IL2, nlcd_2001_IL3, nlcd_2001_IL4,
   nlcd_2001_IN1, nlcd_2001_IN2, nlcd_2001_IN3, nlcd_2001_IN4,
   nlcd_2001_MI1, nlcd_2001_MI2, nlcd_2001_MI3, nlcd_2001_MI4,
   nlcd_2001_MN1, nlcd_2001_MN2, nlcd_2001_MN3, nlcd_2001_MN4,
   nlcd_2001_WI1, nlcd_2001_WI2, nlcd_2001_WI3, nlcd_2001_WI4)

# Clip nlcd_2004
nlcd_2004_IL1 <- raster::crop(x = nlcd_2004, y = a_IL1)
nlcd_2004_IL2 <- raster::crop(x = nlcd_2004, y = a_IL2)
nlcd_2004_IL3 <- raster::crop(x = nlcd_2004, y = a_IL3)
nlcd_2004_IL4 <- raster::crop(x = nlcd_2004, y = a_IL4)
nlcd_2004_IN1 <- raster::crop(x = nlcd_2004, y = a_IN1)
nlcd_2004_IN2 <- raster::crop(x = nlcd_2004, y = a_IN2)
nlcd_2004_IN3 <- raster::crop(x = nlcd_2004, y = a_IN3)
nlcd_2004_IN4 <- raster::crop(x = nlcd_2004, y = a_IN4)
nlcd_2004_MI1 <- raster::crop(x = nlcd_2004, y = a_MI1)
nlcd_2004_MI2 <- raster::crop(x = nlcd_2004, y = a_MI2)
nlcd_2004_MI3 <- raster::crop(x = nlcd_2004, y = a_MI3)
nlcd_2004_MI4 <- raster::crop(x = nlcd_2004, y = a_MI4)
nlcd_2004_MN1 <- raster::crop(x = nlcd_2004, y = a_MN1)
nlcd_2004_MN2 <- raster::crop(x = nlcd_2004, y = a_MN2)
nlcd_2004_MN3 <- raster::crop(x = nlcd_2004, y = a_MN3)
nlcd_2004_MN4 <- raster::crop(x = nlcd_2004, y = a_MN4)
nlcd_2004_WI1 <- raster::crop(x = nlcd_2004, y = a_WI1)
nlcd_2004_WI2 <- raster::crop(x = nlcd_2004, y = a_WI2)
nlcd_2004_WI3 <- raster::crop(x = nlcd_2004, y = a_WI3)
nlcd_2004_WI4 <- raster::crop(x = nlcd_2004, y = a_WI4)

# Convert to data frame
nlcd_2004_IL1 <- raster::as.data.frame(nlcd_2004_IL1, xy = TRUE)
nlcd_2004_IL2 <- raster::as.data.frame(nlcd_2004_IL2, xy = TRUE)
nlcd_2004_IL3 <- raster::as.data.frame(nlcd_2004_IL3, xy = TRUE)
nlcd_2004_IL4 <- raster::as.data.frame(nlcd_2004_IL4, xy = TRUE)
nlcd_2004_IN1 <- raster::as.data.frame(nlcd_2004_IN1, xy = TRUE)
nlcd_2004_IN2 <- raster::as.data.frame(nlcd_2004_IN2, xy = TRUE)
nlcd_2004_IN3 <- raster::as.data.frame(nlcd_2004_IN3, xy = TRUE)
nlcd_2004_IN4 <- raster::as.data.frame(nlcd_2004_IN4, xy = TRUE)
nlcd_2004_MI1 <- raster::as.data.frame(nlcd_2004_MI1, xy = TRUE)
nlcd_2004_MI2 <- raster::as.data.frame(nlcd_2004_MI2, xy = TRUE)
nlcd_2004_MI3 <- raster::as.data.frame(nlcd_2004_MI3, xy = TRUE)
nlcd_2004_MI4 <- raster::as.data.frame(nlcd_2004_MI4, xy = TRUE)
nlcd_2004_MN1 <- raster::as.data.frame(nlcd_2004_MN1, xy = TRUE)
nlcd_2004_MN2 <- raster::as.data.frame(nlcd_2004_MN2, xy = TRUE)
nlcd_2004_MN3 <- raster::as.data.frame(nlcd_2004_MN3, xy = TRUE)
nlcd_2004_MN4 <- raster::as.data.frame(nlcd_2004_MN4, xy = TRUE)
nlcd_2004_WI1 <- raster::as.data.frame(nlcd_2004_WI1, xy = TRUE)
nlcd_2004_WI2 <- raster::as.data.frame(nlcd_2004_WI2, xy = TRUE)
nlcd_2004_WI3 <- raster::as.data.frame(nlcd_2004_WI3, xy = TRUE)
nlcd_2004_WI4 <- raster::as.data.frame(nlcd_2004_WI4, xy = TRUE)

# Save 2004 data for further processing on VM
save(nlcd_2004_IL1, nlcd_2004_IL2, nlcd_2004_IL3, nlcd_2004_IL4,
     nlcd_2004_IN1, nlcd_2004_IN2, nlcd_2004_IN3, nlcd_2004_IN4,
     nlcd_2004_MI1, nlcd_2004_MI2, nlcd_2004_MI3, nlcd_2004_MI4,
     nlcd_2004_MN1, nlcd_2004_MN2, nlcd_2004_MN3, nlcd_2004_MN4,
     nlcd_2004_WI1, nlcd_2004_WI2, nlcd_2004_WI3, nlcd_2004_WI4,
     file = 'data/raw/NLCD/nlcd_2004_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2004, nlcd_2004_IL1, nlcd_2004_IL2, nlcd_2004_IL3, nlcd_2004_IL4,
   nlcd_2004_IN1, nlcd_2004_IN2, nlcd_2004_IN3, nlcd_2004_IN4,
   nlcd_2004_MI1, nlcd_2004_MI2, nlcd_2004_MI3, nlcd_2004_MI4,
   nlcd_2004_MN1, nlcd_2004_MN2, nlcd_2004_MN3, nlcd_2004_MN4,
   nlcd_2004_WI1, nlcd_2004_WI2, nlcd_2004_WI3, nlcd_2004_WI4)

# Clip nlcd_2006
nlcd_2006_IL1 <- raster::crop(x = nlcd_2006, y = a_IL1)
nlcd_2006_IL2 <- raster::crop(x = nlcd_2006, y = a_IL2)
nlcd_2006_IL3 <- raster::crop(x = nlcd_2006, y = a_IL3)
nlcd_2006_IL4 <- raster::crop(x = nlcd_2006, y = a_IL4)
nlcd_2006_IN1 <- raster::crop(x = nlcd_2006, y = a_IN1)
nlcd_2006_IN2 <- raster::crop(x = nlcd_2006, y = a_IN2)
nlcd_2006_IN3 <- raster::crop(x = nlcd_2006, y = a_IN3)
nlcd_2006_IN4 <- raster::crop(x = nlcd_2006, y = a_IN4)
nlcd_2006_MI1 <- raster::crop(x = nlcd_2006, y = a_MI1)
nlcd_2006_MI2 <- raster::crop(x = nlcd_2006, y = a_MI2)
nlcd_2006_MI3 <- raster::crop(x = nlcd_2006, y = a_MI3)
nlcd_2006_MI4 <- raster::crop(x = nlcd_2006, y = a_MI4)
nlcd_2006_MN1 <- raster::crop(x = nlcd_2006, y = a_MN1)
nlcd_2006_MN2 <- raster::crop(x = nlcd_2006, y = a_MN2)
nlcd_2006_MN3 <- raster::crop(x = nlcd_2006, y = a_MN3)
nlcd_2006_MN4 <- raster::crop(x = nlcd_2006, y = a_MN4)
nlcd_2006_WI1 <- raster::crop(x = nlcd_2006, y = a_WI1)
nlcd_2006_WI2 <- raster::crop(x = nlcd_2006, y = a_WI2)
nlcd_2006_WI3 <- raster::crop(x = nlcd_2006, y = a_WI3)
nlcd_2006_WI4 <- raster::crop(x = nlcd_2006, y = a_WI4)

# Convert to data frame
nlcd_2006_IL1 <- raster::as.data.frame(nlcd_2006_IL1, xy = TRUE)
nlcd_2006_IL2 <- raster::as.data.frame(nlcd_2006_IL2, xy = TRUE)
nlcd_2006_IL3 <- raster::as.data.frame(nlcd_2006_IL3, xy = TRUE)
nlcd_2006_IL4 <- raster::as.data.frame(nlcd_2006_IL4, xy = TRUE)
nlcd_2006_IN1 <- raster::as.data.frame(nlcd_2006_IN1, xy = TRUE)
nlcd_2006_IN2 <- raster::as.data.frame(nlcd_2006_IN2, xy = TRUE)
nlcd_2006_IN3 <- raster::as.data.frame(nlcd_2006_IN3, xy = TRUE)
nlcd_2006_IN4 <- raster::as.data.frame(nlcd_2006_IN4, xy = TRUE)
nlcd_2006_MI1 <- raster::as.data.frame(nlcd_2006_MI1, xy = TRUE)
nlcd_2006_MI2 <- raster::as.data.frame(nlcd_2006_MI2, xy = TRUE)
nlcd_2006_MI3 <- raster::as.data.frame(nlcd_2006_MI3, xy = TRUE)
nlcd_2006_MI4 <- raster::as.data.frame(nlcd_2006_MI4, xy = TRUE)
nlcd_2006_MN1 <- raster::as.data.frame(nlcd_2006_MN1, xy = TRUE)
nlcd_2006_MN2 <- raster::as.data.frame(nlcd_2006_MN2, xy = TRUE)
nlcd_2006_MN3 <- raster::as.data.frame(nlcd_2006_MN3, xy = TRUE)
nlcd_2006_MN4 <- raster::as.data.frame(nlcd_2006_MN4, xy = TRUE)
nlcd_2006_WI1 <- raster::as.data.frame(nlcd_2006_WI1, xy = TRUE)
nlcd_2006_WI2 <- raster::as.data.frame(nlcd_2006_WI2, xy = TRUE)
nlcd_2006_WI3 <- raster::as.data.frame(nlcd_2006_WI3, xy = TRUE)
nlcd_2006_WI4 <- raster::as.data.frame(nlcd_2006_WI4, xy = TRUE)

# Save 2006 data for further processing on VM
save(nlcd_2006_IL1, nlcd_2006_IL2, nlcd_2006_IL3, nlcd_2006_IL4,
     nlcd_2006_IN1, nlcd_2006_IN2, nlcd_2006_IN3, nlcd_2006_IN4,
     nlcd_2006_MI1, nlcd_2006_MI2, nlcd_2006_MI3, nlcd_2006_MI4,
     nlcd_2006_MN1, nlcd_2006_MN2, nlcd_2006_MN3, nlcd_2006_MN4,
     nlcd_2006_WI1, nlcd_2006_WI2, nlcd_2006_WI3, nlcd_2006_WI4,
     file = 'data/raw/NLCD/nlcd_2006_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2006, nlcd_2006_IL1, nlcd_2006_IL2, nlcd_2006_IL3, nlcd_2006_IL4,
   nlcd_2006_IN1, nlcd_2006_IN2, nlcd_2006_IN3, nlcd_2006_IN4,
   nlcd_2006_MI1, nlcd_2006_MI2, nlcd_2006_MI3, nlcd_2006_MI4,
   nlcd_2006_MN1, nlcd_2006_MN2, nlcd_2006_MN3, nlcd_2006_MN4,
   nlcd_2006_WI1, nlcd_2006_WI2, nlcd_2006_WI3, nlcd_2006_WI4)

# Clip nlcd_2008
nlcd_2008_IL1 <- raster::crop(x = nlcd_2008, y = a_IL1)
nlcd_2008_IL2 <- raster::crop(x = nlcd_2008, y = a_IL2)
nlcd_2008_IL3 <- raster::crop(x = nlcd_2008, y = a_IL3)
nlcd_2008_IL4 <- raster::crop(x = nlcd_2008, y = a_IL4)
nlcd_2008_IN1 <- raster::crop(x = nlcd_2008, y = a_IN1)
nlcd_2008_IN2 <- raster::crop(x = nlcd_2008, y = a_IN2)
nlcd_2008_IN3 <- raster::crop(x = nlcd_2008, y = a_IN3)
nlcd_2008_IN4 <- raster::crop(x = nlcd_2008, y = a_IN4)
nlcd_2008_MI1 <- raster::crop(x = nlcd_2008, y = a_MI1)
nlcd_2008_MI2 <- raster::crop(x = nlcd_2008, y = a_MI2)
nlcd_2008_MI3 <- raster::crop(x = nlcd_2008, y = a_MI3)
nlcd_2008_MI4 <- raster::crop(x = nlcd_2008, y = a_MI4)
nlcd_2008_MN1 <- raster::crop(x = nlcd_2008, y = a_MN1)
nlcd_2008_MN2 <- raster::crop(x = nlcd_2008, y = a_MN2)
nlcd_2008_MN3 <- raster::crop(x = nlcd_2008, y = a_MN3)
nlcd_2008_MN4 <- raster::crop(x = nlcd_2008, y = a_MN4)
nlcd_2008_WI1 <- raster::crop(x = nlcd_2008, y = a_WI1)
nlcd_2008_WI2 <- raster::crop(x = nlcd_2008, y = a_WI2)
nlcd_2008_WI3 <- raster::crop(x = nlcd_2008, y = a_WI3)
nlcd_2008_WI4 <- raster::crop(x = nlcd_2008, y = a_WI4)

# Convert to data frame
nlcd_2008_IL1 <- raster::as.data.frame(nlcd_2008_IL1, xy = TRUE)
nlcd_2008_IL2 <- raster::as.data.frame(nlcd_2008_IL2, xy = TRUE)
nlcd_2008_IL3 <- raster::as.data.frame(nlcd_2008_IL3, xy = TRUE)
nlcd_2008_IL4 <- raster::as.data.frame(nlcd_2008_IL4, xy = TRUE)
nlcd_2008_IN1 <- raster::as.data.frame(nlcd_2008_IN1, xy = TRUE)
nlcd_2008_IN2 <- raster::as.data.frame(nlcd_2008_IN2, xy = TRUE)
nlcd_2008_IN3 <- raster::as.data.frame(nlcd_2008_IN3, xy = TRUE)
nlcd_2008_IN4 <- raster::as.data.frame(nlcd_2008_IN4, xy = TRUE)
nlcd_2008_MI1 <- raster::as.data.frame(nlcd_2008_MI1, xy = TRUE)
nlcd_2008_MI2 <- raster::as.data.frame(nlcd_2008_MI2, xy = TRUE)
nlcd_2008_MI3 <- raster::as.data.frame(nlcd_2008_MI3, xy = TRUE)
nlcd_2008_MI4 <- raster::as.data.frame(nlcd_2008_MI4, xy = TRUE)
nlcd_2008_MN1 <- raster::as.data.frame(nlcd_2008_MN1, xy = TRUE)
nlcd_2008_MN2 <- raster::as.data.frame(nlcd_2008_MN2, xy = TRUE)
nlcd_2008_MN3 <- raster::as.data.frame(nlcd_2008_MN3, xy = TRUE)
nlcd_2008_MN4 <- raster::as.data.frame(nlcd_2008_MN4, xy = TRUE)
nlcd_2008_WI1 <- raster::as.data.frame(nlcd_2008_WI1, xy = TRUE)
nlcd_2008_WI2 <- raster::as.data.frame(nlcd_2008_WI2, xy = TRUE)
nlcd_2008_WI3 <- raster::as.data.frame(nlcd_2008_WI3, xy = TRUE)
nlcd_2008_WI4 <- raster::as.data.frame(nlcd_2008_WI4, xy = TRUE)

# Save 2008 data for further processing on VM
save(nlcd_2008_IL1, nlcd_2008_IL2, nlcd_2008_IL3, nlcd_2008_IL4,
     nlcd_2008_IN1, nlcd_2008_IN2, nlcd_2008_IN3, nlcd_2008_IN4,
     nlcd_2008_MI1, nlcd_2008_MI2, nlcd_2008_MI3, nlcd_2008_MI4,
     nlcd_2008_MN1, nlcd_2008_MN2, nlcd_2008_MN3, nlcd_2008_MN4,
     nlcd_2008_WI1, nlcd_2008_WI2, nlcd_2008_WI3, nlcd_2008_WI4,
     file = 'data/raw/NLCD/nlcd_2008_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2008, nlcd_2008_IL1, nlcd_2008_IL2, nlcd_2008_IL3, nlcd_2008_IL4,
   nlcd_2008_IN1, nlcd_2008_IN2, nlcd_2008_IN3, nlcd_2008_IN4,
   nlcd_2008_MI1, nlcd_2008_MI2, nlcd_2008_MI3, nlcd_2008_MI4,
   nlcd_2008_MN1, nlcd_2008_MN2, nlcd_2008_MN3, nlcd_2008_MN4,
   nlcd_2008_WI1, nlcd_2008_WI2, nlcd_2008_WI3, nlcd_2008_WI4)

# Clip nlcd_2011
nlcd_2011_IL1 <- raster::crop(x = nlcd_2011, y = a_IL1)
nlcd_2011_IL2 <- raster::crop(x = nlcd_2011, y = a_IL2)
nlcd_2011_IL3 <- raster::crop(x = nlcd_2011, y = a_IL3)
nlcd_2011_IL4 <- raster::crop(x = nlcd_2011, y = a_IL4)
nlcd_2011_IN1 <- raster::crop(x = nlcd_2011, y = a_IN1)
nlcd_2011_IN2 <- raster::crop(x = nlcd_2011, y = a_IN2)
nlcd_2011_IN3 <- raster::crop(x = nlcd_2011, y = a_IN3)
nlcd_2011_IN4 <- raster::crop(x = nlcd_2011, y = a_IN4)
nlcd_2011_MI1 <- raster::crop(x = nlcd_2011, y = a_MI1)
nlcd_2011_MI2 <- raster::crop(x = nlcd_2011, y = a_MI2)
nlcd_2011_MI3 <- raster::crop(x = nlcd_2011, y = a_MI3)
nlcd_2011_MI4 <- raster::crop(x = nlcd_2011, y = a_MI4)
nlcd_2011_MN1 <- raster::crop(x = nlcd_2011, y = a_MN1)
nlcd_2011_MN2 <- raster::crop(x = nlcd_2011, y = a_MN2)
nlcd_2011_MN3 <- raster::crop(x = nlcd_2011, y = a_MN3)
nlcd_2011_MN4 <- raster::crop(x = nlcd_2011, y = a_MN4)
nlcd_2011_WI1 <- raster::crop(x = nlcd_2011, y = a_WI1)
nlcd_2011_WI2 <- raster::crop(x = nlcd_2011, y = a_WI2)
nlcd_2011_WI3 <- raster::crop(x = nlcd_2011, y = a_WI3)
nlcd_2011_WI4 <- raster::crop(x = nlcd_2011, y = a_WI4)

# Convert to data frame
nlcd_2011_IL1 <- raster::as.data.frame(nlcd_2011_IL1, xy = TRUE)
nlcd_2011_IL2 <- raster::as.data.frame(nlcd_2011_IL2, xy = TRUE)
nlcd_2011_IL3 <- raster::as.data.frame(nlcd_2011_IL3, xy = TRUE)
nlcd_2011_IL4 <- raster::as.data.frame(nlcd_2011_IL4, xy = TRUE)
nlcd_2011_IN1 <- raster::as.data.frame(nlcd_2011_IN1, xy = TRUE)
nlcd_2011_IN2 <- raster::as.data.frame(nlcd_2011_IN2, xy = TRUE)
nlcd_2011_IN3 <- raster::as.data.frame(nlcd_2011_IN3, xy = TRUE)
nlcd_2011_IN4 <- raster::as.data.frame(nlcd_2011_IN4, xy = TRUE)
nlcd_2011_MI1 <- raster::as.data.frame(nlcd_2011_MI1, xy = TRUE)
nlcd_2011_MI2 <- raster::as.data.frame(nlcd_2011_MI2, xy = TRUE)
nlcd_2011_MI3 <- raster::as.data.frame(nlcd_2011_MI3, xy = TRUE)
nlcd_2011_MI4 <- raster::as.data.frame(nlcd_2011_MI4, xy = TRUE)
nlcd_2011_MN1 <- raster::as.data.frame(nlcd_2011_MN1, xy = TRUE)
nlcd_2011_MN2 <- raster::as.data.frame(nlcd_2011_MN2, xy = TRUE)
nlcd_2011_MN3 <- raster::as.data.frame(nlcd_2011_MN3, xy = TRUE)
nlcd_2011_MN4 <- raster::as.data.frame(nlcd_2011_MN4, xy = TRUE)
nlcd_2011_WI1 <- raster::as.data.frame(nlcd_2011_WI1, xy = TRUE)
nlcd_2011_WI2 <- raster::as.data.frame(nlcd_2011_WI2, xy = TRUE)
nlcd_2011_WI3 <- raster::as.data.frame(nlcd_2011_WI3, xy = TRUE)
nlcd_2011_WI4 <- raster::as.data.frame(nlcd_2011_WI4, xy = TRUE)

# Save 2011 data for further processing on VM
save(nlcd_2011_IL1, nlcd_2011_IL2, nlcd_2011_IL3, nlcd_2011_IL4,
     nlcd_2011_IN1, nlcd_2011_IN2, nlcd_2011_IN3, nlcd_2011_IN4,
     nlcd_2011_MI1, nlcd_2011_MI2, nlcd_2011_MI3, nlcd_2011_MI4,
     nlcd_2011_MN1, nlcd_2011_MN2, nlcd_2011_MN3, nlcd_2011_MN4,
     nlcd_2011_WI1, nlcd_2011_WI2, nlcd_2011_WI3, nlcd_2011_WI4,
     file = 'data/raw/NLCD/nlcd_2011_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2011, nlcd_2011_IL1, nlcd_2011_IL2, nlcd_2011_IL3, nlcd_2011_IL4,
   nlcd_2011_IN1, nlcd_2011_IN2, nlcd_2011_IN3, nlcd_2011_IN4,
   nlcd_2011_MI1, nlcd_2011_MI2, nlcd_2011_MI3, nlcd_2011_MI4,
   nlcd_2011_MN1, nlcd_2011_MN2, nlcd_2011_MN3, nlcd_2011_MN4,
   nlcd_2011_WI1, nlcd_2011_WI2, nlcd_2011_WI3, nlcd_2011_WI4)

# Clip nlcd_2013
nlcd_2013_IL1 <- raster::crop(x = nlcd_2013, y = a_IL1)
nlcd_2013_IL2 <- raster::crop(x = nlcd_2013, y = a_IL2)
nlcd_2013_IL3 <- raster::crop(x = nlcd_2013, y = a_IL3)
nlcd_2013_IL4 <- raster::crop(x = nlcd_2013, y = a_IL4)
nlcd_2013_IN1 <- raster::crop(x = nlcd_2013, y = a_IN1)
nlcd_2013_IN2 <- raster::crop(x = nlcd_2013, y = a_IN2)
nlcd_2013_IN3 <- raster::crop(x = nlcd_2013, y = a_IN3)
nlcd_2013_IN4 <- raster::crop(x = nlcd_2013, y = a_IN4)
nlcd_2013_MI1 <- raster::crop(x = nlcd_2013, y = a_MI1)
nlcd_2013_MI2 <- raster::crop(x = nlcd_2013, y = a_MI2)
nlcd_2013_MI3 <- raster::crop(x = nlcd_2013, y = a_MI3)
nlcd_2013_MI4 <- raster::crop(x = nlcd_2013, y = a_MI4)
nlcd_2013_MN1 <- raster::crop(x = nlcd_2013, y = a_MN1)
nlcd_2013_MN2 <- raster::crop(x = nlcd_2013, y = a_MN2)
nlcd_2013_MN3 <- raster::crop(x = nlcd_2013, y = a_MN3)
nlcd_2013_MN4 <- raster::crop(x = nlcd_2013, y = a_MN4)
nlcd_2013_WI1 <- raster::crop(x = nlcd_2013, y = a_WI1)
nlcd_2013_WI2 <- raster::crop(x = nlcd_2013, y = a_WI2)
nlcd_2013_WI3 <- raster::crop(x = nlcd_2013, y = a_WI3)
nlcd_2013_WI4 <- raster::crop(x = nlcd_2013, y = a_WI4)

# Convert to data frame
nlcd_2013_IL1 <- raster::as.data.frame(nlcd_2013_IL1, xy = TRUE)
nlcd_2013_IL2 <- raster::as.data.frame(nlcd_2013_IL2, xy = TRUE)
nlcd_2013_IL3 <- raster::as.data.frame(nlcd_2013_IL3, xy = TRUE)
nlcd_2013_IL4 <- raster::as.data.frame(nlcd_2013_IL4, xy = TRUE)
nlcd_2013_IN1 <- raster::as.data.frame(nlcd_2013_IN1, xy = TRUE)
nlcd_2013_IN2 <- raster::as.data.frame(nlcd_2013_IN2, xy = TRUE)
nlcd_2013_IN3 <- raster::as.data.frame(nlcd_2013_IN3, xy = TRUE)
nlcd_2013_IN4 <- raster::as.data.frame(nlcd_2013_IN4, xy = TRUE)
nlcd_2013_MI1 <- raster::as.data.frame(nlcd_2013_MI1, xy = TRUE)
nlcd_2013_MI2 <- raster::as.data.frame(nlcd_2013_MI2, xy = TRUE)
nlcd_2013_MI3 <- raster::as.data.frame(nlcd_2013_MI3, xy = TRUE)
nlcd_2013_MI4 <- raster::as.data.frame(nlcd_2013_MI4, xy = TRUE)
nlcd_2013_MN1 <- raster::as.data.frame(nlcd_2013_MN1, xy = TRUE)
nlcd_2013_MN2 <- raster::as.data.frame(nlcd_2013_MN2, xy = TRUE)
nlcd_2013_MN3 <- raster::as.data.frame(nlcd_2013_MN3, xy = TRUE)
nlcd_2013_MN4 <- raster::as.data.frame(nlcd_2013_MN4, xy = TRUE)
nlcd_2013_WI1 <- raster::as.data.frame(nlcd_2013_WI1, xy = TRUE)
nlcd_2013_WI2 <- raster::as.data.frame(nlcd_2013_WI2, xy = TRUE)
nlcd_2013_WI3 <- raster::as.data.frame(nlcd_2013_WI3, xy = TRUE)
nlcd_2013_WI4 <- raster::as.data.frame(nlcd_2013_WI4, xy = TRUE)

# Save 2013 data for further processing on VM
save(nlcd_2013_IL1, nlcd_2013_IL2, nlcd_2013_IL3, nlcd_2013_IL4,
     nlcd_2013_IN1, nlcd_2013_IN2, nlcd_2013_IN3, nlcd_2013_IN4,
     nlcd_2013_MI1, nlcd_2013_MI2, nlcd_2013_MI3, nlcd_2013_MI4,
     nlcd_2013_MN1, nlcd_2013_MN2, nlcd_2013_MN3, nlcd_2013_MN4,
     nlcd_2013_WI1, nlcd_2013_WI2, nlcd_2013_WI3, nlcd_2013_WI4,
     file = 'data/raw/NLCD/nlcd_2013_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2013, nlcd_2013_IL1, nlcd_2013_IL2, nlcd_2013_IL3, nlcd_2013_IL4,
   nlcd_2013_IN1, nlcd_2013_IN2, nlcd_2013_IN3, nlcd_2013_IN4,
   nlcd_2013_MI1, nlcd_2013_MI2, nlcd_2013_MI3, nlcd_2013_MI4,
   nlcd_2013_MN1, nlcd_2013_MN2, nlcd_2013_MN3, nlcd_2013_MN4,
   nlcd_2013_WI1, nlcd_2013_WI2, nlcd_2013_WI3, nlcd_2013_WI4)

# Clip nlcd_2016
nlcd_2016_IL1 <- raster::crop(x = nlcd_2016, y = a_IL1)
nlcd_2016_IL2 <- raster::crop(x = nlcd_2016, y = a_IL2)
nlcd_2016_IL3 <- raster::crop(x = nlcd_2016, y = a_IL3)
nlcd_2016_IL4 <- raster::crop(x = nlcd_2016, y = a_IL4)
nlcd_2016_IN1 <- raster::crop(x = nlcd_2016, y = a_IN1)
nlcd_2016_IN2 <- raster::crop(x = nlcd_2016, y = a_IN2)
nlcd_2016_IN3 <- raster::crop(x = nlcd_2016, y = a_IN3)
nlcd_2016_IN4 <- raster::crop(x = nlcd_2016, y = a_IN4)
nlcd_2016_MI1 <- raster::crop(x = nlcd_2016, y = a_MI1)
nlcd_2016_MI2 <- raster::crop(x = nlcd_2016, y = a_MI2)
nlcd_2016_MI3 <- raster::crop(x = nlcd_2016, y = a_MI3)
nlcd_2016_MI4 <- raster::crop(x = nlcd_2016, y = a_MI4)
nlcd_2016_MN1 <- raster::crop(x = nlcd_2016, y = a_MN1)
nlcd_2016_MN2 <- raster::crop(x = nlcd_2016, y = a_MN2)
nlcd_2016_MN3 <- raster::crop(x = nlcd_2016, y = a_MN3)
nlcd_2016_MN4 <- raster::crop(x = nlcd_2016, y = a_MN4)
nlcd_2016_WI1 <- raster::crop(x = nlcd_2016, y = a_WI1)
nlcd_2016_WI2 <- raster::crop(x = nlcd_2016, y = a_WI2)
nlcd_2016_WI3 <- raster::crop(x = nlcd_2016, y = a_WI3)
nlcd_2016_WI4 <- raster::crop(x = nlcd_2016, y = a_WI4)

# Convert to data frame
nlcd_2016_IL1 <- raster::as.data.frame(nlcd_2016_IL1, xy = TRUE)
nlcd_2016_IL2 <- raster::as.data.frame(nlcd_2016_IL2, xy = TRUE)
nlcd_2016_IL3 <- raster::as.data.frame(nlcd_2016_IL3, xy = TRUE)
nlcd_2016_IL4 <- raster::as.data.frame(nlcd_2016_IL4, xy = TRUE)
nlcd_2016_IN1 <- raster::as.data.frame(nlcd_2016_IN1, xy = TRUE)
nlcd_2016_IN2 <- raster::as.data.frame(nlcd_2016_IN2, xy = TRUE)
nlcd_2016_IN3 <- raster::as.data.frame(nlcd_2016_IN3, xy = TRUE)
nlcd_2016_IN4 <- raster::as.data.frame(nlcd_2016_IN4, xy = TRUE)
nlcd_2016_MI1 <- raster::as.data.frame(nlcd_2016_MI1, xy = TRUE)
nlcd_2016_MI2 <- raster::as.data.frame(nlcd_2016_MI2, xy = TRUE)
nlcd_2016_MI3 <- raster::as.data.frame(nlcd_2016_MI3, xy = TRUE)
nlcd_2016_MI4 <- raster::as.data.frame(nlcd_2016_MI4, xy = TRUE)
nlcd_2016_MN1 <- raster::as.data.frame(nlcd_2016_MN1, xy = TRUE)
nlcd_2016_MN2 <- raster::as.data.frame(nlcd_2016_MN2, xy = TRUE)
nlcd_2016_MN3 <- raster::as.data.frame(nlcd_2016_MN3, xy = TRUE)
nlcd_2016_MN4 <- raster::as.data.frame(nlcd_2016_MN4, xy = TRUE)
nlcd_2016_WI1 <- raster::as.data.frame(nlcd_2016_WI1, xy = TRUE)
nlcd_2016_WI2 <- raster::as.data.frame(nlcd_2016_WI2, xy = TRUE)
nlcd_2016_WI3 <- raster::as.data.frame(nlcd_2016_WI3, xy = TRUE)
nlcd_2016_WI4 <- raster::as.data.frame(nlcd_2016_WI4, xy = TRUE)

# Save 2016 data for further processing on VM
save(nlcd_2016_IL1, nlcd_2016_IL2, nlcd_2016_IL3, nlcd_2016_IL4,
     nlcd_2016_IN1, nlcd_2016_IN2, nlcd_2016_IN3, nlcd_2016_IN4,
     nlcd_2016_MI1, nlcd_2016_MI2, nlcd_2016_MI3, nlcd_2016_MI4,
     nlcd_2016_MN1, nlcd_2016_MN2, nlcd_2016_MN3, nlcd_2016_MN4,
     nlcd_2016_WI1, nlcd_2016_WI2, nlcd_2016_WI3, nlcd_2016_WI4,
     file = 'data/raw/NLCD/nlcd_2016_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2016, nlcd_2016_IL1, nlcd_2016_IL2, nlcd_2016_IL3, nlcd_2016_IL4,
   nlcd_2016_IN1, nlcd_2016_IN2, nlcd_2016_IN3, nlcd_2016_IN4,
   nlcd_2016_MI1, nlcd_2016_MI2, nlcd_2016_MI3, nlcd_2016_MI4,
   nlcd_2016_MN1, nlcd_2016_MN2, nlcd_2016_MN3, nlcd_2016_MN4,
   nlcd_2016_WI1, nlcd_2016_WI2, nlcd_2016_WI3, nlcd_2016_WI4)

# Clip nlcd_2019
nlcd_2019_IL1 <- raster::crop(x = nlcd_2019, y = a_IL1)
nlcd_2019_IL2 <- raster::crop(x = nlcd_2019, y = a_IL2)
nlcd_2019_IL3 <- raster::crop(x = nlcd_2019, y = a_IL3)
nlcd_2019_IL4 <- raster::crop(x = nlcd_2019, y = a_IL4)
nlcd_2019_IN1 <- raster::crop(x = nlcd_2019, y = a_IN1)
nlcd_2019_IN2 <- raster::crop(x = nlcd_2019, y = a_IN2)
nlcd_2019_IN3 <- raster::crop(x = nlcd_2019, y = a_IN3)
nlcd_2019_IN4 <- raster::crop(x = nlcd_2019, y = a_IN4)
nlcd_2019_MI1 <- raster::crop(x = nlcd_2019, y = a_MI1)
nlcd_2019_MI2 <- raster::crop(x = nlcd_2019, y = a_MI2)
nlcd_2019_MI3 <- raster::crop(x = nlcd_2019, y = a_MI3)
nlcd_2019_MI4 <- raster::crop(x = nlcd_2019, y = a_MI4)
nlcd_2019_MN1 <- raster::crop(x = nlcd_2019, y = a_MN1)
nlcd_2019_MN2 <- raster::crop(x = nlcd_2019, y = a_MN2)
nlcd_2019_MN3 <- raster::crop(x = nlcd_2019, y = a_MN3)
nlcd_2019_MN4 <- raster::crop(x = nlcd_2019, y = a_MN4)
nlcd_2019_WI1 <- raster::crop(x = nlcd_2019, y = a_WI1)
nlcd_2019_WI2 <- raster::crop(x = nlcd_2019, y = a_WI2)
nlcd_2019_WI3 <- raster::crop(x = nlcd_2019, y = a_WI3)
nlcd_2019_WI4 <- raster::crop(x = nlcd_2019, y = a_WI4)

# Convert to data frame
nlcd_2019_IL1 <- raster::as.data.frame(nlcd_2019_IL1, xy = TRUE)
nlcd_2019_IL2 <- raster::as.data.frame(nlcd_2019_IL2, xy = TRUE)
nlcd_2019_IL3 <- raster::as.data.frame(nlcd_2019_IL3, xy = TRUE)
nlcd_2019_IL4 <- raster::as.data.frame(nlcd_2019_IL4, xy = TRUE)
nlcd_2019_IN1 <- raster::as.data.frame(nlcd_2019_IN1, xy = TRUE)
nlcd_2019_IN2 <- raster::as.data.frame(nlcd_2019_IN2, xy = TRUE)
nlcd_2019_IN3 <- raster::as.data.frame(nlcd_2019_IN3, xy = TRUE)
nlcd_2019_IN4 <- raster::as.data.frame(nlcd_2019_IN4, xy = TRUE)
nlcd_2019_MI1 <- raster::as.data.frame(nlcd_2019_MI1, xy = TRUE)
nlcd_2019_MI2 <- raster::as.data.frame(nlcd_2019_MI2, xy = TRUE)
nlcd_2019_MI3 <- raster::as.data.frame(nlcd_2019_MI3, xy = TRUE)
nlcd_2019_MI4 <- raster::as.data.frame(nlcd_2019_MI4, xy = TRUE)
nlcd_2019_MN1 <- raster::as.data.frame(nlcd_2019_MN1, xy = TRUE)
nlcd_2019_MN2 <- raster::as.data.frame(nlcd_2019_MN2, xy = TRUE)
nlcd_2019_MN3 <- raster::as.data.frame(nlcd_2019_MN3, xy = TRUE)
nlcd_2019_MN4 <- raster::as.data.frame(nlcd_2019_MN4, xy = TRUE)
nlcd_2019_WI1 <- raster::as.data.frame(nlcd_2019_WI1, xy = TRUE)
nlcd_2019_WI2 <- raster::as.data.frame(nlcd_2019_WI2, xy = TRUE)
nlcd_2019_WI3 <- raster::as.data.frame(nlcd_2019_WI3, xy = TRUE)
nlcd_2019_WI4 <- raster::as.data.frame(nlcd_2019_WI4, xy = TRUE)

# Save 2019 data for further processing on VM
save(nlcd_2019_IL1, nlcd_2019_IL2, nlcd_2019_IL3, nlcd_2019_IL4,
     nlcd_2019_IN1, nlcd_2019_IN2, nlcd_2019_IN3, nlcd_2019_IN4,
     nlcd_2019_MI1, nlcd_2019_MI2, nlcd_2019_MI3, nlcd_2019_MI4,
     nlcd_2019_MN1, nlcd_2019_MN2, nlcd_2019_MN3, nlcd_2019_MN4,
     nlcd_2019_WI1, nlcd_2019_WI2, nlcd_2019_WI3, nlcd_2019_WI4,
     file = 'data/raw/NLCD/nlcd_2019_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2019, nlcd_2019_IL1, nlcd_2019_IL2, nlcd_2019_IL3, nlcd_2019_IL4,
   nlcd_2019_IN1, nlcd_2019_IN2, nlcd_2019_IN3, nlcd_2019_IN4,
   nlcd_2019_MI1, nlcd_2019_MI2, nlcd_2019_MI3, nlcd_2019_MI4,
   nlcd_2019_MN1, nlcd_2019_MN2, nlcd_2019_MN3, nlcd_2019_MN4,
   nlcd_2019_WI1, nlcd_2019_WI2, nlcd_2019_WI3, nlcd_2019_WI4)

# Clip nlcd_2019
nlcd_2021_IL1 <- raster::crop(x = nlcd_2021, y = a_IL1)
nlcd_2021_IL2 <- raster::crop(x = nlcd_2021, y = a_IL2)
nlcd_2021_IL3 <- raster::crop(x = nlcd_2021, y = a_IL3)
nlcd_2021_IL4 <- raster::crop(x = nlcd_2021, y = a_IL4)
nlcd_2021_IN1 <- raster::crop(x = nlcd_2021, y = a_IN1)
nlcd_2021_IN2 <- raster::crop(x = nlcd_2021, y = a_IN2)
nlcd_2021_IN3 <- raster::crop(x = nlcd_2021, y = a_IN3)
nlcd_2021_IN4 <- raster::crop(x = nlcd_2021, y = a_IN4)
nlcd_2021_MI1 <- raster::crop(x = nlcd_2021, y = a_MI1)
nlcd_2021_MI2 <- raster::crop(x = nlcd_2021, y = a_MI2)
nlcd_2021_MI3 <- raster::crop(x = nlcd_2021, y = a_MI3)
nlcd_2021_MI4 <- raster::crop(x = nlcd_2021, y = a_MI4)
nlcd_2021_MN1 <- raster::crop(x = nlcd_2021, y = a_MN1)
nlcd_2021_MN2 <- raster::crop(x = nlcd_2021, y = a_MN2)
nlcd_2021_MN3 <- raster::crop(x = nlcd_2021, y = a_MN3)
nlcd_2021_MN4 <- raster::crop(x = nlcd_2021, y = a_MN4)
nlcd_2021_WI1 <- raster::crop(x = nlcd_2021, y = a_WI1)
nlcd_2021_WI2 <- raster::crop(x = nlcd_2021, y = a_WI2)
nlcd_2021_WI3 <- raster::crop(x = nlcd_2021, y = a_WI3)
nlcd_2021_WI4 <- raster::crop(x = nlcd_2021, y = a_WI4)

# Convert to data frame
nlcd_2021_IL1 <- raster::as.data.frame(nlcd_2021_IL1, xy = TRUE)
nlcd_2021_IL2 <- raster::as.data.frame(nlcd_2021_IL2, xy = TRUE)
nlcd_2021_IL3 <- raster::as.data.frame(nlcd_2021_IL3, xy = TRUE)
nlcd_2021_IL4 <- raster::as.data.frame(nlcd_2021_IL4, xy = TRUE)
nlcd_2021_IN1 <- raster::as.data.frame(nlcd_2021_IN1, xy = TRUE)
nlcd_2021_IN2 <- raster::as.data.frame(nlcd_2021_IN2, xy = TRUE)
nlcd_2021_IN3 <- raster::as.data.frame(nlcd_2021_IN3, xy = TRUE)
nlcd_2021_IN4 <- raster::as.data.frame(nlcd_2021_IN4, xy = TRUE)
nlcd_2021_MI1 <- raster::as.data.frame(nlcd_2021_MI1, xy = TRUE)
nlcd_2021_MI2 <- raster::as.data.frame(nlcd_2021_MI2, xy = TRUE)
nlcd_2021_MI3 <- raster::as.data.frame(nlcd_2021_MI3, xy = TRUE)
nlcd_2021_MI4 <- raster::as.data.frame(nlcd_2021_MI4, xy = TRUE)
nlcd_2021_MN1 <- raster::as.data.frame(nlcd_2021_MN1, xy = TRUE)
nlcd_2021_MN2 <- raster::as.data.frame(nlcd_2021_MN2, xy = TRUE)
nlcd_2021_MN3 <- raster::as.data.frame(nlcd_2021_MN3, xy = TRUE)
nlcd_2021_MN4 <- raster::as.data.frame(nlcd_2021_MN4, xy = TRUE)
nlcd_2021_WI1 <- raster::as.data.frame(nlcd_2021_WI1, xy = TRUE)
nlcd_2021_WI2 <- raster::as.data.frame(nlcd_2021_WI2, xy = TRUE)
nlcd_2021_WI3 <- raster::as.data.frame(nlcd_2021_WI3, xy = TRUE)
nlcd_2021_WI4 <- raster::as.data.frame(nlcd_2021_WI4, xy = TRUE)

# Save 2021 data for further processing on VM
save(nlcd_2021_IL1, nlcd_2021_IL2, nlcd_2021_IL3, nlcd_2021_IL4,
     nlcd_2021_IN1, nlcd_2021_IN2, nlcd_2021_IN3, nlcd_2021_IN4,
     nlcd_2021_MI1, nlcd_2021_MI2, nlcd_2021_MI3, nlcd_2021_MI4,
     nlcd_2021_MN1, nlcd_2021_MN2, nlcd_2021_MN3, nlcd_2021_MN4,
     nlcd_2021_WI1, nlcd_2021_WI2, nlcd_2021_WI3, nlcd_2021_WI4,
     file = 'data/raw/NLCD/nlcd_2021_states.RData')

# Remove year-specific files and repeat for next year
rm(nlcd_2021, nlcd_2021_IL1, nlcd_2021_IL2, nlcd_2021_IL3, nlcd_2021_IL4,
   nlcd_2021_IN1, nlcd_2021_IN2, nlcd_2021_IN3, nlcd_2021_IN4,
   nlcd_2021_MI1, nlcd_2021_MI2, nlcd_2021_MI3, nlcd_2021_MI4,
   nlcd_2021_MN1, nlcd_2021_MN2, nlcd_2021_MN3, nlcd_2021_MN4,
   nlcd_2021_WI1, nlcd_2021_WI2, nlcd_2021_WI3, nlcd_2021_WI4)
