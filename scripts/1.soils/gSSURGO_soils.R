## Downloading data for each 
rm(list = ls())

library(sf)
library(soilDB)
library(terra)
library(ggplot2)
library(dplyr)

source('Google Drive 2/define_bounds.R')

# Define bounding boxes for each state
IL <- define_quadrants(xmin = -91.514, xmax = -87.4947,
                    ymin = 36.9704, ymax = 42.5088)

IL1 <- IL[[1]]
IL2 <- IL[[2]]
IL3 <- IL[[3]]
IL4 <- IL[[4]]

a_IL1 <- sf::st_bbox(
  c(xmin = IL1[1], xmax = IL1[2],
    ymin = IL1[3], ymax = IL1[4]),
  crs = sf::st_crs(4326)
)
a_IL2 <- sf::st_bbox(
  c(xmin = IL2[1], xmax = IL2[2],
    ymin = IL2[3], ymax = IL2[4]),
  crs = sf::st_crs(4326)
)
a_IL3 <- sf::st_bbox(
  c(xmin = IL3[1], xmax = IL3[2],
    ymin = IL3[3], ymax = IL3[4]),
  crs = sf::st_crs(4326)
)
a_IL4 <- sf::st_bbox(
  c(xmin = IL4[1], xmax = IL4[2],
    ymin = IL4[3], ymax = IL4[4]),
  crs = sf::st_crs(4326)
)

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
)
a_IN2 <- sf::st_bbox(
  c(xmin = IN2[1], xmax = IN2[2],
    ymin = IN2[3], ymax = IN2[4]),
  crs = sf::st_crs(4326)
)
a_IN3 <- sf::st_bbox(
  c(xmin = IN3[1], xmax = IN3[2],
    ymin = IN3[3], ymax = IN3[4]),
  crs = sf::st_crs(4326)
)
a_IN4 <- sf::st_bbox(
  c(xmin = IN4[1], xmax = IN4[2],
    ymin = IN4[3], ymax = IN4[4]),
  crs = sf::st_crs(4326)
)

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
)
a_MI2 <- sf::st_bbox(
  c(xmin = MI2[1], xmax = MI2[2],
    ymin = MI2[3], ymax = MI2[4]),
  crs = sf::st_crs(4326)
)
a_MI3 <- sf::st_bbox(
  c(xmin = MI3[1], xmax = MI3[2],
    ymin = MI3[3], ymax = MI3[4]),
  crs = sf::st_crs(4326)
)
a_MI4 <- sf::st_bbox(
  c(xmin = MI4[1], xmax = MI4[2],
    ymin = MI4[3], ymax = MI4[4]),
  crs = sf::st_crs(4326)
)

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
)
a_MN2 <- sf::st_bbox(
  c(xmin = MN2[1], xmax = MN2[2],
    ymin = MN2[3], ymax = MN2[4]),
  crs = sf::st_crs(4326)
)
a_MN3 <- sf::st_bbox(
  c(xmin = MN3[1], xmax = MN3[2],
    ymin = MN3[3], ymax = MN3[4]),
  crs = sf::st_crs(4326)
)
a_MN4 <- sf::st_bbox(
  c(xmin = MN4[1], xmax = MN4[2],
    ymin = MN4[3], ymax = MN4[4]),
  crs = sf::st_crs(4326)
)

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
)
a_WI2 <- sf::st_bbox(
  c(xmin = WI2[1], xmax = WI2[2],
    ymin = WI2[3], ymax = WI2[4]),
  crs = sf::st_crs(4326)
)
a_WI3 <- sf::st_bbox(
  c(xmin = WI3[1], xmax = WI3[2],
    ymin = WI3[3], ymax = WI3[4]),
  crs = sf::st_crs(4326)
)
a_WI4 <- sf::st_bbox(
  c(xmin = WI4[1], xmax = WI4[2],
    ymin = WI4[3], ymax = WI4[4]),
  crs = sf::st_crs(4326)
)

# fetch gSSURGO map unit keys at 3000m resolution
# lowest resolution possible
# need to scale to the correct 8x8 km grid!!
mu_IL1 <- soilDB::mukey.wcs(aoi = a_IL1, db = 'gssurgo', res = 80)
mu_IL2 <- soilDB::mukey.wcs(aoi = a_IL2, db = 'gssurgo', res = 80)
mu_IL3 <- soilDB::mukey.wcs(aoi = a_IL3, db = 'gssurgo', res = 80)
mu_IL4 <- soilDB::mukey.wcs(aoi = a_IL4, db = 'gssurgo', res = 80)

mu_IN1 <- soilDB::mukey.wcs(aoi = a_IN1, db = 'gssurgo', res = 80)
mu_IN2 <- soilDB::mukey.wcs(aoi = a_IN2, db = 'gssurgo', res = 80)
mu_IN3 <- soilDB::mukey.wcs(aoi = a_IN3, db = 'gssurgo', res = 80)
mu_IN4 <- soilDB::mukey.wcs(aoi = a_IN4, db = 'gssurgo', res = 80)

mu_MI1 <- soilDB::mukey.wcs(aoi = a_MI1, db = 'gssurgo', res = 80)
mu_MI2 <- soilDB::mukey.wcs(aoi = a_MI2, db = 'gssurgo', res = 80)
mu_MI3 <- soilDB::mukey.wcs(aoi = a_MI3, db = 'gssurgo', res = 80)
mu_MI4 <- soilDB::mukey.wcs(aoi = a_MI4, db = 'gssurgo', res = 80)

mu_MN1 <- soilDB::mukey.wcs(aoi = a_MN1, db = 'gssurgo', res = 80)
mu_MN2 <- soilDB::mukey.wcs(aoi = a_MN2, db = 'gssurgo', res = 80)
mu_MN3 <- soilDB::mukey.wcs(aoi = a_MN3, db = 'gssurgo', res = 80)
mu_MN4 <- soilDB::mukey.wcs(aoi = a_MN4, db = 'gssurgo', res = 80)

mu_WI1 <- soilDB::mukey.wcs(aoi = a_WI1, db = 'gssurgo', res = 80)
mu_WI2 <- soilDB::mukey.wcs(aoi = a_WI2, db = 'gssurgo', res = 80)
mu_WI3 <- soilDB::mukey.wcs(aoi = a_WI3, db = 'gssurgo', res = 80)
mu_WI4 <- soilDB::mukey.wcs(aoi = a_WI4, db = 'gssurgo', res = 80)

# extract RAT for thematic mapping for each state
rat_IL1 <- terra::cats(mu_IL1)[[1]]
rat_IL2 <- terra::cats(mu_IL2)[[1]]
rat_IL3 <- terra::cats(mu_IL3)[[1]]
rat_IL4 <- terra::cats(mu_IL4)[[1]]

rat_IN1 <- terra::cats(mu_IN1)[[1]]
rat_IN2 <- terra::cats(mu_IN2)[[1]]
rat_IN3 <- terra::cats(mu_IN3)[[1]]
rat_IN4 <- terra::cats(mu_IN4)[[1]]

rat_MI1 <- terra::cats(mu_MI1)[[1]]
rat_MI2 <- terra::cats(mu_MI2)[[1]]
rat_MI3 <- terra::cats(mu_MI3)[[1]]
rat_MI4 <- terra::cats(mu_MI4)[[1]]

rat_MN1 <- terra::cats(mu_MN1)[[1]]
rat_MN2 <- terra::cats(mu_MN2)[[1]]
rat_MN3 <- terra::cats(mu_MN3)[[1]]
rat_MN4 <- terra::cats(mu_MN4)[[1]]

rat_WI1 <- terra::cats(mu_WI1)[[1]]
rat_WI2 <- terra::cats(mu_WI2)[[1]]
rat_WI3 <- terra::cats(mu_WI3)[[1]]
rat_WI4 <- terra::cats(mu_WI4)[[1]]

# save
save(rat_IL1, rat)
# define variables of interest
vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r')

# get data for variables of interest for each state
# property = variables
# method = dominant component
# mukeys = look-up
# top depth  & bottom depth are varied as experiments to see sensitivity
tab_IL <- soilDB::get_SDA_property(property = vars,
                                   method = 'Dominant Component (Numeric)',
                                   mukeys = as.integer(rat_IL$mukey),
                                   top_depth = 25,
                                   bottom_depth = 50)
tab_IN <- soilDB::get_SDA_property(property = vars,
                                   method = 'Dominant Component (Numeric)',
                                   mukeys = as.integer(rat_IN$mukey),
                                   top_depth = 25,
                                   bottom_depth = 50)
tab_MI <- soilDB::get_SDA_property(property = vars,
                                   method = 'Dominant Component (Numeric)',
                                   mukeys = as.integer(rat_MI$mukey),
                                   top_depth = 25,
                                   bottom_depth = 50)
tab_MN <- soilDB::get_SDA_property(property = vars,
                                   method = 'Dominant Component (Numeric)',
                                   mukeys = as.integer(rat_MN$mukey),
                                   top_depth = 25,
                                   bottom_depth = 50)
tab_WI <- soilDB::get_SDA_property(property = vars,
                                   method = 'Dominant Component (Numeric)',
                                   mukeys = as.integer(rat_WI$mukey),
                                   top_depth = 25,
                                   bottom_depth = 50)

# set raster categories 
levels(mu_IL) <- tab_IL[, c('mukey', vars)]
levels(mu_IN) <- tab_IN[, c('mukey', vars)]
levels(mu_MI) <- tab_MI[, c('mukey', vars)]
levels(mu_MN) <- tab_MN[, c('mukey', vars)]
levels(mu_WI) <- tab_WI[, c('mukey', vars)]

# stack of numerical grids
ssc_IL <- terra::catalyze(mu_IL)
ssc_IN <- terra::catalyze(mu_IN)
ssc_MI <- terra::catalyze(mu_MI)
ssc_MN <- terra::catalyze(mu_MN)
ssc_WI <- terra::catalyze(mu_WI)

# reproject
ssc_IL <- terra::project(x = ssc_IL, 'EPSG:4326')
ssc_IN <- terra::project(x = ssc_IN, 'EPSG:4326')
ssc_MI <- terra::project(x = ssc_MI, 'EPSG:4326')
ssc_MN <- terra::project(x = ssc_MN, 'EPSG:4326')
ssc_WI <- terra::project(x = ssc_WI, 'EPSG:4326')

# Convert to data frame
df_IL <- raster::as.data.frame(x = ssc_IL, xy = TRUE)
df_IN <- raster::as.data.frame(x = ssc_IN, xy = TRUE)
df_MI <- raster::as.data.frame(x = ssc_MI, xy = TRUE)
df_MN <- raster::as.data.frame(x = ssc_MN, xy = TRUE)
df_WI <- raster::as.data.frame(x = ssc_WI, xy = TRUE)

# Combine
soils_df <- rbind(df_IL, df_IN, df_MI, df_MN, df_WI)

# Make map to compare against
states <- map_data('state', region = c('illinois', 'indiana', 'michigan', 'minnesota', 'wisconsin'))

# Take out values that are not (0,1) bounded
for_removal <- which(soils_df$sandtotal_r < 0 | soils_df$sandtotal_r > 100 |
                       soils_df$silttotal_r < 0 | soils_df$silttotal_r > 100 |
                       soils_df$claytotal_r < 0 | soils_df$claytotal_r > 100)
soils_df <- soils_df[-for_removal,]

# Plot each variable
soils_df |>
  dplyr::rename(`% sand` = sandtotal_r) |>
  ggplot(aes(x = x, y = y, color = `% sand`)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_map(projection = 'albers', lat0 = 50, lat1 = 40) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrBr', limits = c(0, 100)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
soils_df |>
  dplyr::rename(`% silt` = silttotal_r) |>
  ggplot(aes(x = x, y = y, color = `% silt`)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_map(projection = 'albers', lat0 = 50, lat1 = 40) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrBr', limits = c(0, 100)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
soils_df |>
  dplyr::rename(`% clay` = claytotal_r) |>
  ggplot(aes(x = x, y = y, color = `% clay`)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_map(projection = 'albers', lat0 = 50, lat1 = 40) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrBr', limits = c(0, 100)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
