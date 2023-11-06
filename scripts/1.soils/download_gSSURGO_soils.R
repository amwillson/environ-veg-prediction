## Downloading data for each state in the Upper Midwest region of interest

## Author: AM Willson

rm(list = ls())

library(sf)
library(soilDB)
library(terra)
library(ggplot2)
library(dplyr)

# Helper function for dividing the state into four quadrants
source('scripts/1.soils/define_bounds.R')

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

# fetch gSSURGO map unit keys at 80m resolution (lowest possible = 30m)
# lowest resolution possible for all states
# (resolution limited by extent of Michigain)
# will need to be rescaled for use in analysis
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

# Save downloads
save(mu_IL1, mu_IL2, mu_IL3, mu_IL4,
     mu_IN1, mu_IN2, mu_IN3, mu_IN4,
     mu_MI1, mu_MI2, mu_MI3, mu_MI4,
     mu_MN1, mu_MN2, mu_MN3, mu_MN4,
     mu_WI1, mu_WI2, mu_WI3, mu_WI4,
     file = 'data/raw/gssurgo_state.RData')
