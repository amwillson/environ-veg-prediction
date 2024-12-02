#### STEP 6-4

## Univariate GAM fit to HISTORICAL
## TOTAL STEM DENSITY and CLIMATE and SOIL covariates
## plus COORDINATES

## Note that this script does not include all of the steps of the other
## scripts because the initial model fit here fails to converge
## This suggested to me that this model is not really appropriate
## so I did not continue with this model

## 1. Load data
## 2. Fit GAM

## Input: data/processed/PLS/xydata_in.RData
## Dataframe of in-sample grid cells with historical (PLS) era
## vegetation, soil, and climate data

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/xycovar.RData
## Fitted GAM object saved to external hard drive
## This is never used because the model could not converge

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select relevant columns
gam_data <- pls_in |>
  dplyr::select(total_density, # response variable
                clay, sand, silt, caco3, awc, flood, # edaphic variables
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax, # climatic variables
                x, y) |> # coordinates
  dplyr::rename(x_coord = x,
                y_coord = y) |>
  dplyr::distinct()

#### 2. Fit GAM ####

## Fit model with all covariates plus coordinates

# Fit GAM
density_gam_H_xycovar <- mvgam::mvgam(formula = total_density ~
                                        s(clay) +
                                        s(sand) +
                                        s(silt) +
                                        s(caco3) +
                                        s(awc) +
                                        s(flood) +
                                        s(ppt_sum) +
                                        s(tmean_mean) +
                                        s(ppt_cv) +
                                        s(tmean_sd) +
                                        s(tmin) +
                                        s(tmax) +
                                        s(vpdmax) +
                                        s(x_coord) +
                                        s(y_coord),
                                      data = gam_data,
                                      burnin = 1000,
                                      samples = 1000,
                                      family = mvgam::lognormal()) # Takes about 

## Note that this model cannot converge because the coordinates are too correlated with
## some of the climate variables
## I am saving to show this, but this model will not be pursued further

# Save
save(density_gam_H_xycovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/xycovar.RData')
