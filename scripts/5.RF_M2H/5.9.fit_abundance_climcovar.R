#### STEP 5-9

## Multivariate random forest fit to MODERN
## RELATIVE ABUNDANCE and CLIMATE coviarates only

## NOTE that the random forest model is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to your file structure

## 1. Load data
## 2. Hyperparameter tuning
## 3. Fit random forest

## Input: data/processed/FIA/xydata_in.RData
## Dataframe of in-sample grid cells with modern (FIA) era
## vegetation, soil, and climate data

## Output: /Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/climcovar.RData
## Fitted random forest object saved to external hard drive
## Used in 5.12.abundance_historical_predictions.R,
## 5.13.abundance_modern_predictions.R

rm(list = ls())

#### 1. Load data ####

# Load FIA data
load('data/processed/FIA/xydata_in.RData')

# Select relevant columns
rf_data <- fia_in |>
  dplyr::ungroup() |>
  dplyr::select(Ash, Basswood, Beech,
                Birch, Cherry, Dogwood,
                Elm, Fir, Hemlock,
                Hickory, Ironwood, Maple,
                Oak, Pine, Spruce,
                Tamarack, Walnut,
                `Other hardwood`,
                `Black gum/sweet gum`,
                `Cedar/juniper`,
                `Poplar/tulip poplar`, # response variables
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::rename(oh = `Other hardwood`,
                gum = `Black gum/sweet gum`,
                cedar = `Cedar/juniper`,
                poplar = `Poplar/tulip poplar`) |>
  dplyr::distinct()

# Convert to regular dataframe
rf_data <- as.data.frame(rf_data)

#### 2. Hyperparameter tuning ####

# Tune mtry and nodesize
tune_rf <- randomForestSRC::tune(formula = Multivar(Ash, Basswood, Beech,
                                                    Birch, Cherry, Dogwood,
                                                    Elm, Fir, Hemlock,
                                                    Hickory, Ironwood, Maple,
                                                    Oak, Pine, Spruce,
                                                    Tamarack, Walnut, oh,
                                                    gum, cedar, poplar) ~ ., # formula
                                 data = rf_data, # data
                                 nodesizeTry = 1:10, # node sizes to try
                                 mtryStart = 3, # low to force trying low values
                                 ntreeTry = 500) # number of trees to grow

# Optimal hyperparameter combination
opt_hyper <- tune_rf$optimal
opt_hyper

# Format error rate for all combinations of hyperparameters
tune_hyper <- as.data.frame(tune_rf$results)

# Plot error rate for all combinations
tune_hyper |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = nodesize, y = mtry,
                                  fill = err)) +
  ggplot2::theme_minimal()

# Plot error rate for all mtry options with nodesize = 2
tune_hyper |>
  dplyr::filter(nodesize == 2) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = mtry, y = err),
                    stat = 'identity') +
  ggplot2::geom_vline(ggplot2::aes(xintercept = tune_rf$optimal[2],
                                   color = 'optimal')) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = round(sqrt(7)),
                                   color = 'default')) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::theme_minimal()

## Use nodesize = 2, mtry = 3 because there is minimal
## change in error rate between mtry = 3 and 6

#### 3. Fit random forest ####

# Fit random forest
abund_rf_M_climcovar <- randomForestSRC::rfsrc(formula = Multivar(Ash, Basswood, Beech,
                                                                  Birch, Cherry, Dogwood,
                                                                  Elm, Fir, Hemlock,
                                                                  Hickory, Ironwood, Maple,
                                                                  Oak, Pine, Spruce,
                                                                  Tamarack, Walnut, oh,
                                                                  gum, cedar, poplar) ~ ., # formula
                                               data = rf_data, # data
                                               ntree = 1000, # higher number of trees because this is production quality
                                               mtry = 3, # from above decision
                                               nodesize = 2, # from above decision
                                               importance = TRUE, # calculate variable importance
                                               forest = TRUE) # save forest variables

# Save
save(abund_rf_M_climcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/rf/M/abundance/climcovar.RData')
