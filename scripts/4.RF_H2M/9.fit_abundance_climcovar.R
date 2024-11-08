## STEP 4-9

## Multivariate random forest fit to HISTORICAL
## RELATIVE ABUNDANCE and CLIMATE coviarates only

## 1. Load data
## 2. Hyperparameter tuning
## 3. Fit random forest

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select relevant columns
rf_data <- pls_in |>
  dplyr::select(Ash, Basswood, Beech,
                Birch, Cherry, Dogwood,
                Elm, Fir, Hemlock,
                Hickory, Ironwood, Maple,
                Oak, Pine, Spruce,
                Tamarack, Walnut,
                `Other hardwood`,
                `Black gum/sweet gum`,
                `Cedar/juniper`,
                `Poplar/tulip poplar`, # response variable
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::rename(oh = `Other hardwood`, # rename so no special characters
                gum = `Black gum/sweet gum`,
                cedar = `Cedar/juniper`,
                poplar = `Poplar/tulip poplar`) |>
  dplyr::distinct()

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

# Plot error rate for all mtry options with nodesize = 1
tune_hyper |>
  dplyr::filter(nodesize == 1) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = mtry, y = err),
                    stat = 'identity') +
  ggplot2::geom_vline(ggplot2::aes(xintercept = tune_rf$optimal[2],
                                   color = 'optimal')) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = round(sqrt(7)),
                                   color = 'default')) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::theme_minimal()

## Use nodesize = 1, mtry = 3 because there is minimal
## change in error rate between mtry = 3 and 7

#### 3. Fit random forest ####

# Fit random forest
abund_rf_H_climcovar <- randomForestSRC::rfsrc(formula = Multivar(Ash, Basswood, Beech,
                                                                 Birch, Cherry, Dogwood,
                                                                 Elm, Fir, Hemlock,
                                                                 Hickory, Ironwood, Maple,
                                                                 Oak, Pine, Spruce,
                                                                 Tamarack, Walnut, oh,
                                                                 gum, cedar, poplar) ~ ., # formula
                                              data = rf_data, # data
                                              ntree = 1000, # higher number of trees because this is production quality
                                              mtry = 3, # from above decision
                                              nodesize = 1, # from above decision
                                              importance = TRUE, # calculate variable importance
                                              forest = TRUE) # save forest variables

# Save
save(abund_rf_H_climcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/climcovar.RData')
