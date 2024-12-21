#### STEP 4-8

## Multivariate random forest fit to HISTORICAL
## RELATIVE ABUNDANCE and CLIMATE and SOIL covariates

## NOTE that the random forest model is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to your file structure

## 1. Load data
## 2. Hyperparameter tuning
## 3. Fit random forest
## 4. Variable importance
## 5. Partial effects plots

## Input: data/processed/PLS/xydata_in.RData
## Dataframe of in-sample grid cells with historical (PLS) era
## vegetation, soil, and climate data
## From 1.3.Split_data.R

## Output: /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData
## Fitted random forest object saved to external hard drive
## Used in 4.12.abundance_historical_predictions.R,
## 4.13.abundance_modern_predictions.R

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
                clay, sand, silt, caco3, awc, flood, # edaphic variables
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
  ggplot2::geom_vline(ggplot2::aes(xintercept = round(sqrt(13)),
                                   color = 'default')) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::theme_minimal()

## Use nodesize = 1, mtry = 4 to balance betweeen optimal mtry
## and reducing correlation between learners

#### 3. Fit random forest ####

# Fit random forest
abund_rf_H_allcovar <- randomForestSRC::rfsrc(formula = Multivar(Ash, Basswood, Beech,
                                                                 Birch, Cherry, Dogwood,
                                                                 Elm, Fir, Hemlock,
                                                                 Hickory, Ironwood, Maple,
                                                                 Oak, Pine, Spruce,
                                                                 Tamarack, Walnut, oh,
                                                                 gum, cedar, poplar) ~ ., # formula
                                              data = rf_data, # data
                                              ntree = 1000, # higher number of trees because this is prodution quality
                                              mtry = 4, # from above decision
                                              nodesize = 1, # from above decision
                                              importance = TRUE, # calculate variable importance
                                              forest = TRUE) # save forest variables

# Save
# Change directory according to your file structure
save(abund_rf_H_allcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData')

#### 4. Variable importance ####

## This is used to see if any variable should be removed
## (if confience intervals overlap zero)
## And what variables should be selected for
## a reduced model ran later

# Calculate variable importance with confidence intervals
var_imp <- randomForestSRC::subsample(obj = abund_rf_H_allcovar,
                                      B = 100)

# Save importance with confidence intervals
imp_CI <- randomForestSRC::extract.subsample(var_imp)$var.jk.sel.Z

# Plot importance
imp_CI |>
  tibble::rownames_to_column(var = 'var') |>
  dplyr::mutate(var = dplyr::if_else(var == 'clay', 'Soil % clay', var),
                var = dplyr::if_else(var == 'sand', 'Soil % sand', var),
                var = dplyr::if_else(var == 'silt', 'Soil % silt', var),
                var = dplyr::if_else(var == 'caco3', 'Soil calcium carbonate\nconcentration', var),
                var = dplyr::if_else(var == 'awc', 'Available water\ncontent', var),
                var = dplyr::if_else(var == 'flood', '% grid cell in\nfloodplain', var),
                var = dplyr::if_else(var == 'ppt_sum', 'Total annual\nprecipitation', var),
                var = dplyr::if_else(var == 'tmean_mean', 'Mean annual\ntemperature', var),
                var = dplyr::if_else(var == 'ppt_cv', 'Precipitation\nseasonality (CV)', var),
                var = dplyr::if_else(var == 'tmean_sd', 'Temperature\nseasonality (SD)', var),
                var = dplyr::if_else(var == 'tmin', 'Minimum annual\ntemperature', var),
                var = dplyr::if_else(var == 'tmax', 'Maximum annual\ntemperature', var),
                var = dplyr::if_else(var == 'vpdmax', 'Maximum annual vapor\npressure deficit', var)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = reorder(var, mean), y = mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(x = reorder(var, mean),
                                      ymin = lower, ymax = upper)) +
  ggplot2::xlab('') + ggplot2::ylab('Relative importance') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

## Most important: maximum annual temperature,
## precipitaiton seasonality, total annual precipitation

# Find minimum depth of each variable across trees
var_select <- randomForestSRC::var.select(object = abund_rf_H_allcovar,
                                          method = 'md', # minimum depth
                                          ntree = 500,
                                          conservative = 'low')

# Format variable depth
var_depth <- var_select$varselect

# Plot variable depth and importance together
# Note that importance is log-transform so
# both fit on the same axis.
# This is illustrative only. It is not recommended
# to plot multiple variables on the same axis
# like this
var_depth |>
  tibble::rownames_to_column(var = 'var') |>
  dplyr::mutate(var = dplyr::if_else(var == 'clay', 'Soil % clay', var),
                var = dplyr::if_else(var == 'sand', 'Soil % sand', var),
                var = dplyr::if_else(var == 'silt', 'Soil % silt', var),
                var = dplyr::if_else(var == 'caco3', 'Soil calcium carbonate\nconcentration', var),
                var = dplyr::if_else(var == 'awc', 'Available water\ncontent', var),
                var = dplyr::if_else(var == 'flood', '% grid cell in\nfloodplain', var),
                var = dplyr::if_else(var == 'ppt_sum', 'Total annual\nprecipitation', var),
                var = dplyr::if_else(var == 'tmean_mean', 'Mean annual\ntemperature', var),
                var = dplyr::if_else(var == 'ppt_cv', 'Precipitation\nseasonality (CV)', var),
                var = dplyr::if_else(var == 'tmean_sd', 'Temperature\nseasonality (SD)', var),
                var = dplyr::if_else(var == 'tmin', 'Minimum annual\ntemperature', var),
                var = dplyr::if_else(var == 'tmax', 'Maximum annual\ntemperature', var),
                var = dplyr::if_else(var == 'vpdmax', 'Maximum annual vapor\npressure deficit', var)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = reorder(var, -depth),
                                   y = depth,
                                   color = 'Minimum\ndepth')) +
  ggplot2::geom_point(ggplot2::aes(x = reorder(var, -depth), 
                                   y = log(vimp),
                                   color = 'Importance')) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('') + ggplot2::ylab('Minimum depth / log(Importance)') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

## For reduced covariate random forest: choose maximum
## annual temperature, total annual precipitation, precipitation
## seasonality, and soil % clay
## Skipping mean annual temperature because correlation coefficient
## between maximum and mean temperature = 0.95. Soil variables
## are not very important according to either analysis, but
## clay is the most important

#### 5. Partial effects plots ####

## Partial plots are done separately for each taxon

### Ash ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Ash',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Ash',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Ash',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'caco3',
                                               m.target = 'Ash',
                                               partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'awc',
                                                m.target = 'Ash',
                                                partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'flood',
                                              m.target = 'Ash',
                                              partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'ppt_sum',
                                                m.target = 'Ash',
                                                partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'tmean_mean',
                                              m.target = 'Ash',
                                              partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'ppt_cv',
                                                m.target = 'Ash',
                                                partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'tmean_sd',
                                                 m.target = 'Ash',
                                                 partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmin',
                                                   m.target = 'Ash',
                                                   partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Ash',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'vpdmax',
                                               m.target = 'Ash',
                                               partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted ash relative abundance') +
  ggplot2::ylim(c(0.033, 0.054)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ash_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Basswood ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Basswood',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Basswood',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Basswood',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Basswood',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Basswood',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Basswood',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Basswood',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Basswood',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Basswood',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Basswood',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Basswood',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Basswood',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Basswood',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted basswood relative abundance') +
  ggplot2::ylim(c(0.017, 0.030)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_basswood_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Beech ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Beech',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Beech',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Beech',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Beech',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Beech',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Beech',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Beech',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Beech',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Beech',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Beech',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Beech',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Beech',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Beech',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted beech relative abundance') +
  ggplot2::ylim(c(0.036, 0.119)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_beech_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Birch ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Birch',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Birch',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Birch',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Birch',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Birch',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Birch',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Birch',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Birch',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Birch',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Birch',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Birch',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Birch',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Birch',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted birch relative abundance') +
  ggplot2::ylim(c(0.023, 0.070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_birch_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Cherry ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Cherry',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Cherry',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Cherry',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Cherry',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Cherry',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Cherry',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Cherry',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Cherry',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Cherry',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Cherry',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Cherry',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Cherry',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Cherry',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted cherry relative abundance') +
  ggplot2::ylim(c(0.0022, 0.0038)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cherry_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Dogwood ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Dogwood',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Dogwood',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Dogwood',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Dogwood',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Dogwood',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Dogwood',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Dogwood',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Dogwood',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Dogwood',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Dogwood',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Dogwood',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Dogwood',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Dogwood',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted dogwood relative abundance') +
  ggplot2::ylim(c(0.0011, 0.0070)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_dogwood_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Elm ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Elm',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Elm',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Elm',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Elm',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Elm',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Elm',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Elm',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Elm',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Elm',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Elm',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Elm',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Elm',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Elm',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted elm relative abundance') +
  ggplot2::ylim(c(0.039, 0.068)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_elm_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Fir ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Fir',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Fir',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Fir',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Fir',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Fir',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Fir',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Fir',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Fir',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Fir',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Fir',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Fir',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Fir',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Fir',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted fir relative abundance') +
  ggplot2::ylim(c(0.0041, 0.028)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_fir_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Hemlock ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Hemlock',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Hemlock',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Hemlock',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Hemlock',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Hemlock',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Hemlock',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Hemlock',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Hemlock',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Hemlock',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Hemlock',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Hemlock',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Hemlock',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Hemlock',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted hemlock relative abundance') +
  ggplot2::ylim(c(0.016, 0.058)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hemlock_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Hickory ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Hickory',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Hickory',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Hickory',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Hickory',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Hickory',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Hickory',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Hickory',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Hickory',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Hickory',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Hickory',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Hickory',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Hickory',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Hickory',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted hickory relative abundance') +
  ggplot2::ylim(c(0.026, 0.074)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_hickory_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Ironwood ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Ironwood',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Ironwood',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Ironwood',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Ironwood',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Ironwood',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Ironwood',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Ironwood',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Ironwood',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Ironwood',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Ironwood',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Ironwood',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Ironwood',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Ironwood',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted ironwood relative abundance') +
  ggplot2::ylim(c(0.0095, 0.017)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_ironwood_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Maple ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Maple',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Maple',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Maple',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Maple',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Maple',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Maple',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Maple',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Maple',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Maple',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Maple',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Maple',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Maple',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Maple',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted maple relative abundance') +
  ggplot2::ylim(c(0.057, 0.099)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_maple_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Oak ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Oak',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Oak',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Oak',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Oak',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Oak',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Oak',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Oak',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Oak',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Oak',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Oak',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Oak',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Oak',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Oak',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted oak relative abundance') +
  ggplot2::ylim(c(0.22, 0.45)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oak_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Pine ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Pine',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Pine',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Pine',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Pine',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Pine',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Pine',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Pine',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Pine',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Pine',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Pine',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Pine',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Pine',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Pine',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted pine relative abundance') +
  ggplot2::ylim(c(0.036, 0.085)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_pine_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Spruce ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Spruce',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Spruce',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Spruce',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Spruce',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Spruce',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Spruce',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Spruce',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Spruce',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Spruce',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Spruce',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Spruce',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Spruce',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Spruce',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted spruce relative abundance') +
  ggplot2::ylim(c(0.0068, 0.045)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_spruce_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Tamarack ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Tamarack',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Tamarack',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Tamarack',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Tamarack',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Tamarack',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Tamarack',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Tamarack',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Tamarack',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Tamarack',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Tamarack',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Tamarack',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Tamarack',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Tamarack',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted tamarack relative abundance') +
  ggplot2::ylim(c(0.029, 0.065)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_tamarack_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Walnut ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'Walnut',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'Walnut',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'Walnut',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'Walnut',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'Walnut',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'Walnut',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'Walnut',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'Walnut',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'Walnut',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'Walnut',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'Walnut',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'Walnut',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'Walnut',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted walnut relative abundance') +
  ggplot2::ylim(c(0.0042, 0.0097)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_walnut_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Other hardwood ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'oh',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'oh',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'oh',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'oh',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'oh',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'oh',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'oh',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'oh',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'oh',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'oh',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'oh',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'oh',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'oh',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted other hardwood\nrelative abundance') +
  ggplot2::ylim(c(0.010, 0.022)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_oh_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Black gum/sweet gum ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'gum',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted black gum/sweetgum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'gum',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'gum',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gumm_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'gum',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'gum',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'gum',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'gum',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'gum',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'gum',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'gum',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'gum',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'gum',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'gum',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted black gum/sweet gum\nrelative abundance') +
  ggplot2::ylim(c(0.0015, 0.0083)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_gum_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Cedar/juniper ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'cedar',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'cedar',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'cedar',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'cedar',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'cedar',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'cedar',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'cedar',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'cedar',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'cedar',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'cedar',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'cedar',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'cedar',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'cedar',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted cedar/juniper\nrelative abundance') +
  ggplot2::ylim(c(0.0095, 0.039)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_cedar_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### Poplar/tulip poplar ###

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'clay',
                                               m.target = 'poplar',
                                               partial = TRUE)

# Partial plot data
clay_data <- clay_partial$pData$clay

# Partial plot
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x.uniq, y = clay_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x.uniq,
                                    ymin = clay_data$yhat - clay_data$yhat.se,
                                    ymax = clay_data$yhat + clay_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               m.target = 'poplar',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               m.target = 'poplar',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Soil calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                m.target = 'poplar',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Soil available water content
awc_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              m.target = 'poplar',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in a floodplain
flood_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                m.target = 'poplar',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              m.target = 'poplar',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                m.target = 'poplar',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 m.target = 'poplar',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   m.target = 'poplar',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard devaition (°C))') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               m.target = 'poplar',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               m.target = 'poplar',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual vapor pressure deficit
vpd_partial <- randomForestSRC::plot.variable(x = abund_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              m.target = 'poplar',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') + ggplot2::ylab('Predicted poplar/tulip poplar\nrelative abundance') +
  ggplot2::ylim(c(0.043, 0.080)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/fit/all_covar_partial_poplar_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')
