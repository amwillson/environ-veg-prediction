## STEP 4-1

## Univariate random forest fit to HISTORICAL 
## TOTAL STEM DENSITY and CLIMATE and SOIL covariates

## 1. Load data
## 2. Hyperparameter tuning
## 3. Fit random forest
## 4. Variable importance
## 5. Partial effects plots

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select relevant columns
rf_data <- pls_in |>
  dplyr::select(total_density, # response variable
                clay, sand, silt, caco3, awc, flood, # edaphic variables
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::distinct()

#### 2. Hyperparameter tuning ####

# Tune mtry and nodesize
tune_rf <- randomForestSRC::tune(formula = total_density ~ ., # formula
                                 data = rf_data, # data
                                 nodesizeTry = 1:10, # node sizes to try, default is typically 5
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
  ggplot2::geom_vline(ggplot2::aes(xintercept = tune_rf$optimal[2], color = 'optimal')) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = round(sqrt(13)),
                                   color = 'default')) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::theme_minimal()

## Use nodesize = 1, mtry = 5 to balance between
## optimal mtry and reducing correlation between learners

#### 3. Fit random forest ####

# Fit random forest
density_rf_H_allcovar <- randomForestSRC::rfsrc(formula = total_density ~ ., # formula
                                              data = rf_data, # data
                                              ntree = 1000, # higher number of trees because this is production quality
                                              mtry = 5, # from above decision
                                              nodesize = 1, # from above decision
                                              importance = TRUE, # calculate variable importance
                                              forest = TRUE) # save forest variables

# Save
save(density_rf_H_allcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/rf/H/density/allcovar.RData')

#### 4. Variable importance ####

## This is used to see if any variable should be removed
## (if confidence intervals overlap zero) 
## And what variables should be selected for
## a reduced model ran later

# Calculate variable importance with confidence intervals
var_imp <- randomForestSRC::subsample(obj = density_rf_H_allcovar,
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

## Most important: precipitation seasonality,
## maximum annual temperature, temperature seasonality

# Find minimium depth of each variable across trees
var_select <- randomForestSRC::var.select(object = density_rf_H_allcovar, # random forest object
                                          method = 'md', # minimum depth
                                          ntree = 500, # number of trees
                                          conservative = 'high') # conservative threshold

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

## For reduced covariate random forest: choose three most important
## climate variables + soil % clay, which has low minimum depth,
## suggesting that it provides extra information not captured by
## climate variables

#### 5. Parital effects plots ####

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                               partial.xvar = 'clay',
                                               partial.values = seq(from = min(density_rf_H_allcovar$xvar$clay),
                                                                    to = max(density_rf_H_allcovar$xvar$clay),
                                                                    length.out = 50))

# Partial plot data
clay_data <- randomForestSRC::get.partial.plot.data(clay_partial)

# Partial plot with GAM smooth
ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x, y = clay_data$yhat)) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand

sand_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                               partial.xvar = 'sand',
                                               partial.values = seq(from = min(density_rf_H_allcovar$xvar$sand),
                                                                    to = max(density_rf_H_allcovar$xvar$sand),
                                                                    length.out = 50))

sand_data <- randomForestSRC::get.partial.plot.data(sand_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x, y = sand_data$yhat)) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt

silt_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                               partial.xvar = 'silt',
                                               partial.values = seq(from = min(density_rf_H_allcovar$xvar$silt),
                                                                    to = max(density_rf_H_allcovar$xvar$silt),
                                                                    length.out = 50))

silt_data <- randomForestSRC::get.partial.plot.data(silt_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x, y = silt_data$yhat)) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Calcium carbonate concentration

caco3_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                                partial.xvar = 'caco3',
                                                partial.values = seq(from = min(density_rf_H_allcovar$xvar$caco3),
                                                                     to = max(density_rf_H_allcovar$xvar$caco3),
                                                                     length.out = 50))

caco3_data <- randomForestSRC::get.partial.plot.data(caco3_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x, y = caco3_data$yhat)) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + 
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Available water content

awc_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                              partial.xvar = 'awc',
                                              partial.values = seq(from = min(density_rf_H_allcovar$xvar$awc),
                                                                   to = max(density_rf_H_allcovar$xvar$awc),
                                                                   length.out = 50))

awc_data <- randomForestSRC::get.partial.plot.data(awc_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x, y = awc_data$yhat)) +
  ggplot2::xlab('Soil available water content (cm/cm)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in floodplain

flood_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                                partial.xvar = 'flood',
                                                partial.values = seq(from = min(density_rf_H_allcovar$xvar$flood),
                                                                     to = max(density_rf_H_allcovar$xvar$flood),
                                                                     length.out = 50))

flood_data <- randomForestSRC::get.partial.plot.data(flood_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x, y = flood_data$yhat)) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation

ppt_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                              partial.xvar = 'ppt_sum',
                                              partial.values = seq(from = min(density_rf_H_allcovar$xvar$ppt_sum),
                                                                   to = max(density_rf_H_allcovar$xvar$ppt_sum),
                                                                   length.out = 50))

ppt_data <- randomForestSRC::get.partial.plot.data(ppt_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x, y = ppt_data$yhat)) +
  ggplot2::xlab('Total annual precipitation (mm/year)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature

tmean_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                                partial.xvar = 'tmean_mean',
                                                partial.values = seq(from = min(density_rf_H_allcovar$xvar$tmean_mean),
                                                                     to = max(density_rf_H_allcovar$xvar$tmean_mean),
                                                                     length.out = 50))

tmean_data <- randomForestSRC::get.partial.plot.data(tmean_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x, y = tmean_data$yhat)) +
  ggplot2::xlab('Mean annual temperature (°C)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality

ppt_cv_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                                 partial.xvar = 'ppt_cv',
                                                 partial.values = seq(from = min(density_rf_H_allcovar$xvar$ppt_cv),
                                                                      to = max(density_rf_H_allcovar$xvar$ppt_cv),
                                                                      length.out = 50))

ppt_cv_data <- randomForestSRC::get.partial.plot.data(ppt_cv_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x, y = ppt_cv_data$yhat)) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation (%))') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality

tmean_sd_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                                   partial.xvar = 'tmean_sd',
                                                   partial.values = seq(from = min(density_rf_H_allcovar$xvar$tmean_sd),
                                                                        to = max(density_rf_H_allcovar$xvar$tmean_sd),
                                                                        length.out = 50))

tmean_sd_data <- randomForestSRC::get.partial.plot.data(tmean_sd_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x, y = tmean_sd_data$yhat)) +
  ggplot2::xlab('Temperature seasonality (standard deviation (°C))') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature

tmin_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                               partial.xvar = 'tmin',
                                               partial.values = seq(from = min(density_rf_H_allcovar$xvar$tmin),
                                                                    to = max(density_rf_H_allcovar$xvar$tmin),
                                                                    length.out = 50))

tmin_data <- randomForestSRC::get.partial.plot.data(tmin_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x, y = tmin_data$yhat)) +
  ggplot2::xlab('Minimum annual temperature (°C)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature

tmax_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                               partial.xvar = 'tmax',
                                               partial.values = seq(from = min(density_rf_H_allcovar$xvar$tmax),
                                                                    to = max(density_rf_H_allcovar$xvar$tmax),
                                                                    length.out = 50))

tmax_data <- randomForestSRC::get.partial.plot.data(tmax_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x, y = tmax_data$yhat)) +
  ggplot2::xlab('Maximum annual temperature (°C)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum VPD

vpd_partial <- randomForestSRC::partial.rfsrc(object = density_rf_H_allcovar,
                                              partial.xvar = 'vpdmax',
                                              partial.values = seq(from = min(density_rf_H_allcovar$xvar$vpdmax),
                                                                   to = max(density_rf_H_allcovar$xvar$vpdmax),
                                                                   length.out = 50))

vpd_data <- randomForestSRC::get.partial.plot.data(vpd_partial)

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x, y = vpd_data$yhat)) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

