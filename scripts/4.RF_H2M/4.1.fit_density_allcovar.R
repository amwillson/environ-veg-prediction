#### STEP 4-1

## Univariate random forest fit to HISTORICAL 
## TOTAL STEM DENSITY and CLIMATE and SOIL covariates

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

## Output: /Volumes/FileBackup/SDM_bigdata/out/rf/H/density/allcovar.RData
## Fitted random forest object saved to external hard drive
## Used in 4.5.density_historical_predictions.R,
## 4.6.density_modern_predictions.R

## Figures of partial effects plots also saved to figures/ directory

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

# Find minimum depth of each variable across trees
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

## For reduced covariate random forest: choose precipitation seasonality,
## maximum annual temperature, total annual precipitaiton
## + soil % clay, which has low minimum depth,
## suggesting that it provides extra information not captured by
## climate variables
## Dropping temperature seasonality because of strong
## correlations with other covariates

#### 5. Parital effects plots ####

## Soil % clay
# Partial effect
clay_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                               xvar.names = 'clay',
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
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % sand
sand_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                               xvar.names = 'sand',
                                               partial = TRUE)

sand_data <- sand_partial$pData$sand

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x.uniq, y = sand_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x.uniq,
                                    ymin = sand_data$yhat - sand_data$yhat.se,
                                    ymax = sand_data$yhat + sand_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_sand.png',
                height = 8, width = 9.5, units = 'cm')

## Soil % silt
silt_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                               xvar.names = 'silt',
                                               partial = TRUE)

silt_data <- silt_partial$pData$silt

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x.uniq, y = silt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x.uniq,
                                    ymin = silt_data$yhat - silt_data$yhat.se,
                                    ymax = silt_data$yhat + silt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_silt.png',
                height = 8, width = 9.5, units = 'cm')

## Calcium carbonate concentration
caco3_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                                xvar.names = 'caco3',
                                                partial = TRUE)

caco3_data <- caco3_partial$pData$caco3

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x.uniq, y = caco3_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x.uniq,
                                    ymin = caco3_data$yhat - caco3_data$yhat.se,
                                    ymax = caco3_data$yhat + caco3_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + 
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_caco3.png',
                height = 8, width = 9.5, units = 'cm')

## Available water content
awc_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                              xvar.names = 'awc',
                                              partial = TRUE)

awc_data <- awc_partial$pData$awc

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x.uniq, y = awc_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x.uniq,
                                    ymin = awc_data$yhat - awc_data$yhat.se,
                                    ymax = awc_data$yhat + awc_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_awc.png',
                height = 8, width = 9.5, units = 'cm')

## % of grid cell in floodplain
flood_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                                xvar.names = 'flood',
                                                partial = TRUE)

flood_data <- flood_partial$pData$flood

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x.uniq, y = flood_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x.uniq,
                                    ymin = flood_data$yhat - flood_data$yhat.se,
                                    ymax = flood_data$yhat + flood_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

## Total annual precipitation
ppt_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                              xvar.names = 'ppt_sum',
                                              partial = TRUE)

ppt_data <- ppt_partial$pData$ppt_sum

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x.uniq, y = ppt_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x.uniq,
                                    ymin = ppt_data$yhat - ppt_data$yhat.se,
                                    ymax = ppt_data$yhat + ppt_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

## Mean annual temperature
tmean_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                                xvar.names = 'tmean_mean',
                                                partial = TRUE)

tmean_data <- tmean_partial$pData$tmean_mean

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x.uniq, y = tmean_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x.uniq,
                                    ymin = tmean_data$yhat - tmean_data$yhat.se,
                                    ymax = tmean_data$yhat + tmean_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

## Precipitation seasonality
ppt_cv_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                                 xvar.names = 'ppt_cv',
                                                 partial = TRUE)

ppt_cv_data <- ppt_cv_partial$pData$ppt_cv

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x.uniq, y = ppt_cv_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x.uniq,
                                    ymin = ppt_cv_data$yhat - ppt_cv_data$yhat.se,
                                    ymax = ppt_cv_data$yhat + ppt_cv_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (coefficient of variation)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

## Temperature seasonality
tmean_sd_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                                   xvar.names = 'tmean_sd',
                                                   partial = TRUE)

tmean_sd_data <- tmean_sd_partial$pData$tmean_sd

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x.uniq, y = tmean_sd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x.uniq,
                                    ymin = tmean_sd_data$yhat - tmean_sd_data$yhat.se,
                                    ymax = tmean_sd_data$yhat + tmean_sd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (standard deviation (°C))') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

## Minimum annual temperature
tmin_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                               xvar.names = 'tmin',
                                               partial = TRUE)

tmin_data <- tmin_partial$pData$tmin

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmin_data$x.uniq, y = tmin_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x.uniq,
                                    ymin = tmin_data$yhat - tmin_data$yhat.se,
                                    ymax = tmin_data$yhat + tmin_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_tmin.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum annual temperature
tmax_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                               xvar.names = 'tmax',
                                               partial = TRUE)

tmax_data <- tmax_partial$pData$tmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x.uniq, y = tmax_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x.uniq,
                                    ymin = tmax_data$yhat - tmax_data$yhat.se,
                                    ymax = tmax_data$yhat + tmax_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_tmax.png',
                height = 8, width = 9.5, units = 'cm')

## Maximum VPD
vpd_partial <- randomForestSRC::plot.variable(x = density_rf_H_allcovar,
                                              xvar.names = 'vpdmax',
                                              partial = TRUE)

vpd_data <- vpd_partial$pData$vpdmax

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x.uniq, y = vpd_data$yhat)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x.uniq,
                                    ymin = vpd_data$yhat - vpd_data$yhat.se,
                                    ymax = vpd_data$yhat + vpd_data$yhat.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') +
  ggplot2::ylab('Predicted total stem density (stems/ha)') +
  ggplot2::ylim(c(93, 218)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/fit/all_covar_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

