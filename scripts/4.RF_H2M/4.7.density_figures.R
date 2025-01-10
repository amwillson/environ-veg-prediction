#### STEP 4-7

## Plot out-of-sample predictions from HISTORICAL
## model predicting HISTORICAL and MODERN
## TOTAL STEM DENSITY

## 1. Load historical predictions
## 2. Process historical predictions
## 3. Plot historical predictions
## 4. Plot historical predicted vs observed
## 5. Calculate historical r
## 6. Load modern predictions
## 7. Process modern predictions
## 8. Plot modern predictions
## 9. Plot modern predicted vs observed
## 10. Calculate modern r
## 11. Compare predictions in space
## 12. Compare differences in space
## 13. Compare r

## Input: out/rf/H/density/predicted_historical_rf1.RData
## Dataframe with observed historical out-of-sample vegetation, soil, 
## and climate data as well as predicted historical total
## stem density from main model with soil and climate covariates
## From 4.5.density_historical_predictions.R

## Input: out/rf/H/density/predicted_historical_rf2.RData
## Dataframe with observed historical out-of-sample vegetation, soil,
## and climate data as well as predicted historical total
## stem density from alternate model with only climate covariates
## From 4.5.density_historical_predictions.R

## Input: out/rf/H/density/predicted_historical_rf3.RData
## Dataframe with observed historical out-of-sample vegetation, soil,
## and climate data as well as predicted historical total
## stem density from alternate model with the reduced set
## of covariates
## From 4.5.density_historical_predictions.R

## Input: out/rf/H/density/predicted_historical_rf4.RData
## Dataframe with observed historical out-of-sample vegetation, soil,
## and climate data as well as predicted historical total
## stem density from alternate model with soil and climate covariates
## as well as latitude and longitude of the grid cell
## From 4.5.density_historical_predictions.R

## Input: out/rf/H/density/predicted_modern_rf1.RData
## Dataframe with observed modern out-of-sample vegetation, soil,
## and climate data as well as predicted modern total
## stem density from main model with soil and climate covariates
## From 4.6.density_modern_predictions.R

## Input: out/rf/H/density/predicted_modern_rf2.RData
## Dataframe with observed modern out-of-sample vegetation, soil,
## and climate data as well as predicted modern total stem
## density from alterate model with only climate covariates
## From 4.6.density_modern_predictions.R

## Input: out/rf/H/density/predicted_modern_rf3.RData
## Dataframe with observed modern out-of-sample vegetation, soil,
## and climate data as well as predicted modern total stem
## density from alternate model with the reduced set of covariates
## From 4.6.density_modern_predictions.R

## Input: out/rf/H/density/predicted_modern_rf4.RData
## Dataframe with observed modern out-of-sample vegetation, soil,
## and climate data as well as predicted modern total stem density
## from alternate model with soil and climate covariates as well
## as latitude and longitude of the grid cell
## From 4.6.density_modern_predictions.R

## Output: none except figures saved in figure/ directory

rm(list = ls())

#### 1. Load historical predictions ####

# Load historical predictions from model 1
load('out/rf/H/density/predicted_historical_rf1.RData')
# Load historical predictions from model 2
load('out/rf/H/density/predicted_historical_rf2.RData')
# Load historical predictions from model 3
load('out/rf/H/density/predicted_historical_rf3.RData')
# Load historical predictions from model 4
load('out/rf/H/density/predicted_historical_rf4.RData')

#### 2. Process historical predictions ####

# Select only necessary columns from each dataframe
pred_historical_rf1 <- dplyr::select(pred_historical_rf1,
                                     x, y, total_density,
                                     pred_out_rf1)
pred_historical_rf2 <- dplyr::select(pred_historical_rf2,
                                     x, y, total_density,
                                     pred_out_rf2)
pred_historical_rf3 <- dplyr::select(pred_historical_rf3,
                                     x, y, total_density,
                                     pred_out_rf3)
pred_historical_rf4 <- dplyr::select(pred_historical_rf4,
                                     x, y, total_density,
                                     pred_out_rf4)

# Combine dataframes
pred_historical <- pred_historical_rf1 |>
  dplyr::full_join(y = pred_historical_rf2,
                   by = c('x', 'y', 'total_density')) |>
  dplyr::full_join(y = pred_historical_rf3,
                   by = c('x', 'y', 'total_density')) |>
  dplyr::full_join(y = pred_historical_rf4,
                   by = c('x', 'y', 'total_density'))

# Check that rows combined correctly
nrow(pred_historical) == nrow(pred_historical_rf1) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_rf2) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_rf3) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_rf4) # should be TRUE

# Calculate observed minus predicted
pred_historical <- pred_historical |>
  dplyr::mutate(diff1 = total_density - pred_out_rf1,
                diff2 = total_density - pred_out_rf2,
                diff3 = total_density - pred_out_rf3,
                diff4 = total_density - pred_out_rf4)

#### 3. Plot historical predictions ####

# Map of study region
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot predictions over space with main model
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_out_rf1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1) +
  ggplot2::ggtitle('Predicted historical total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred_space_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with all four models
pred_historical |>
  tidyr::pivot_longer(cols = pred_out_rf1:pred_out_rf4,
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'pred_out_rf1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = predicted)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Historical predictions') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred_space_facets.png',
                height = 15, width = 15, units = 'cm')

#### 4. Plot historical predicted versus observed ####

# Main model scatterplot of predicted versus observed
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = pred_out_rf1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = pred_out_rf1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Predictions of historical total stem density') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10),
                 panel.grid = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA, linewidth = 1))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_predvobs_allcovar.png',
                height = 10, width = 10, units = 'cm')

# All four models
pred_historical |>
  tidyr::pivot_longer(cols = pred_out_rf1:pred_out_rf4,
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'pred_out_rf1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = predicted)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = predicted,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Historical predictions') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_predvobs_facets.png',
                width = 15, height = 15, units = 'cm')

# Plot observed - predicted over space
# Shows spatial distribution of model error
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::ggtitle('Difference between observed and predicted\nhistorical total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred-obs_allcovar.png',
                width = 10, height = 10, units = 'cm')

# All four models
pred_historical |>
  tidyr::pivot_longer(cols = diff1:diff4,
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'diff1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'diff2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'diff3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::ggtitle('Historical predictions') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred-obs_facets.png',
                width = 15, height = 15, units = 'cm')

#### 5. Calculate historical r ####

# Correlation coefficients
corr1 <- cor(x = pred_historical$total_density,
             y = pred_historical$pred_out_rf1)
corr2 <- cor(x = pred_historical$total_density,
             y = pred_historical$pred_out_rf2)
corr3 <- cor(x = pred_historical$total_density,
             y = pred_historical$pred_out_rf3)
corr4 <- cor(x = pred_historical$total_density,
             y = pred_historical$pred_out_rf4)

# Combine
historical_cors <- c(corr1, corr2, corr3, corr4)

# Format
historical_cors <- as.data.frame(historical_cors)
historical_cors$model <- c('Climate + soil', 'Climate only',
                           'Four covariates',
                           'Climate + soil + coordinates')

historical_cors <- dplyr::select(historical_cors,
                                 model, historical_cors)
historical_cors

#### 6. Load modern predictions ####

# Load modern predictions from model 1
load('out/rf/H/density/predicted_modern_rf1.RData')
# Load modern predictions from model 2
load('out/rf/H/density/predicted_modern_rf2.RData')
# Load modern predictions from model 3
load('out/rf/H/density/predicted_modern_rf3.RData')
# Load modern predictions from model 4
load('out/rf/H/density/predicted_modern_rf4.RData')

#### 7. Process modern predictions ####

# Select only necessary columns from each dataframe
pred_modern_rf1 <- dplyr::select(pred_modern_rf1,
                                 x, y, total_density,
                                 pred_out_rf1)
pred_modern_rf2 <- dplyr::select(pred_modern_rf2,
                                 x, y, total_density,
                                 pred_out_rf2)
pred_modern_rf3 <- dplyr::select(pred_modern_rf3,
                                 x, y, total_density,
                                 pred_out_rf3)
pred_modern_rf4 <- dplyr::select(pred_modern_rf4,
                                 x, y, total_density,
                                 pred_out_rf4)

# Combine dataframes
pred_modern <- pred_modern_rf1 |>
  dplyr::full_join(y = pred_modern_rf2,
                   by = c('x', 'y', 'total_density')) |>
  dplyr::full_join(y = pred_modern_rf3,
                   by = c('x', 'y', 'total_density')) |>
  dplyr::full_join(y = pred_modern_rf4,
                   by = c('x', 'y', 'total_density'))

# Check that rows combined correctly
nrow(pred_modern) == nrow(pred_modern_rf1) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_rf2) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_rf3) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_rf4) # should be TRUE

# Calculate observed minus predicted
pred_modern <- pred_modern |>
  dplyr::mutate(diff1 = total_density - pred_out_rf1,
                diff2 = total_density - pred_out_rf2,
                diff3 = total_density - pred_out_rf3,
                diff4 = total_density - pred_out_rf4)

#### 8. Plot modern predictions ####

# Plot predictions over space for main model
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_out_rf1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1) +
  ggplot2::ggtitle('Predicted modern total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred_space_allcovar.png',
                height = 10, width = 10, units = 'cm')

# Plot predictions over space with all models
pred_modern |>
  tidyr::pivot_longer(cols = pred_out_rf1:pred_out_rf4,
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'pred_out_rf1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = predicted)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Modern predictions') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred_space_facets.png',
                height = 15, width = 15, units = 'cm')

#### 9. Plot modern predicted vs observed ####

# Main model scatterplot of predicted versus observed
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = pred_out_rf1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = pred_out_rf1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Predictions of modern total stem density') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10),
                 panel.grid = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA, linewidth = 1))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_predvobs_allcovar.png',
                height = 10, width = 10, units = 'cm')

# All four models
pred_modern |>
  tidyr::pivot_longer(cols = pred_out_rf1:pred_out_rf4,
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'pred_out_rf1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_out_rf4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = predicted)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = predicted,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Modern predictions') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_predvobs_facets.png',
                width = 15, height = 15, units = 'cm')

# Plot observed - predicted over space
# Shows spatial distribution of model error
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::ggtitle('Difference between observed and predicted\nmodern total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred-obs_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot with color legend log transformation
pred_modern |>
  dplyr::mutate(sign = dplyr::if_else(diff1 < 0, -1, 1),
                logdiff1 = log(abs(diff1)),
                diff1_trans = logdiff1 * sign) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1_trans)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'log(Observed-\nPredicted)') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred-obs_allcovar_log.png',
                width = 10, height = 10, units = 'cm')

# Plot with color legend sqrt transformation
pred_modern |>
  dplyr::mutate(sign = dplyr::if_else(diff1 < 0, -1, 1),
                sqrtdiff1 = sqrt(abs(diff1)),
                diff1_trans = sqrtdiff1 * sign) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1_trans)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(Observed-\nPredicted)') +
  ggplot2::ggtitle('Difference between observed and predicted\nmodern total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred-obs_allcovar_sqrt.png',
                width = 10, height = 10, units = 'cm')

# All four models - untransformed
pred_modern |>
  tidyr::pivot_longer(cols = diff1:diff4,
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'diff1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'diff2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'diff3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::ggtitle('Modern predictions') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred-obs_facets.png',
                width = 15, height = 15, units = 'cm')

# Facets with log transformation
pred_modern |>
  tidyr::pivot_longer(cols = diff1:diff4,
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'diff1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'diff2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'diff3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  dplyr::mutate(sign = dplyr::if_else(diff < 0, -1, 1),
                logdiff = log(abs(diff)),
                diff_trans = logdiff * sign) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_trans)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'log(Observed-\nPredicted)') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred-obs_facets_log.png',
                height = 20, width = 20, units = 'cm')

# Facets with sqrt transformation
pred_modern |>
  tidyr::pivot_longer(cols = diff1:diff4,
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'diff1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'diff2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'diff3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  dplyr::mutate(sign = dplyr::if_else(diff < 0, -1, 1),
                sqrtdiff = sqrt(abs(diff)),
                diff_trans = sqrtdiff * sign) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_trans)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(Observed-\nPredicted)') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred-obs_facets_sqrt.png',
                height = 20, width = 20, units = 'cm')

#### 10. Calculate modern r ####

# Correlation coefficients
corr1 <- cor(x = pred_modern$total_density,
             y = pred_modern$pred_out_rf1)
corr2 <- cor(x = pred_modern$total_density,
             y = pred_modern$pred_out_rf2)
corr3 <- cor(x = pred_modern$total_density,
             y = pred_modern$pred_out_rf3)
corr4 <- cor(x = pred_modern$total_density,
             y = pred_modern$pred_out_rf4)

# Combine
modern_cors <- c(corr1, corr2, corr3, corr4)

# Format
modern_cors <- as.data.frame(modern_cors)
modern_cors$model <- c('Climate + soil', 'Climate only',
                       'Four covariates',
                       'Climate + soil + coordinates')
modern_cors <- dplyr::select(modern_cors,
                             model, modern_cors)

modern_cors

#### 11. Compare predictions in space ####

# Rename columns to combine
colnames(pred_historical) <- c('x', 'y', 'total_density',
                               'pred_out_rf1_H', 'pred_out_rf2_H',
                               'pred_out_rf3_H', 'pred_out_rf4_H',
                               'diff1_H', 'diff2_H', 'diff3_H', 'diff4_H')
colnames(pred_modern) <- c('x', 'y', 'total_density',
                           'pred_out_rf1_M', 'pred_out_rf2_M', 
                           'pred_out_rf3_M', 'pred_out_rf4_M',
                           'diff1_M', 'diff2_M', 'diff3_M', 'diff4_M')

# Combine historical and modern dataframes
preds <- pred_historical |>
  dplyr::select(-total_density) |>
  dplyr::full_join(y = dplyr::select(pred_modern, -total_density),
                   by = c('x', 'y'))

# Find difference between predictions between historic and modern
preds <- preds |>
  dplyr::mutate(pred_diff1 = pred_out_rf1_M - pred_out_rf1_H,
                pred_diff2 = pred_out_rf2_M - pred_out_rf2_H,
                pred_diff3 = pred_out_rf3_M - pred_out_rf3_H,
                pred_diff4 = pred_out_rf4_M - pred_out_rf4_H)

# Plot difference in prediction in space with main model
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'stems/ha',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Change in predicted total stem density\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_pred_allcovar.png',
                height = 10, width = 10, units = 'cm')

# Plot difference in prediction in space with all models
preds |>
  tidyr::pivot_longer(cols = pred_diff1:pred_diff4,
                      names_to = 'fit',
                      values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'pred_diff1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_diff2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_diff3',
                                     'Four covariates',
                                     fit),
                fit = dplyr::if_else(fit == 'pred_diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'stems/ha',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Change in predicted total stem density\nfrom historic to modern period') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_pred_facet.png',
                height = 20, width = 20, units = 'cm')

#### 12. Compare differences in space ####

# Calculate change in prediction accuracy
preds <- preds |>
  dplyr::mutate(delta_accuracy1 = abs(diff1_H - diff1_M), # change in accuracy
                delta_accuracy2 = abs(diff2_H - diff2_M),
                delta_accuracy3 = abs(diff3_H - diff3_M),
                delta_accuracy4 = abs(diff4_H - diff4_M),
                hm_worse1 = dplyr::if_else(abs(diff1_H) > abs(diff1_M), 'Historic', 'Modern'), # whether historic or modern prediction is worse
                hm_worse2 = dplyr::if_else(abs(diff2_H) > abs(diff2_M), 'Historic', 'Modern'),
                hm_worse3 = dplyr::if_else(abs(diff3_H) > abs(diff3_M), 'Historic', 'Modern'),
                hm_worse4 = dplyr::if_else(abs(diff4_H) > abs(diff4_M), 'Historic', 'Modern'),
                h_sign1 = dplyr::if_else(diff1_H > 0, 'under', 'over'), # sign of historic prediction accuracy
                h_sign2 = dplyr::if_else(diff2_H > 0, 'under', 'over'),
                h_sign3 = dplyr::if_else(diff3_H > 0, 'under', 'over'),
                h_sign4 = dplyr::if_else(diff4_H > 0, 'under', 'over'),
                m_sign1 = dplyr::if_else(diff1_M > 0, 'under', 'over'),
                m_sign2 = dplyr::if_else(diff2_M > 0, 'under', 'over'),
                m_sign3 = dplyr::if_else(diff3_M > 0, 'under', 'over'),
                m_sign4 = dplyr::if_else(diff4_M > 0, 'under', 'over'))

# Plot absolute change in prediction accuracy with main model
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1) +
  ggplot2::ggtitle('Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_pred-obs_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot change with direction:
# negative = modern worse, positive = historic worse
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse1 == 'Historic', 1, -1),
                delta = delta_accuracy1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'stems/ha') +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_pred-obs_allcovar2.png',
                width = 10, height = 10, units = 'cm')

# Plot change with direction and whether density was over or underpredicted in each period
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse1 == 'Historic', 1, -1),
                delta = delta_accuracy1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'stems/ha') +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_pred-obs_allcovar3.png',
                height = 20, width = 20, units = 'cm')

# Where was historic vs modern prediction worse?
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Whether prediction accuracy is worse\nin historic or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_which_better_accuracy.png',
                width = 10, height = 10, units = 'cm')

# Plot difference in accuracy where historic is worse
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 750)) +
  ggplot2::ggtitle('Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_change_modern_better.png',
                width = 10, height = 10, units = 'cm')

# Plot difference in accuracy where modern is worse
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 750)) +
  ggplot2::ggtitle('Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/comp_change_historic_better.png',
                width = 10, height = 10, units = 'cm')

#### 13. Compare r ####

# Combine and format
both_cors <- cbind(historical_cors, modern_cors$modern_cors)
colnames(both_cors) <- c('model', 'Historical', 'Modern')

# Plot comparison of correlation coefficient
both_cors |>
  tidyr::pivot_longer(cols = Historical:Modern,
                      names_to = 'period',
                      values_to = 'correlation') |>
  dplyr::mutate(model = dplyr::if_else(model == 'Climate + soil + coordinates',
                                       'Climate + soil +\ncoordinates',
                                       model)) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = model, y = correlation,
                                 fill = period),
                    stat = 'identity', 
                    position = ggplot2::position_dodge(width = 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation coefficient') +
  ggplot2::scale_fill_discrete(name = 'Prediction period') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/pred_corr_comparison.png',
                height = 8, width = 16, units = 'cm')