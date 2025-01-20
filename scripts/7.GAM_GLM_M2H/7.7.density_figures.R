#### STEP 7-7

## Plot out-of-sample predictions from MODERN
## model predicting HISTORICAL and MODERN
## TOTAL STEM DENSITY

## 1. Load historical predictions
## 2. Process historical predictions
## 3. Plot historical predictions
## 4. Plot historical predicte vs observed
## 5. Calculate historical r
## 6. Load modern predictions
## 7. Process modern predictions
## 8. Plot modern predictions
## 9. Plot modern predicted vs observed
## 10. Calculate modern r
## 11. Compare predictions in space
## 12. Compare differences in space
## 13. Compare r

## Input: out/gam/M/density/predicted_historical_gam1.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the main model with climate and soil covariates
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam2.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the alternate model with only climate covariates
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam3.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the alternate model with only the reduced set of covariates
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam4.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample
## grid cells using the alternate model with the soil and climate
## covariates as well as the latitude and the longitude of the grid cell
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam1_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample
## grid cells using the main model with climate and soil covariates
## with lower maximum basis dimensionality
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam2_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample
## grid cells using the alternate model with only climate covariates
## with lower maximum basis dimensionality
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam3_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample
## grid cells using the alternate model with only the reduced set of covariates
## with lower maximum basis dimensionality
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_historical_gam4_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample
## grid cells using the alternate model with the soil and climate
## covariates as well as the latitude and the longitude of the grid cell
## From 7.5.density_historical_predictions.R

## Input: out/gam/M/density/predicted_modern_gam1.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample 
## grid cells using the main model with climate and soil covariates
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam2.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample 
## grid cells using the alternate model with only climate covariates
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam3.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample 
## grid cells using the alternate model with only the reduced set of covariates
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam4.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with the soil and climate
## covariates as well as the latitude and the longitude of the grid cell
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam1_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the main model with climate and soil covariates
## with lower maximum basis dimensionality
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam2_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with only climate covariates
## with lower maximum basis dimensionality
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam3_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with only the reduced set of covariates
## with lower maximum basis dimensionality
## From 7.6.density_modern_predictions.R

## Input: out/gam/M/density/predicted_modern_gam4_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with the soil and climate
## covariates as well as the latitude and the longitude of the grid cell
## From 7.6.density_modern_predictions.R

## Output: none except figures saved in figure/ directory

rm(list = ls())

#### 1. Load historical preictions ####

# Load historical predictions from model 1
load('out/gam/M/density/predicted_historical_gam1.RData')
# Load historical predictions from model 2
load('out/gam/M/density/predicted_historical_gam2.RData')
# Load historical predictions from model 3
load('out/gam/M/density/predicted_historical_gam3.RData')
# Load historical predictions from model 4
load('out/gam/M/density/predicted_historical_gam4.RData')

# Load historical predictions from model 1 with lower basis dimensionality
load('out/gam/M/density/predicted_historical_gam1_4k.RData')
# Load historical predictions from model 2 with lower basis dimensionality
load('out/gam/M/density/predicted_historical_gam2_4k.RData')
# Load historical predictions from model 3 with lower basis dimensionality
load('out/gam/M/density/predicted_historical_gam3_4k.RData')
# Load historical predictions from model 4 with lower basis dimensionaltiy
load('out/gam/M/density/predicted_historical_gam4_4k.RData')

#### 2. Process historical predictions ####

# Select only necessary columns from each dataframe
pred_historical_gam1 <- dplyr::select(pred_historical_gam1,
                                      x_coord, y_coord, 
                                      total_density,
                                      estimate_gam1,
                                      conf.low_gam1, 
                                      conf.high_gam1)
pred_historical_gam2 <- dplyr::select(pred_historical_gam2,
                                      x_coord, y_coord, 
                                      total_density,
                                      estimate_gam2,
                                      conf.low_gam2, 
                                      conf.high_gam2)
pred_historical_gam3 <- dplyr::select(pred_historical_gam3,
                                      x_coord, y_coord, 
                                      total_density,
                                      estimate_gam3,
                                      conf.low_gam3, 
                                      conf.high_gam3)
pred_historical_gam4 <- dplyr::select(pred_historical_gam4,
                                      x_coord, y_coord,
                                      total_density,
                                      estimate_gam4,
                                      conf.low_gam4,
                                      conf.high_gam4)
pred_historical_gam1_4k <- dplyr::select(pred_historical_gam1_4k,
                                         x_coord, y_coord, 
                                         total_density,
                                         estimate_gam1_4k,
                                         conf.low_gam1_4k, 
                                         conf.high_gam1_4k)
pred_historical_gam2_4k <- dplyr::select(pred_historical_gam2_4k,
                                         x_coord, y_coord, 
                                         total_density,
                                         estimate_gam2_4k,
                                         conf.low_gam2_4k, 
                                         conf.high_gam2_4k)
pred_historical_gam3_4k <- dplyr::select(pred_historical_gam3_4k,
                                         x_coord, y_coord, 
                                         total_density,
                                         estimate_gam3_4k,
                                         conf.low_gam3_4k, 
                                         conf.high_gam3_4k)
pred_historical_gam4_4k <- dplyr::select(pred_historical_gam4_4k,
                                         x_coord, y_coord,
                                         total_density,
                                         estimate_gam4_4k,
                                         conf.low_gam4_4k,
                                         conf.high_gam4_4k)

# Combine dataframes for models fit with k = 10
pred_historical <- pred_historical_gam1 |>
  dplyr::full_join(y = pred_historical_gam2,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_historical_gam3,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_historical_gam4,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  # Rename coordinate columns
  dplyr::rename(x = x_coord,
                y = y_coord)

# Combine dataframes for models fit with k = 5
pred_historical_4k <- pred_historical_gam1_4k |>
  dplyr::full_join(y = pred_historical_gam2_4k,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_historical_gam3_4k,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_historical_gam4_4k,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  # Rename coordinate columns
  dplyr::rename(x = x_coord,
                y = y_coord)

# Check that rows combined correctly
nrow(pred_historical) == nrow(pred_historical_gam1) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_gam2) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_gam3) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_gam4) # should be TRUE
nrow(pred_historical_4k) == nrow(pred_historical_gam1_4k) # should be TRUE
nrow(pred_historical_4k) == nrow(pred_historical_gam2_4k) # should be TRUE
nrow(pred_historical_4k) == nrow(pred_historical_gam3_4k) # should be TRUE
nrow(pred_historical_4k) == nrow(pred_historical_gam4_4k) # should be TRUE

# Calculate observed minus predicted with k = 10
pred_historical <- pred_historical |>
  dplyr::mutate(diff1 = total_density - estimate_gam1,
                diff2 = total_density - estimate_gam2,
                diff3 = total_density - estimate_gam3,
                diff4 = total_density - estimate_gam4)

# Calculate obesrved minus predicted with k = 5
pred_historical_4k <- pred_historical_4k |>
  dplyr::mutate(diff1 = total_density - estimate_gam1_4k,
                diff2 = total_density - estimate_gam2_4k,
                diff3 = total_density - estimate_gam3_4k,
                diff4 = total_density - estimate_gam4_4k)

#### 3. Plot historical predictions ####

# Map of study region
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot predictions over space with main model
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred_space_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with main model with lower basis dimension
pred_historical_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam1_4k)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred_space_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with uncorrelated model
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred_space_redcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with main model with lower basis dimension
pred_historical_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam3_4k)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred_space_redcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with all four models
pred_historical |>
  tidyr::pivot_longer(cols = c(estimate_gam1, estimate_gam2, 
                               estimate_gam3, estimate_gam4),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = predicted)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred_space_facets.png',
                height = 20, width = 20, units = 'cm')

# Plot predictions over space with all four models with lower basis dimension
pred_historical_4k |>
  tidyr::pivot_longer(cols = c(estimate_gam1_4k,
                               estimate_gam2_4k,
                               estimate_gam3_4k,
                               estimate_gam4_4k),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1_4k',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2_4k',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3_4k',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4_4k',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = predicted)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred_space_facets_4k.png',
                height = 20, width = 20, units = 'cm')

#### 4. Plot historical predicted versus observed ####

# Main model scatterplot of predicted versus observed
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_predvobs_allcovar.png',
                height = 10, width = 10, units = 'cm')

# Main model scatterplot of predicted versus observed with lower basis dimensionality
pred_historical_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam1_4k)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam1_4k,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_predvobs_allcovar_4k.png',
                height = 10, width = 10, units = 'cm')

# Model with uncorrelated covariates
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam3)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam3,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_predvobs_redcovar.png',
                height = 10, width = 10, units = 'cm')

# Uncorrelated covariates model with lower basis dimensionality
pred_historical_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam3_4k)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam3_4k,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_predvobs_redcovar_4k.png',
                height = 10, width = 10, units = 'cm')

# All four models
pred_historical |>
  tidyr::pivot_longer(cols = c(estimate_gam1,
                               estimate_gam2,
                               estimate_gam3,
                               estimate_gam4),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4',
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
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_predvobs_facets.png',
                width = 20, height = 20, units = 'cm')

# All four models with lower basis dimensionality
pred_historical_4k |>
  tidyr::pivot_longer(cols = c(estimate_gam1_4k,
                               estimate_gam2_4k,
                               estimate_gam3_4k,
                               estimate_gam4_4k),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1_4k',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2_4k',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3_4k',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4_4k',
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
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_predvobs_facets_4k.png',
                width = 20, height = 20, units = 'cm')

# Plot observed - predicted over space
# Shows spatial distribution of model error
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred-obs_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot observed - predicted with lower basis dimension
pred_historical_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred-obs_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Model with uncorrelated covariates
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred-obs_redcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot observed - predicted with lower basis dimension
pred_historical_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(Observed-\nPredicted)') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred-obs_redcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# All three models
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
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred-obs_facets.png',
                width = 20, height = 20, units = 'cm')

# All three models with lower basis dimension
pred_historical_4k |>
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
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2h_pred-obs_facets_4k.png',
                width = 20, height = 20, units = 'cm')

#### 5. Calculate historical r ####

# Correlation coefficients
corr1 <- cor(x = pred_historical$total_density,
             y = pred_historical$estimate_gam1)
corr2 <- cor(x = pred_historical$total_density,
             y = pred_historical$estimate_gam2)
corr3 <- cor(x = pred_historical$total_density,
             y = pred_historical$estimate_gam3)
corr4 <- cor(x = pred_historical$total_density,
             y = pred_historical$estimate_gam4)

corr1_4k <- cor(x = pred_historical_4k$total_density,
                y = pred_historical_4k$estimate_gam1_4k)
corr2_4k <- cor(x = pred_historical_4k$total_density,
                y = pred_historical_4k$estimate_gam2_4k)
corr3_4k <- cor(x = pred_historical_4k$total_density,
                y = pred_historical_4k$estimate_gam3_4k)
corr4_4k <- cor(x = pred_historical_4k$total_density,
                y = pred_historical_4k$estimate_gam4_4k)

# Combine
historical_cors <- c(corr1, corr2, corr3, corr4)
historical_cors_4k <- c(corr1_4k, corr2_4k, corr3_4k, corr4_4k)

# Format
historical_cors <- as.data.frame(historical_cors)
historical_cors_4k <- as.data.frame(historical_cors_4k)

historical_cors$model <- historical_cors_4k$model <-
  c('Climate + soil', 'Climate only', 
    'Uncorrelated covariates', 'Climate + soil + coordinates')

historical_cors <- dplyr::select(historical_cors,
                                 model, historical_cors)
historical_cors_4k <- dplyr::select(historical_cors_4k,
                                    model, historical_cors_4k)

historical_cors
historical_cors_4k

#### 6. Load modern predictions ####

# Load modern predictions from model 1
load('out/gam/M/density/predicted_modern_gam1.RData')
# Load modern predictions from model 2
load('out/gam/M/density/predicted_modern_gam2.RData')
# Load modern predictions from model 3
load('out/gam/M/density/predicted_modern_gam3.RData')
# Load modern predictions from model 4
load('out/gam/M/density/predicted_modern_gam4.RData')

# Load modern predictions from model 1 with lower basis dimension
load('out/gam/M/density/predicted_modern_gam1_4k.RData')
# Load modern predictions from model 2 with lower basis dimension
load('out/gam/M/density/predicted_modern_gam2_4k.RData')
# Load modern predictions from model 3 with lower basis dimension
load('out/gam/M/density/predicted_modern_gam3_4k.RData')
# Load modern predictions from model 4 with lower basis dimension
load('out/gam/M/density/predicted_modern_gam4_4k.RData')

#### 7. Process modern predictions ####

# Select only necessary columns from each dataframe
pred_modern_gam1 <- dplyr::select(pred_modern_gam1,
                                  x_coord, y_coord, 
                                  total_density,
                                  estimate_gam1,
                                  conf.low_gam1,
                                  conf.high_gam1)
pred_modern_gam2 <- dplyr::select(pred_modern_gam2,
                                  x_coord, y_coord, 
                                  total_density,
                                  estimate_gam2,
                                  conf.low_gam2,
                                  conf.high_gam2)
pred_modern_gam3 <- dplyr::select(pred_modern_gam3,
                                  x_coord, y_coord, 
                                  total_density,
                                  estimate_gam3,
                                  conf.low_gam3,
                                  conf.high_gam3)
pred_modern_gam4 <- dplyr::select(pred_modern_gam4,
                                  x_coord, y_coord,
                                  total_density,
                                  estimate_gam4,
                                  conf.low_gam4,
                                  conf.high_gam4)

pred_modern_gam1_4k <- dplyr::select(pred_modern_gam1_4k,
                                     x_coord, y_coord, 
                                     total_density,
                                     estimate_gam1_4k,
                                     conf.low_gam1_4k,
                                     conf.high_gam1_4k)
pred_modern_gam2_4k <- dplyr::select(pred_modern_gam2_4k,
                                     x_coord, y_coord, 
                                     total_density,
                                     estimate_gam2_4k,
                                     conf.low_gam2_4k,
                                     conf.high_gam2_4k)
pred_modern_gam3_4k <- dplyr::select(pred_modern_gam3_4k,
                                     x_coord, y_coord, 
                                     total_density,
                                     estimate_gam3_4k,
                                     conf.low_gam3_4k,
                                     conf.high_gam3_4k)
pred_modern_gam4_4k <- dplyr::select(pred_modern_gam4_4k,
                                     x_coord, y_coord,
                                     total_density,
                                     estimate_gam4_4k,
                                     conf.low_gam4_4k,
                                     conf.high_gam4_4k)

# Combine dataframes for models fit with k = 10
pred_modern <- pred_modern_gam1 |>
  dplyr::full_join(y = pred_modern_gam2,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_modern_gam3,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_modern_gam4,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  # Rename coordinate columns
  dplyr::rename(x = x_coord,
                y = y_coord)

# Combine dataframes for models fit with k = 5
pred_modern_4k <- pred_modern_gam1_4k |>
  dplyr::full_join(y = pred_modern_gam2_4k,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_modern_gam3_4k,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  dplyr::full_join(y = pred_modern_gam4_4k,
                   by = c('x_coord', 'y_coord', 'total_density')) |>
  # Rename coordinate columns
  dplyr::rename(x = x_coord,
                y = y_coord)

# Check that rows combined correctly
nrow(pred_modern) == nrow(pred_modern_gam1) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_gam2) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_gam3) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_gam4) # should be TRUE
nrow(pred_modern_4k) == nrow(pred_modern_gam1_4k) # should be TRUE
nrow(pred_modern_4k) == nrow(pred_modern_gam2_4k) # should be TRUE
nrow(pred_modern_4k) == nrow(pred_modern_gam3_4k) # should be TRUE
nrow(pred_modern_4k) == nrow(pred_modern_gam4_4k) # should be TRUE

# Calculate observed minus predicted with k = 10
pred_modern <- pred_modern |>
  dplyr::mutate(diff1 = total_density - estimate_gam1,
                diff2 = total_density - estimate_gam2,
                diff3 = total_density - estimate_gam3,
                diff4 = total_density - estimate_gam4)

# Calculate observed minus predicted with k = 5
pred_modern_4k <- pred_modern_4k |>
  dplyr::mutate(diff1 = total_density - estimate_gam1_4k,
                diff2 = total_density - estimate_gam2_4k,
                diff3 = total_density - estimate_gam3_4k,
                diff4 = total_density - estimate_gam4_4k)

#### 8. Plot modern predictions ####

# Plot predictions over space with main model
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred_space_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with main model with lower basis dimension
pred_modern_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam1_4k)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred_space_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with model with uncorrelated covariates
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred_space_redcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with main model with lower basis dimension
pred_modern_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate_gam3_4k)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred_space_redcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot predictions over space with all four models
pred_modern |>
  tidyr::pivot_longer(cols = c(estimate_gam1, estimate_gam2, 
                               estimate_gam3, estimate_gam4),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = predicted)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred_space_facets.png',
                height = 20, width = 20, units = 'cm')

# Plot predictions over space with all three models with lower basis dimension
pred_modern_4k |>
  tidyr::pivot_longer(cols = c(estimate_gam1_4k,
                               estimate_gam2_4k,
                               estimate_gam3_4k,
                               estimate_gam4_4k),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1_4k',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2_4k',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3_4k',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4_4k',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = predicted)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Total stem\ndensity',
                                palette = 'Greens',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred_space_facets_4k.png',
                height = 20, width = 20, units = 'cm')

#### 9. Plot modern predicted versus observed ####

# Main model scatterplot of predicted versus observed
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_predvobs_allcovar.png',
                height = 10, width = 10, units = 'cm')

# Main model scatterplot of predicted versus observed with lower basis dimensionality
pred_modern_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam1_4k)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam1_4k,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_predvobs_allcovar_4k.png',
                height = 10, width = 10, units = 'cm')

# Model with uncorrelated covariates scatterplot of predicted versus observed
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam3)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam3,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_predvobs_redcovar.png',
                height = 10, width = 10, units = 'cm')

# Main model scatterplot of predicted versus observed with lower basis dimensionality
pred_modern_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_density, y = estimate_gam3_4k)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_density, y = estimate_gam3_4k,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_predvobs_redcovar_4k.png',
                height = 10, width = 10, units = 'cm')

# All four models
pred_modern |>
  tidyr::pivot_longer(cols = c(estimate_gam1,
                               estimate_gam2,
                               estimate_gam3,
                               estimate_gam4),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4',
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
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_predvobs_facets.png',
                width = 20, height = 20, units = 'cm')

# All four models with lower basis dimensionality
pred_modern_4k |>
  tidyr::pivot_longer(cols = c(estimate_gam1_4k,
                               estimate_gam2_4k,
                               estimate_gam3_4k,
                               estimate_gam4_4k),
                      names_to = 'fit',
                      values_to = 'predicted') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'estimate_gam1_4k',
                                     'Climate + soil',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam2_4k',
                                     'Climate only',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam3_4k',
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'estimate_gam4_4k',
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
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_predvobs_facets_4k.png',
                width = 20, height = 20, units = 'cm')

# Plot observed - predicted over space
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred-obs_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot observed - predicted with lower basis dimension
pred_modern_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred-obs_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot observed - predicted over space with model with uncorrelated covariates
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred-obs_redcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot observed - predicted with lower basis dimension
pred_modern_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred-obs_redcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# All four models
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
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred-obs_facets.png',
                width = 20, height = 20, units = 'cm')

# All four models with lower basis dimension
pred_modern_4k |>
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
                                     'Uncorrelated\ncovariates',
                                     fit),
                fit = dplyr::if_else(fit == 'diff4',
                                     'Climate + soil +\ncoordinates',
                                     fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/m2m_pred-obs_facets_4k.png',
                width = 20, height = 20, units = 'cm')

#### 10. Calculate modern r ####

# Correlation coefficients
corr1 <- cor(x = pred_modern$total_density,
             y = pred_modern$estimate_gam1)
corr2 <- cor(x = pred_modern$total_density,
             y = pred_modern$estimate_gam2)
corr3 <- cor(x = pred_modern$total_density,
             y = pred_modern$estimate_gam3)
corr4 <- cor(x = pred_modern$total_density,
             y = pred_modern$estimate_gam4)

corr1_4k <- cor(x = pred_modern_4k$total_density,
                y = pred_modern_4k$estimate_gam1_4k)
corr2_4k <- cor(x = pred_modern_4k$total_density,
                y = pred_modern_4k$estimate_gam2_4k)
corr3_4k <- cor(x = pred_modern_4k$total_density,
                y = pred_modern_4k$estimate_gam3_4k)
corr4_4k <- cor(x = pred_modern_4k$total_density,
                y = pred_modern_4k$estimate_gam4_4k)

# Combine
modern_cors <- c(corr1, corr2, corr3, corr4)
modern_cors_4k <- c(corr1_4k, corr2_4k, corr3_4k, corr4_4k)

# Format
modern_cors <- as.data.frame(modern_cors)
modern_cors_4k <- as.data.frame(modern_cors_4k)

modern_cors$model <- modern_cors_4k$model <-
  c('Climate + soil', 'Climate only', 
    'Uncorrelated covariates',
    'Climate + soil + coordinates')

modern_cors <- dplyr::select(modern_cors,
                             model, modern_cors)
modern_cors_4k <- dplyr::select(modern_cors_4k,
                                model, modern_cors_4k)

modern_cors
modern_cors_4k

#### 11. Compare predictions in space ####

# Remove confidence intervals
pred_historical <- dplyr::select(pred_historical,
                                 x, y,
                                 estimate_gam1,
                                 estimate_gam2,
                                 estimate_gam3,
                                 estimate_gam4,
                                 diff1:diff4)
pred_historical_4k <- dplyr::select(pred_historical_4k,
                                    x, y,
                                    estimate_gam1_4k,
                                    estimate_gam2_4k,
                                    estimate_gam3_4k,
                                    estimate_gam4_4k,
                                    diff1:diff4)
pred_modern <- dplyr::select(pred_modern,
                             x, y,
                             estimate_gam1,
                             estimate_gam2,
                             estimate_gam3,
                             estimate_gam4,
                             diff1:diff4)
pred_modern_4k <- dplyr::select(pred_modern_4k,
                                x, y,
                                estimate_gam1_4k,
                                estimate_gam2_4k,
                                estimate_gam3_4k,
                                estimate_gam4_4k,
                                diff1:diff4)

# Rename columns to combine
colnames(pred_historical) <- c('x', 'y', 
                               'estimate_gam1_H', 'estimate_gam2_H', 
                               'estimate_gam3_H', 'estimate_gam4_H',
                               'diff1_H', 'diff2_H', 
                               'diff3_H', 'diff4_H')
colnames(pred_historical_4k) <- c('x', 'y', 
                                  'estimate_gam1_4k_H', 'estimate_gam2_4k_H', 
                                  'estimate_gam3_4k_H', 'estimate_gam4_4k_H',
                                  'diff1_H', 'diff2_H', 
                                  'diff3_H', 'diff4_H')
colnames(pred_modern) <- c('x', 'y', 
                           'estimate_gam1_M', 'estimate_gam2_M', 
                           'estimate_gam3_M', 'estimate_gam4_M',
                           'diff1_M', 'diff2_M', 
                           'diff3_M', 'diff4_M')
colnames(pred_modern_4k) <- c('x', 'y', 
                              'estimate_gam1_4k_M', 'estimate_gam2_4k_M', 
                              'estimate_gam3_4k_M', 'estimate_gam4_4k_M',
                              'diff1_M', 'diff2_M', 
                              'diff3_M', 'diff4_M')

# Combine historical and modern with higher dimensional basis function
preds <- pred_historical |>
  dplyr::full_join(y = pred_modern, by = c('x', 'y'))

# Combine historical and modern with lower dimensional basis function
preds_4k <- pred_historical_4k |>
  dplyr::full_join(y = pred_modern_4k, by = c('x', 'y'))

# Find difference between predictions between historic and modern
preds <- preds |>
  dplyr::mutate(pred_diff1 = estimate_gam1_M - estimate_gam1_H,
                pred_diff2 = estimate_gam2_M - estimate_gam2_H,
                pred_diff3 = estimate_gam3_M - estimate_gam3_H,
                pred_diff4 = estimate_gam4_M - estimate_gam4_H)
preds_4k <- preds_4k |>
  dplyr::mutate(pred_diff1 = estimate_gam1_4k_M - estimate_gam1_4k_H,
                pred_diff2 = estimate_gam2_4k_M - estimate_gam2_4k_H,
                pred_diff3 = estimate_gam3_4k_M - estimate_gam3_4k_H,
                pred_diff4 = estimate_gam4_4k_M - estimate_gam4_4k_H)

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
                filename = 'figures/gam/M/density/pred/comp_pred_allcovar.png',
                height = 10, width = 10, units = 'cm')

# Plot difference in prediction in space with main model with lower basis dimensionality
preds_4k |>
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
                filename = 'figures/gam/M/density/pred/comp_pred_allcovar_4k.png',
                height = 10, width = 10, units = 'cm')

# Plot difference in prediction in space with model with uncorrelated covariates
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff3)) +
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
                filename = 'figures/gam/M/density/pred/comp_pred_redcovar.png',
                height = 10, width = 10, units = 'cm')

# Plot difference in prediction in space with model with uncorrelated covariates
# with lower basis dimensionality
preds_4k |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff3)) +
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
                filename = 'figures/gam/M/density/pred/comp_pred_redcovar_4k.png',
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
                                     'Uncorrelated\ncovariates',
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
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Change in predicted total stem density\nfrom historic to modern period') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred_facet.png',
                height = 20, width = 20, units = 'cm')

# Plot difference in prediction in space with all models with lower basis dimensionality
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
                                     'Uncorrelated\ncovariates',
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
  ggplot2::facet_wrap(~fit, nrow = 2) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Change in predicted total stem density\nfrom historic to modern period') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred_facet_4k.png',
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

preds_4k <- preds_4k |>
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
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot absoltue change in prediction acuracy with main model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot absolute change in prediction accuracy with model with uncorrelated covariates
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_redcovar.png',
                width = 10, height = 10, units = 'cm')

# Plot absoltue change in prediction acuracy with model with lower basis dimension
# with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_redcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot change in direction:
# negative = modern worse, positive = historic worse
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse1 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy1),
                delta = sqrtdelta * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern\nNegative = better prediction in historic') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_allcovar2.png',
                width = 10, height = 10, units = 'cm')

# Plot change in direction with model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse1 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy1),
                delta = sqrtdelta * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern\nNegative = better prediction in historic') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_allcovar2_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot change in direction with model with uncorrelated covariates
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse3 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy3),
                delta = sqrtdelta * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern\nNegative = better prediction in historic') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_redcovar2.png',
                width = 10, height = 10, units = 'cm')

# Plot change in direction with model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse3 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy3),
                delta = sqrtdelta * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern\nNegative = better prediction in historic') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_redcovar2_4k.png',
                width = 10, height = 10, units = 'cm')

# Plot change with direction and whether density was over or underpredicted in each period
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse1 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy1),
                delta = sqrtdelta * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern period\nNegative = better prediction in historic period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_allcovar3.png',
                height = 20, width = 20, units = 'cm')

# Plot change with direction and whether density was over or underpredicted in each period
# with model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse1 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy1),
                delta = sqrtdelta * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern period\nNegative = better prediction in historic period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_allcovar3_4k.png',
                height = 20, width = 20, units = 'cm')

# Plot change with direction and whether density was over or underpredicted in each period
# with model with uncorrelated covariates
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse3 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy3),
                delta = sqrtdelta * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign3 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign3 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern period\nNegative = better prediction in historic period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_redcovar3.png',
                height = 20, width = 20, units = 'cm')

# Plot change with direction and whether density was over or underpredicted in each period
# with model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse3 == 'Historic', 1, -1),
                sqrtdelta = sqrt(delta_accuracy3),
                delta = sqrtdelta * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign3 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign3 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'sqrt(stems/ha)') +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Change in prediction accuracy from historic to modern period',
                   subtitle = 'Positive = better prediction in modern period\nNegative = better prediction in historic period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_pred-obs_redcovar3_4k.png',
                height = 20, width = 20, units = 'cm')

# Where ws historic vs modern prediction worse?
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
                filename = 'figures/gam/M/density/pred/comp_which_better_accuracy_allcovar.png',
                width = 10, height = 10, units = 'cm')

# Where is historic vs modern prediction worse with lower basis dimension model?
preds_4k |>
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
                filename = 'figures/gam/M/density/pred/comp_which_better_accuracy_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# Where is historic vs modern prediction worse with model with uncorrelated covariates?
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Whether prediction accuracy is worse\nin historic or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_which_better_accuracy_redcovar.png',
                width = 10, height = 10, units = 'cm')

# With model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Whether prediction accuracy is worse\nin historic or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_which_better_accuracy_redcovar_4k.png',
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
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_modern_better_allcovar.png',
                width = 10, height = 10, units = 'cm')

# With model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_modern_better_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# With model with uncorrelated covariates
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse3 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_modern_better_redcovar.png',
                width = 10, height = 10, units = 'cm')

# With model with uncorrelated covariates and lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse3 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_modern_better_redcovar_4k.png',
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
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_historic_better_allcovar.png',
                width = 10, height = 10, units = 'cm')

# With model with lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_historic_better_allcovar_4k.png',
                width = 10, height = 10, units = 'cm')

# With model with uncorrelated covariates
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse3 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_historic_better_redcovar.png',
                width = 10, height = 10, units = 'cm')

# With model with uncorrelated covariates and lower basis dimension
preds_4k |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse3 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Reds',
                                direction = 1,
                                transform = 'sqrt') +
  ggplot2::ggtitle('Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/pred/comp_change_historic_better_redcovar_4k.png',
                width = 10, height = 10, units = 'cm')

#### 13. Compare r ####

# Combine and format
both_cors <- cbind(historical_cors, modern_cors$modern_cors)
colnames(both_cors) <- c('model', 'Historical', 'Modern')

both_cors_4k <- cbind(historical_cors_4k, modern_cors_4k$modern_cors_4k)
colnames(both_cors_4k) <- c('model', 'Historical', 'Modern')

# Plot comparison of correlation coefficient
both_cors |>
  tidyr::pivot_longer(cols = Historical:Modern,
                      names_to = 'period',
                      values_to = 'correlation') |>
  dplyr::mutate(model = dplyr::if_else(model == 'Climate + soil + coordinates',
                                       'Climate + soil +\ncoordinates',
                                       model),
                model = dplyr::if_else(model == 'Uncorrelated covariates',
                                       'Uncorrelated\ncovariates',
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
                filename = 'figures/gam/M/density/pred/pred_corr_comparison.png',
                height = 8, width = 12, units = 'cm')

# Plot comparison of correlation coefficient with lower basis dimension
both_cors_4k |>
  tidyr::pivot_longer(cols = Historical:Modern,
                      names_to = 'period',
                      values_to = 'correlation') |>
  dplyr::mutate(model = dplyr::if_else(model == 'Climate + soil + coordinates',
                                       'Climate + soil +\ncoordinates',
                                       model),
                model = dplyr::if_else(model == 'Uncorrelated covariates',
                                       'Uncorrelated\ncovariates',
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
                filename = 'figures/gam/M/density/pred/pred_corr_comparison_4k.png',
                height = 8, width = 12, units = 'cm')
