## STEP 4-7

## Plot out-of-sample predictions from HISTORICAL
## model predicting HISTORICAL and MODERN
## TOTAL STEM DENSITY

## 1. Load historical predictions
## 2. Process historical predictions
## 3. Plot historical predicted vs observed
## 4. Calculate historical r
## 5. Load modern predictions
## 6. Process modern predictions
## 7. Plot modern predicted vs observed
## 8. Calculate modern r
## 9. Compare r

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

#### 3. Plot historical predicted versus observed ####

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
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred_allcovar.png',
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
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred_facets.png',
                width = 20, height = 20, units = 'cm')

# Map of study region
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

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
                filename = 'figures/rf/H/density/pred/h2h_pred_spatial_allcovar.png',
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
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2h_pred_spatial_facets.png',
                width = 20, height = 20, units = 'cm')

#### 4. Calculate historical r ####

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

#### 5. Load modern predictions ####

# Load modern predictions from model 1
load('out/rf/H/density/predicted_modern_rf1.RData')
# Load modern predictions from model 2
load('out/rf/H/density/predicted_modern_rf2.RData')
# Load modern predictions from model 3
load('out/rf/H/density/predicted_modern_rf3.RData')
# Load modern predictions from model 4
load('out/rf/H/density/predicted_modern_rf4.RData')

#### 6. Process modern predictions ####

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

#### 7. Plot modern predicted vs observed ####

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
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred_allcovar.png',
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
                fit = dplyr::if_else(fit == 'pred_out_rf2',
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
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred_facets.png',
                width = 20, height = 20, units = 'cm')

# Plot observed - predicted over space
# Shows spatial distribution of model error
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
                filename = 'figures/rf/H/density/pred/h2m_pred_spatial_allcovar.png',
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
                filename = 'figures/rf/H/density/pred/h2m_pred_spatial_allcovar_log.png',
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
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred_spatial_allcovar_sqrt.png',
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
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 12))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/density/pred/h2m_pred_spatial_facets.png',
                width = 20, height = 20, units = 'cm')

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
                filename = 'figures/rf/H/density/pred/h2m_pred_spatial_facets_log.png',
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
                filename = 'figures/rf/H/density/pred/h2m_pred_spatial_facets_sqrt.png',
                height = 20, width = 20, units = 'cm')

#### 8. Calculate modern r ####

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

#### 9. Compare r ####

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
