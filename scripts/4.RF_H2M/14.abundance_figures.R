## STEP 4-14

## Plot out-of-sample predictions from HISTORICAL
## model predicting HISTORICAL and MODERN
## RELATIVE ABUNDANCE

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
## 11. Compre predictions in space
## 12. Compare differences in space
## 13. Compare r

rm(list = ls())

#### 1. Load historical predictions ####

# Load historical predictions from model 1
load('out/rf/H/abundance/predicted_historical_rf1.RData')
# Load historical predictions from model 2
load('out/rf/H/abundance/predicted_historical_rf2.RData')
# Load historical predictions from model 3
load('out/rf/H/abundance/predicted_historical_rf3.RData')
# Load historical predictions from model 4
load('out/rf/H/abundance/predicted_historical_rf4.RData')

#### 2. Process historical predictions ####

# Observed column names
obs_cols <- c('Ash', 'Basswood', 'Beech',
              'Birch', 'Cherry', 'Dogwood',
              'Elm', 'Fir', 'Hemlock',
              'Hickory', 'Ironwood', 'Maple',
              'Oak', 'Pine', 'Spruce',
              'Tamarack', 'Walnut', 'oh',
              'gum', 'cedar', 'poplar')
# Predicted column names
pred_cols <- c('Ash_pred', 'Basswood_pred', 'Beech_pred',
               'Birch_pred', 'Cherry_pred', 'Dogwood_pred',
               'Elm_pred', 'Fir_pred', 'Hemlock_pred',
               'Hickory_pred', 'Ironwood_pred', 'Maple_pred',
               'Oak_pred', 'Pine_pred', 'Spruce_pred',
               'Tamarack_pred', 'Walnut_pred', 'oh_pred',
               'gum_pred', 'cedar_pred', 'poplar_pred')

# Predicted column names for each model
pred_cols1 <- paste(pred_cols, '1', sep = '')
pred_cols2 <- paste(pred_cols, '2', sep = '')
pred_cols3 <- paste(pred_cols, '3', sep = '')
pred_cols4 <- paste(pred_cols, '4', sep = '')

# Select only necessary columns from each dataframe
pred_historical_rf1 <- dplyr::select(pred_historical_rf1,
                                     x, y,
                                     dplyr::all_of(obs_cols),
                                     dplyr::all_of(pred_cols1))
pred_historical_rf2 <- dplyr::select(pred_historical_rf2,
                                     x, y,
                                     dplyr::all_of(obs_cols),
                                     dplyr::all_of(pred_cols2))
pred_historical_rf3 <- dplyr::select(pred_historical_rf3,
                                     x, y,
                                     dplyr::all_of(obs_cols),
                                     dplyr::all_of(pred_cols3))
pred_historical_rf4 <- dplyr::select(pred_historical_rf4,
                                     x, y,
                                     dplyr::all_of(obs_cols),
                                     dplyr::all_of(pred_cols4))

# Combine dataframes
pred_historical <- pred_historical_rf1 |>
  dplyr::full_join(y = pred_historical_rf2,
                   by = c('x', 'y', obs_cols)) |>
  dplyr::full_join(y = pred_historical_rf3,
                   by = c('x', 'y', obs_cols)) |>
  dplyr::full_join(y = pred_historical_rf4,
                   by = c('x', 'y', obs_cols))

# Check that rows combined correctly
nrow(pred_historical) == nrow(pred_historical_rf1) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_rf2) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_rf3) # should be TRUE
nrow(pred_historical) == nrow(pred_historical_rf4) # should be TRUE

# Calculate observed minus predicted for each taxon
pred_historical <- pred_historical |>
  dplyr::mutate(Ash_diff1 = Ash - Ash_pred1,
                Ash_diff2 = Ash - Ash_pred2,
                Ash_diff3 = Ash - Ash_pred3,
                Ash_diff4 = Ash - Ash_pred4,
                Basswood_diff1 = Basswood - Basswood_pred1,
                Basswood_diff2 = Basswood - Basswood_pred2,
                Basswood_diff3 = Basswood - Basswood_pred3,
                Basswood_diff4 = Basswood - Basswood_pred4,
                Beech_diff1 = Beech - Beech_pred1,
                Beech_diff2 = Beech - Beech_pred2,
                Beech_diff3 = Beech - Beech_pred3,
                Beech_diff4 = Beech - Beech_pred4,
                Birch_diff1 = Birch - Birch_pred1,
                Birch_diff2 = Birch - Birch_pred2,
                Birch_diff3 = Birch - Birch_pred3,
                Birch_diff4 = Birch - Birch_pred4,
                Cherry_diff1 = Cherry - Cherry_pred1,
                Cherry_diff2 = Cherry - Cherry_pred2,
                Cherry_diff3 = Cherry - Cherry_pred3,
                Cherry_diff4 = Cherry - Cherry_pred4,
                Dogwood_diff1 = Dogwood - Dogwood_pred1,
                Dogwood_diff2 = Dogwood - Dogwood_pred2,
                Dogwood_diff3 = Dogwood - Dogwood_pred3,
                Dogwood_diff4 = Dogwood - Dogwood_pred4,
                Elm_diff1 = Elm - Elm_pred1,
                Elm_diff2 = Elm - Elm_pred2,
                Elm_diff3 = Elm - Elm_pred3,
                Elm_diff4 = Elm - Elm_pred4,
                Fir_diff1 = Fir - Fir_pred1,
                Fir_diff2 = Fir - Fir_pred2,
                Fir_diff3 = Fir - Fir_pred3,
                Fir_diff4 = Fir - Fir_pred4,
                Hemlock_diff1 = Hemlock - Hemlock_pred1,
                Hemlock_diff2 = Hemlock - Hemlock_pred2,
                Hemlock_diff3 = Hemlock - Hemlock_pred3,
                Hemlock_diff4 = Hemlock - Hemlock_pred4,
                Hickory_diff1 = Hickory - Hickory_pred1,
                Hickory_diff2 = Hickory - Hickory_pred2,
                Hickory_diff3 = Hickory - Hickory_pred3,
                Hickory_diff4 = Hickory - Hickory_pred4,
                Ironwood_diff1 = Ironwood - Ironwood_pred1,
                Ironwood_diff2 = Ironwood - Ironwood_pred2,
                Ironwood_diff3 = Ironwood - Ironwood_pred3,
                Ironwood_diff4 = Ironwood - Ironwood_pred4,
                Maple_diff1 = Maple - Maple_pred1,
                Maple_diff2 = Maple - Maple_pred2,
                Maple_diff3 = Maple - Maple_pred3,
                Maple_diff4 = Maple - Maple_pred4,
                Oak_diff1 = Oak - Oak_pred1,
                Oak_diff2 = Oak - Oak_pred2,
                Oak_diff3 = Oak - Oak_pred3,
                Oak_diff4 = Oak - Oak_pred4,
                Pine_diff1 = Pine - Pine_pred1,
                Pine_diff2 = Pine - Pine_pred2,
                Pine_diff3 = Pine - Pine_pred3,
                Pine_diff4 = Pine - Pine_pred4,
                Spruce_diff1 = Spruce - Spruce_pred1,
                Spruce_diff2 = Spruce - Spruce_pred2,
                Spruce_diff3 = Spruce - Spruce_pred3,
                Spruce_diff4 = Spruce - Spruce_pred4,
                Tamarack_diff1 = Tamarack - Tamarack_pred1,
                Tamarack_diff2 = Tamarack - Tamarack_pred2,
                Tamarack_diff3 = Tamarack - Tamarack_pred3,
                Tamarack_diff4 = Tamarack - Tamarack_pred4,
                Walnut_diff1 = Walnut - Walnut_pred1,
                Walnut_diff2 = Walnut - Walnut_pred2,
                Walnut_diff3 = Walnut - Walnut_pred3,
                Walnut_diff4 = Walnut - Walnut_pred4,
                oh_diff1 = oh - oh_pred1,
                oh_diff2 = oh - oh_pred2,
                oh_diff3 = oh - oh_pred3,
                oh_diff4 = oh - oh_pred4,
                gum_diff1 = gum - gum_pred1,
                gum_diff2 = gum - gum_pred2,
                gum_diff3 = gum - gum_pred3,
                gum_diff4 = gum - gum_pred4,
                cedar_diff1 = cedar - cedar_pred1,
                cedar_diff2 = cedar - cedar_pred2,
                cedar_diff3 = cedar - cedar_pred3,
                cedar_diff4 = cedar - cedar_pred4,
                poplar_diff1 = poplar - poplar_pred1,
                poplar_diff2 = poplar - poplar_pred2,
                poplar_diff3 = poplar - poplar_pred3,
                poplar_diff4 = poplar - poplar_pred4)

#### 3. Plot historical predictions ####

# Map of study region
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

## Plot predictions over space with main model

# Ash
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ash_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_ash.png',
                width = 10, height = 10, units = 'cm')

# Basswood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Basswood_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_basswood.png',
                width = 10, height = 10, units = 'cm')

# Beech
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Beech_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_beech.png',
                width = 10, height = 10, units = 'cm')

# Birch
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Birch_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_birch.png',
                width = 10, height = 10, units = 'cm')

# Cherry
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Cherry_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_cherry.png',
                width = 10, height = 10, units = 'cm')

# Dogwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Dogwood_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_dogwood.png',
                width = 10, height = 10, units = 'cm')

# Elm
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Elm_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_elm.png',
                width = 10, height = 10, units = 'cm')

# Fir
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Fir_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_fir.png',
                width = 10, height = 10, units = 'cm')

# Hemlock
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hemlock_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_hemlock.png',
                width = 10, height = 10, units = 'cm')

# Hickory
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hickory_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_hickory.png',
                width = 10, height = 10, units = 'cm')

# Ironwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ironwood_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_ironwood.png',
                width = 10, height = 10, units = 'cm')

# Maple
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Maple_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_maple.png',
                width = 10, height = 10, units = 'cm')

# Oak
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Oak_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_oak.png',
                width = 10, height = 10, units = 'cm')

# Pine
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Pine_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_pine.png',
                width = 10, height = 10, units = 'cm')

# Spruce
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Spruce_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_spruce.png',
                width = 10, height = 10, units = 'cm')

# Tamarack
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Tamarack_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_tamarack.png',
                width = 10, height = 10, units = 'cm')

# Walnut
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Walnut_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_walnut.png',
                width = 10, height = 10, units = 'cm')

# Other hardwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_oh.png',
                width = 10, height = 10, units = 'cm')

# Black gum/sweet gum
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = gum_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_gum.png',
                width = 10, height = 10, units = 'cm')

# Cedar/juniper
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cedar_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_cedar.png',
                width = 10, height = 10, units = 'cm')

# Poplar/tulip poplar
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = poplar_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_allcovar_poplar.png',
                width = 10, height = 10, units = 'cm')

## Plot predictions over space with all models

# Ash
pred_historical |>
  tidyr::pivot_longer(cols = c(Ash_pred1, Ash_pred2,
                               Ash_pred3, Ash_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_ash.png',
                height = 20, width = 20, units = 'cm')

# Basswood
pred_historical |>
  tidyr::pivot_longer(cols = c(Basswood_pred1, Basswood_pred2,
                               Basswood_pred3, Basswood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_basswood.png',
                height = 20, width = 20, units = 'cm')

# Beech
pred_historical |>
  tidyr::pivot_longer(cols = c(Beech_pred1, Beech_pred2,
                               Beech_pred3, Beech_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_beech.png',
                height = 20, width = 20, units = 'cm')

# Birch
pred_historical |>
  tidyr::pivot_longer(cols = c(Birch_pred1, Birch_pred2,
                               Birch_pred3, Birch_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_birch.png',
                height = 20, width = 20, units = 'cm')

# Cherry
pred_historical |>
  tidyr::pivot_longer(cols = c(Cherry_pred1, Cherry_pred2,
                               Cherry_pred3, Cherry_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_cherry.png',
                height = 20, width = 20, units = 'cm')

# Dogwood
pred_historical |>
  tidyr::pivot_longer(cols = c(Dogwood_pred1, Dogwood_pred2,
                               Dogwood_pred3, Dogwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_dogwood.png',
                height = 20, width = 20, units = 'cm')

# Elm
pred_historical |>
  tidyr::pivot_longer(cols = c(Elm_pred1, Elm_pred2,
                               Elm_pred3, Elm_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_elm.png',
                height = 20, width = 20, units = 'cm')

# Fir
pred_historical |>
  tidyr::pivot_longer(cols = c(Fir_pred1, Fir_pred2,
                               Fir_pred3, Fir_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_fir.png',
                height = 20, width = 20, units = 'cm')

# Hemlock
pred_historical |>
  tidyr::pivot_longer(cols = c(Hemlock_pred1, Hemlock_pred2,
                               Hemlock_pred3, Hemlock_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_hemlock.png',
                height = 20, width = 20, units = 'cm')

# Hickory
pred_historical |>
  tidyr::pivot_longer(cols = c(Hickory_pred1, Hickory_pred2,
                               Hickory_pred3, Hickory_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_hickory.png',
                height = 20, width = 20, units = 'cm')

# Ironwood
pred_historical |>
  tidyr::pivot_longer(cols = c(Ironwood_pred1, Ironwood_pred2,
                               Ironwood_pred3, Ironwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_ironwood.png',
                height = 20, width = 20, units = 'cm')

# Maple
pred_historical |>
  tidyr::pivot_longer(cols = c(Maple_pred1, Maple_pred2,
                               Maple_pred3, Maple_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_maple.png',
                height = 20, width = 20, units = 'cm')

# Oak
pred_historical |>
  tidyr::pivot_longer(cols = c(Oak_pred1, Oak_pred2,
                               Oak_pred3, Oak_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_oak.png',
                height = 20, width = 20, units = 'cm')

# Pine
pred_historical |>
  tidyr::pivot_longer(cols = c(Pine_pred1, Pine_pred2,
                               Pine_pred3, Pine_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_pine.png',
                height = 20, width = 20, units = 'cm')

# Spruce
pred_historical |>
  tidyr::pivot_longer(cols = c(Spruce_pred1, Spruce_pred2,
                               Spruce_pred3, Spruce_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_spruce.png',
                height = 20, width = 20, units = 'cm')

# Tamarack
pred_historical |>
  tidyr::pivot_longer(cols = c(Tamarack_pred1, Tamarack_pred2,
                               Tamarack_pred3, Tamarack_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_tamarack.png',
                height = 20, width = 20, units = 'cm')

# Walnut
pred_historical |>
  tidyr::pivot_longer(cols = c(Walnut_pred1, Walnut_pred2,
                               Walnut_pred3, Walnut_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_walnut.png',
                height = 20, width = 20, units = 'cm')

# Other hardwood
pred_historical |>
  tidyr::pivot_longer(cols = c(oh_pred1, oh_pred2,
                               oh_pred3, oh_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_oh.png',
                height = 20, width = 20, units = 'cm')

# Black gum/sweet gum
pred_historical |>
  tidyr::pivot_longer(cols = c(gum_pred1, gum_pred2,
                               gum_pred3, gum_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_gum.png',
                height = 20, width = 20, units = 'cm')

# Cedar/juniper
pred_historical |>
  tidyr::pivot_longer(cols = c(cedar_pred1, cedar_pred2,
                               cedar_pred3, cedar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_cedar.png',
                height = 20, width = 20, units = 'cm')

# Poplar/tulip poplar
pred_historical |>
  tidyr::pivot_longer(cols = c(poplar_pred1, poplar_pred2,
                               poplar_pred3, poplar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred_space_facets_poplar.png',
                height = 20, width = 20, units = 'cm')

#### 4. Plot historical predicted versus observed ####

## Main model scatterplot of predicted versus observed per taxon

# Ash
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ash, y = Ash_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ash, y = Ash_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                   color = '1:1'),
                      linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_ash.png',
                height = 10, width = 10, units = 'cm')

# Basswood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Basswood, y = Basswood_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Basswood, y = Basswood_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_basswood.png',
                height = 10, width = 10, units = 'cm')

# Beech
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Beech, y = Beech_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Beech, y = Beech_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_beech.png',
                height = 10, width = 10, units = 'cm')

# Birch
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Birch, y = Birch_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Birch, y = Birch_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_birch.png',
                height = 10, width = 10, units = 'cm')

# Cherry
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Cherry, y = Cherry_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Cherry, y = Cherry_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_cherry.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Dogwood, y = Dogwood_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Dogwood, y = Dogwood_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_dogwood.png',
                height = 10, width = 10, units = 'cm')

# Elm
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Elm, y = Elm_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Elm, y = Elm_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_elm.png',
                height = 10, width = 10, units = 'cm')

# Fir
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Fir, y = Fir_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Fir, y = Fir_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_fir.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hemlock, y = Hemlock_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hemlock, y = Hemlock_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_hemlock.png',
                height = 10, width = 10, units = 'cm')

# Hickory
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hickory, y = Hickory_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hickory, y = Hickory_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_hickory.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ironwood, y = Ironwood_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ironwood, y = Ironwood_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_ironwood.png',
                height = 10, width = 10, units = 'cm')

# Maple
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Maple, y = Maple_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Maple, y = Maple_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_maple.png',
                height = 10, width = 10, units = 'cm')

# Oak
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Oak, y = Oak_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Oak, y = Oak_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_oak.png',
                height = 10, width = 10, units = 'cm')

# Pine
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Pine, y = Pine_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Pine, y = Pine_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_pine.png',
                height = 10, width = 10, units = 'cm')

# Spruce
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Spruce, y = Spruce_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Spruce, y = Spruce_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_spruce.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Tamarack, y = Tamarack_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Tamarack, y = Tamarack_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_tamarack.png',
                height = 10, width = 10, units = 'cm')

# Walnut
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Walnut, y = Walnut_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Walnut, y = Walnut_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_walnut.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = oh, y = oh_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = oh, y = oh_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other hardwod taxa') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_oh.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = gum, y = gum_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = gum, y = gum_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_gum.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = cedar, y = cedar_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = cedar, y = cedar_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_cedar.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = poplar, y = poplar_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = poplar, y = poplar_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_allcovar_poplar.png',
                height = 10, width = 10, units = 'cm')

## Plot predicted versus observed for each model fit

# Ash
pred_historical |>
  tidyr::pivot_longer(cols = c(Ash_pred1, Ash_pred2,
                               Ash_pred3, Ash_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ash, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ash, y = pred,
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
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_ash.png',
                width = 20, height = 20, units = 'cm')

# Basswood
pred_historical |>
  tidyr::pivot_longer(cols = c(Basswood_pred1, Basswood_pred2,
                               Basswood_pred3, Basswood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Basswood, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Basswood, y = pred,
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
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_basswood.png',
                width = 20, height = 20, units = 'cm')

# Beech
pred_historical |>
  tidyr::pivot_longer(cols = c(Beech_pred1, Beech_pred2,
                               Beech_pred3, Beech_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Beech, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Beech, y = pred,
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
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_beech.png',
                width = 20, height = 20, units = 'cm')

# Birch
pred_historical |>
  tidyr::pivot_longer(cols = c(Birch_pred1, Birch_pred2,
                               Birch_pred3, Birch_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Birch, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Birch, y = pred,
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
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_birch.png',
                width = 20, height = 20, units = 'cm')

# Cherry
pred_historical |>
  tidyr::pivot_longer(cols = c(Cherry_pred1, Cherry_pred2,
                               Cherry_pred3, Cherry_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Cherry, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Cherry, y = pred,
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
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_cherry.png',
                width = 20, height = 20, units = 'cm')

# Dogwood
pred_historical |>
  tidyr::pivot_longer(cols = c(Dogwood_pred1, Dogwood_pred2,
                               Dogwood_pred3, Dogwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Dogwood, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Dogwood, y = pred,
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
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_dogwood.png',
                width = 20, height = 20, units = 'cm')

# Elm
pred_historical |>
  tidyr::pivot_longer(cols = c(Elm_pred1, Elm_pred2,
                               Elm_pred3, Elm_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Elm, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Elm, y = pred,
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
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_elm.png',
                width = 20, height = 20, units = 'cm')

# Fir
pred_historical |>
  tidyr::pivot_longer(cols = c(Fir_pred1, Fir_pred2,
                               Fir_pred3, Fir_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Fir, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Fir, y = pred,
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
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_fir.png',
                width = 20, height = 20, units = 'cm')

# Hemlock
pred_historical |>
  tidyr::pivot_longer(cols = c(Hemlock_pred1, Hemlock_pred2,
                               Hemlock_pred3, Hemlock_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hemlock, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hemlock, y = pred,
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
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_hemlock.png',
                width = 20, height = 20, units = 'cm')

# Hickory
pred_historical |>
  tidyr::pivot_longer(cols = c(Hickory_pred1, Hickory_pred2,
                               Hickory_pred3, Hickory_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hickory, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hickory, y = pred,
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
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_hickory.png',
                width = 20, height = 20, units = 'cm')

# Ironwood
pred_historical |>
  tidyr::pivot_longer(cols = c(Ironwood_pred1, Ironwood_pred2,
                               Ironwood_pred3, Ironwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ironwood, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ironwood, y = pred,
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
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_ironwood.png',
                width = 20, height = 20, units = 'cm')

# Maple
pred_historical |>
  tidyr::pivot_longer(cols = c(Maple_pred1, Maple_pred2,
                               Maple_pred3, Maple_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Maple, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Maple, y = pred,
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
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_maple.png',
                width = 20, height = 20, units = 'cm')

# Oak
pred_historical |>
  tidyr::pivot_longer(cols = c(Oak_pred1, Oak_pred2,
                               Oak_pred3, Oak_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Oak, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Oak, y = pred,
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
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_oak.png',
                width = 20, height = 20, units = 'cm')

# Pine
pred_historical |>
  tidyr::pivot_longer(cols = c(Pine_pred1, Pine_pred2,
                               Pine_pred3, Pine_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Pine, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Pine, y = pred,
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
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_pine.png',
                width = 20, height = 20, units = 'cm')

# Spruce
pred_historical |>
  tidyr::pivot_longer(cols = c(Spruce_pred1, Spruce_pred2,
                               Spruce_pred3, Spruce_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Spruce, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Spruce, y = pred,
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
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_spruce.png',
                width = 20, height = 20, units = 'cm')

# Tamarack
pred_historical |>
  tidyr::pivot_longer(cols = c(Tamarack_pred1, Tamarack_pred2,
                               Tamarack_pred3, Tamarack_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Tamarack, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Tamarack, y = pred,
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
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_tamarack.png',
                width = 20, height = 20, units = 'cm')

# Walnut
pred_historical |>
  tidyr::pivot_longer(cols = c(Walnut_pred1, Walnut_pred2,
                               Walnut_pred3, Walnut_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Walnut, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Walnut, y = pred,
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
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_walnut.png',
                width = 20, height = 20, units = 'cm')

# Other hardwood
pred_historical |>
  tidyr::pivot_longer(cols = c(oh_pred1, oh_pred2,
                               oh_pred3, oh_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = oh, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = oh, y = pred,
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
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_oh.png',
                width = 20, height = 20, units = 'cm')

# Black gum/sweet gum
pred_historical |>
  tidyr::pivot_longer(cols = c(gum_pred1, gum_pred2,
                               gum_pred3, gum_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = gum, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = gum, y = pred,
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
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_gum.png',
                width = 20, height = 20, units = 'cm')

# Cedar/juniper
pred_historical |>
  tidyr::pivot_longer(cols = c(cedar_pred1, cedar_pred2,
                               cedar_pred3, cedar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = cedar, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = cedar, y = pred,
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
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_cedar.png',
                width = 20, height = 20, units = 'cm')

# Poplar/tulip poplar
pred_historical |>
  tidyr::pivot_longer(cols = c(poplar_pred1, poplar_pred2,
                               poplar_pred3, poplar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = poplar, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = poplar, y = pred,
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
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_predvobs_facets_poplar.png',
                width = 20, height = 20, units = 'cm')

## Plot observed - predicted over space for each taxon for primary model
## Shows spatial distribution of model error

# Ash
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ash_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_ash.png',
                width = 10, height = 10, units = 'cm')

# Basswood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Basswood_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_basswood.png',
                width = 10, height = 10, units = 'cm')

# Beech
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Beech_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_beech.png',
                width = 10, height = 10, units = 'cm')

# Birch
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Birch_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                               limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_birch.png',
                width = 10, height = 10, units = 'cm')

# Cherry
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Cherry_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_cherry.png',
                width = 10, height = 10, units = 'cm')

# Dogwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Dogwood_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_dogwood.png',
                width = 10, height = 10, units = 'cm')

# Elm
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Elm_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_elm.png',
                width = 10, height = 10, units = 'cm')

# Fir
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Fir_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_fir.png',
                width = 10, height = 10, units = 'cm')

# Hemlock
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hemlock_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_hemlock.png',
                width = 10, height = 10, units = 'cm')

# Hickory
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hickory_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_hickory.png',
                width = 10, height = 10, units = 'cm')

# Ironwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ironwood_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_ironwood.png',
                width = 10, height = 10, units = 'cm')

# Maple
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Maple_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_maple.png',
                width = 10, height = 10, units = 'cm')

# Oak
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Oak_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_oak.png',
                width = 10, height = 10, units = 'cm')

# Pine
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Pine_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_pine.png',
                width = 10, height = 10, units = 'cm')

# Spruce
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Spruce_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_spruce.png',
                width = 10, height = 10, units = 'cm')

# Tamarack
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Tamarack_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_tamarack.png',
                width = 10, height = 10, units = 'cm')

# Walnut
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Walnut_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_walnut.png',
                width = 10, height = 10, units = 'cm')

# Other hardwood
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_oh.png',
                width = 10, height = 10, units = 'cm')

# Black gum/sweet gum
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = gum_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_gum.png',
                width = 10, height = 10, units = 'cm')

# Cedar/juniper
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cedar_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_cedar.png',
                width = 10, height = 10, units = 'cm')

# Poplar/tulip poplar
pred_historical |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = poplar_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_allcovar_poplar.png',
                width = 10, height = 10, units = 'cm')

## Plot observed - predicted for all four models for each taxon

# Ash
pred_historical |>
  tidyr::pivot_longer(cols = c(Ash_diff1, Ash_diff2,
                               Ash_diff3, Ash_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_ash.png',
                width = 20, height = 20, units = 'cm')

# Basswood
pred_historical |>
  tidyr::pivot_longer(cols = c(Basswood_diff1, Basswood_diff2,
                               Basswood_diff3, Basswood_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_basswood.png',
                width = 20, height = 20, units = 'cm')

# Beech
pred_historical |>
  tidyr::pivot_longer(cols = c(Beech_diff1, Beech_diff2,
                               Beech_diff3, Beech_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_beech.png',
                width = 20, height = 20, units = 'cm')

# Birch
pred_historical |>
  tidyr::pivot_longer(cols = c(Birch_diff1, Birch_diff2,
                               Birch_diff3, Birch_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_birch.png',
                width = 20, height = 20, units = 'cm')

# Cherry
pred_historical |>
  tidyr::pivot_longer(cols = c(Cherry_diff1, Cherry_diff2,
                               Cherry_diff3, Cherry_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_cherry.png',
                width = 20, height = 20, units = 'cm')

# Dogwood
pred_historical |>
  tidyr::pivot_longer(cols = c(Dogwood_diff1, Dogwood_diff2,
                               Dogwood_diff3, Dogwood_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_ash.png',
                width = 20, height = 20, units = 'cm')

# Elm
pred_historical |>
  tidyr::pivot_longer(cols = c(Elm_diff1, Elm_diff2,
                               Elm_diff3, Elm_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_elm.png',
                width = 20, height = 20, units = 'cm')

# Fir
pred_historical |>
  tidyr::pivot_longer(cols = c(Fir_diff1, Fir_diff2,
                               Fir_diff3, Fir_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_fir.png',
                width = 20, height = 20, units = 'cm')

# Hemlock
pred_historical |>
  tidyr::pivot_longer(cols = c(Hemlock_diff1, Hemlock_diff2,
                               Hemlock_diff3, Hemlock_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_hemlock.png',
                width = 20, height = 20, units = 'cm')

# Hickory
pred_historical |>
  tidyr::pivot_longer(cols = c(Hickory_diff1, Hickory_diff2,
                               Hickory_diff3, Hickory_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_hickory.png',
                width = 20, height = 20, units = 'cm')

# Ironwood
pred_historical |>
  tidyr::pivot_longer(cols = c(Ironwood_diff1, Ironwood_diff2,
                               Ironwood_diff3, Ironwood_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_ironwood.png',
                width = 20, height = 20, units = 'cm')

# Maple
pred_historical |>
  tidyr::pivot_longer(cols = c(Maple_diff1, Maple_diff2,
                               Maple_diff3, Maple_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_maple.png',
                width = 20, height = 20, units = 'cm')

# Oak
pred_historical |>
  tidyr::pivot_longer(cols = c(Oak_diff1, Oak_diff2,
                               Oak_diff3, Oak_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_oak.png',
                width = 20, height = 20, units = 'cm')

# Pine
pred_historical |>
  tidyr::pivot_longer(cols = c(Pine_diff1, Pine_diff2,
                               Pine_diff3, Pine_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_pine.png',
                width = 20, height = 20, units = 'cm')

# Spruce
pred_historical |>
  tidyr::pivot_longer(cols = c(Spruce_diff1, Spruce_diff2,
                               Spruce_diff3, Spruce_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_spruce.png',
                width = 20, height = 20, units = 'cm')

# Tamarack
pred_historical |>
  tidyr::pivot_longer(cols = c(Tamarack_diff1, Tamarack_diff2,
                               Tamarack_diff3, Tamarack_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_tamarack.png',
                width = 20, height = 20, units = 'cm')

# Walnut
pred_historical |>
  tidyr::pivot_longer(cols = c(Walnut_diff1, Walnut_diff2,
                               Walnut_diff3, Walnut_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_walnut.png',
                width = 20, height = 20, units = 'cm')

# Other hardwood
pred_historical |>
  tidyr::pivot_longer(cols = c(oh_diff1, oh_diff2,
                               oh_diff3, oh_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_oh.png',
                width = 20, height = 20, units = 'cm')

# Black gum/sweet gum
pred_historical |>
  tidyr::pivot_longer(cols = c(gum_diff1, gum_diff2,
                               gum_diff3, gum_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_gum.png',
                width = 20, height = 20, units = 'cm')

# Cedar/juniper
pred_historical |>
  tidyr::pivot_longer(cols = c(cedar_diff1, cedar_diff2,
                               cedar_diff3, cedar_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_cedar.png',
                width = 20, height = 20, units = 'cm')

# Poplar/tulip poplar
pred_historical |>
  tidyr::pivot_longer(cols = c(poplar_diff1, poplar_diff2,
                               poplar_diff3, poplar_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2h_pred-obs_facets_poplar.png',
                width = 20, height = 20, units = 'cm')

#### 5. Calculate historical r ####

## Correlation coefficients for each taxon with model 1

corr_ash1 <- cor(x = pred_historical$Ash,
                 y = pred_historical$Ash_pred1)
corr_basswood1 <- cor(x = pred_historical$Basswood,
                      y = pred_historical$Basswood_pred1)
corr_beech1 <- cor(x = pred_historical$Beech,
                   y = pred_historical$Beech_pred1)
corr_birch1 <- cor(x = pred_historical$Birch,
                   y = pred_historical$Birch_pred1)
corr_cherry1 <- cor(x = pred_historical$Cherry,
                    y = pred_historical$Cherry_pred1)
corr_dogwood1 <- cor(x = pred_historical$Dogwood,
                     y = pred_historical$Dogwood_pred1)
corr_elm1 <- cor(x = pred_historical$Elm,
                 y = pred_historical$Elm_pred1)
corr_fir1 <- cor(x = pred_historical$Fir,
                 y = pred_historical$Fir_pred1)
corr_hemlock1 <- cor(x = pred_historical$Hemlock,
                     y = pred_historical$Hemlock_pred1)
corr_hickory1 <- cor(x = pred_historical$Hickory,
                     y = pred_historical$Hickory_pred1)
corr_ironwood1 <- cor(x = pred_historical$Ironwood,
                      y = pred_historical$Ironwood_pred1)
corr_maple1 <- cor(x = pred_historical$Maple,
                   y = pred_historical$Maple_pred1)
corr_oak1 <- cor(x = pred_historical$Oak,
                 y = pred_historical$Oak_pred1)
corr_pine1 <- cor(x = pred_historical$Pine,
                  y = pred_historical$Pine_pred1)
corr_spruce1 <- cor(x = pred_historical$Spruce,
                    y = pred_historical$Spruce_pred1)
corr_tamarack1 <- cor(x = pred_historical$Tamarack,
                      y = pred_historical$Tamarack_pred1)
corr_walnut1 <- cor(x = pred_historical$Walnut,
                    y = pred_historical$Walnut_pred1)
corr_oh1 <- cor(x = pred_historical$oh,
                y = pred_historical$oh_pred1)
corr_gum1 <- cor(x = pred_historical$gum,
                 y = pred_historical$gum_pred1)
corr_cedar1 <- cor(x = pred_historical$cedar,
                   y = pred_historical$cedar_pred1)
corr_poplar1 <- cor(x = pred_historical$poplar,
                    y = pred_historical$poplar_pred1)

# Taxon names
taxa <- c('Ash', 'Basswood', 'Beech',
          'Birch', 'Cherry', 'Dogwood',
          'Elm', 'Fir', 'Hemlock',
          'Hickory', 'Ironwood', 'Maple',
          'Oak', 'Pine', 'Spruce',
          'Tamarack', 'Walnut', 'Other hardwood',
          'Black gum/sweet gum', 'Cedar/juniper', 'Poplar/tulip poplar')

# Combine correlations
corr1 <- c(corr_ash1, corr_basswood1, corr_beech1,
           corr_birch1, corr_cherry1, corr_dogwood1,
           corr_elm1, corr_fir1, corr_hemlock1,
           corr_hickory1, corr_ironwood1, corr_maple1,
           corr_oak1, corr_pine1, corr_spruce1,
           corr_tamarack1, corr_walnut1, corr_oh1,
           corr_gum1, corr_cedar1, corr_poplar1)

# Combine into dataframe
historical_cors <- as.data.frame(cbind(taxa, corr1))

## Correlation coefficients for each taxon with model 2

corr_ash2 <- cor(x = pred_historical$Ash,
                 y = pred_historical$Ash_pred2)
corr_basswood2 <- cor(x = pred_historical$Basswood,
                      y = pred_historical$Basswood_pred2)
corr_beech2 <- cor(x = pred_historical$Beech,
                   y = pred_historical$Beech_pred2)
corr_birch2 <- cor(x = pred_historical$Birch,
                   y = pred_historical$Birch_pred2)
corr_cherry2 <- cor(x = pred_historical$Cherry,
                    y = pred_historical$Cherry_pred2)
corr_dogwood2 <- cor(x = pred_historical$Dogwood,
                     y = pred_historical$Dogwood_pred2)
corr_elm2 <- cor(x = pred_historical$Elm,
                 y = pred_historical$Elm_pred2)
corr_fir2 <- cor(x = pred_historical$Fir,
                 y = pred_historical$Fir_pred2)
corr_hemlock2 <- cor(x = pred_historical$Hemlock,
                     y = pred_historical$Hemlock_pred2)
corr_hickory2 <- cor(x = pred_historical$Hickory,
                     y = pred_historical$Hickory_pred2)
corr_ironwood2 <- cor(x = pred_historical$Ironwood,
                      y = pred_historical$Ironwood_pred2)
corr_maple2 <- cor(x = pred_historical$Maple,
                   y = pred_historical$Maple_pred2)
corr_oak2 <- cor(x = pred_historical$Oak,
                 y = pred_historical$Oak_pred2)
corr_pine2 <- cor(x = pred_historical$Pine,
                  y = pred_historical$Pine_pred2)
corr_spruce2 <- cor(x = pred_historical$Spruce,
                    y = pred_historical$Spruce_pred2)
corr_tamarack2 <- cor(x = pred_historical$Tamarack,
                      y = pred_historical$Tamarack_pred2)
corr_walnut2 <- cor(x = pred_historical$Walnut,
                    y = pred_historical$Walnut_pred2)
corr_oh2 <- cor(x = pred_historical$oh,
                y = pred_historical$oh_pred2)
corr_gum2 <- cor(x = pred_historical$gum,
                 y = pred_historical$gum_pred2)
corr_cedar2 <- cor(x = pred_historical$cedar,
                   y = pred_historical$cedar_pred2)
corr_poplar2 <- cor(x = pred_historical$poplar,
                    y = pred_historical$poplar_pred2)

# Combine correlations
corr2 <- c(corr_ash2, corr_basswood2, corr_beech2,
           corr_birch2, corr_cherry2, corr_dogwood2,
           corr_elm2, corr_fir2, corr_hemlock2,
           corr_hickory2, corr_ironwood2, corr_maple2,
           corr_oak2, corr_pine2, corr_spruce2,
           corr_tamarack2, corr_walnut2, corr_oh2,
           corr_gum2, corr_cedar2, corr_poplar2)

# Add to existing dataframe
historical_cors$corr2 <- corr2

## Correlation coefficients for each taxon with model 3

corr_ash3 <- cor(x = pred_historical$Ash,
                 y = pred_historical$Ash_pred3)
corr_basswood3 <- cor(x = pred_historical$Basswood,
                      y = pred_historical$Basswood_pred3)
corr_beech3 <- cor(x = pred_historical$Beech,
                   y = pred_historical$Beech_pred3)
corr_birch3 <- cor(x = pred_historical$Birch,
                   y = pred_historical$Birch_pred3)
corr_cherry3 <- cor(x = pred_historical$Cherry,
                    y = pred_historical$Cherry_pred3)
corr_dogwood3 <- cor(x = pred_historical$Dogwood,
                     y = pred_historical$Dogwood_pred3)
corr_elm3 <- cor(x = pred_historical$Elm,
                 y = pred_historical$Elm_pred3)
corr_fir3 <- cor(x = pred_historical$Fir,
                 y = pred_historical$Fir_pred3)
corr_hemlock3 <- cor(x = pred_historical$Hemlock,
                     y = pred_historical$Hemlock_pred3)
corr_hickory3 <- cor(x = pred_historical$Hickory,
                     y = pred_historical$Hickory_pred3)
corr_ironwood3 <- cor(x = pred_historical$Ironwood,
                      y = pred_historical$Ironwood_pred3)
corr_maple3 <- cor(x = pred_historical$Maple,
                   y = pred_historical$Maple_pred3)
corr_oak3 <- cor(x = pred_historical$Oak,
                 y = pred_historical$Oak_pred3)
corr_pine3 <- cor(x = pred_historical$Pine,
                  y = pred_historical$Pine_pred3)
corr_spruce3 <- cor(x = pred_historical$Spruce,
                    y = pred_historical$Spruce_pred3)
corr_tamarack3 <- cor(x = pred_historical$Tamarack,
                      y = pred_historical$Tamarack_pred3)
corr_walnut3 <- cor(x = pred_historical$Walnut,
                    y = pred_historical$Walnut_pred3)
corr_oh3 <- cor(x = pred_historical$oh,
                y = pred_historical$oh_pred3)
corr_gum3 <- cor(x = pred_historical$gum,
                 y = pred_historical$gum_pred3)
corr_cedar3 <- cor(x = pred_historical$cedar,
                   y = pred_historical$cedar_pred3)
corr_poplar3 <- cor(x = pred_historical$poplar,
                    y = pred_historical$poplar_pred3)

# Combine correlations
corr3 <- c(corr_ash3, corr_basswood3, corr_beech3,
           corr_birch3, corr_cherry3, corr_dogwood3,
           corr_elm3, corr_fir3, corr_hemlock3,
           corr_hickory3, corr_ironwood3, corr_maple3,
           corr_oak3, corr_pine3, corr_spruce3,
           corr_tamarack3, corr_walnut3, corr_oh3,
           corr_gum3, corr_cedar3, corr_poplar3)

# Add to existing dataframe
historical_cors$corr3 <- corr3

## Correlation coefficients for each taxon with model 4

corr_ash4 <- cor(x = pred_historical$Ash,
                 y = pred_historical$Ash_pred4)
corr_basswood4 <- cor(x = pred_historical$Basswood,
                      y = pred_historical$Basswood_pred4)
corr_beech4 <- cor(x = pred_historical$Beech,
                   y = pred_historical$Beech_pred4)
corr_birch4 <- cor(x = pred_historical$Birch,
                   y = pred_historical$Birch_pred4)
corr_cherry4 <- cor(x = pred_historical$Cherry,
                    y = pred_historical$Cherry_pred4)
corr_dogwood4 <- cor(x = pred_historical$Dogwood,
                     y = pred_historical$Dogwood_pred4)
corr_elm4 <- cor(x = pred_historical$Elm,
                 y = pred_historical$Elm_pred4)
corr_fir4 <- cor(x = pred_historical$Fir,
                 y = pred_historical$Fir_pred4)
corr_hemlock4 <- cor(x = pred_historical$Hemlock,
                     y = pred_historical$Hemlock_pred4)
corr_hickory4 <- cor(x = pred_historical$Hickory,
                     y = pred_historical$Hickory_pred4)
corr_ironwood4 <- cor(x = pred_historical$Ironwood,
                      y = pred_historical$Ironwood_pred4)
corr_maple4 <- cor(x = pred_historical$Maple,
                   y = pred_historical$Maple_pred4)
corr_oak4 <- cor(x = pred_historical$Oak,
                 y = pred_historical$Oak_pred4)
corr_pine4 <- cor(x = pred_historical$Pine,
                  y = pred_historical$Pine_pred4)
corr_spruce4 <- cor(x = pred_historical$Spruce,
                    y = pred_historical$Spruce_pred4)
corr_tamarack4 <- cor(x = pred_historical$Tamarack,
                      y = pred_historical$Tamarack_pred4)
corr_walnut4 <- cor(x = pred_historical$Walnut,
                    y = pred_historical$Walnut_pred4)
corr_oh4 <- cor(x = pred_historical$oh,
                y = pred_historical$oh_pred4)
corr_gum4 <- cor(x = pred_historical$gum,
                 y = pred_historical$gum_pred4)
corr_cedar4 <- cor(x = pred_historical$cedar,
                   y = pred_historical$cedar_pred4)
corr_poplar4 <- cor(x = pred_historical$poplar,
                    y = pred_historical$poplar_pred4)

# Combine correlations
corr4 <- c(corr_ash4, corr_basswood4, corr_beech4,
           corr_birch4, corr_cherry4, corr_dogwood4,
           corr_elm4, corr_fir4, corr_hemlock4,
           corr_hickory4, corr_ironwood4, corr_maple4,
           corr_oak4, corr_pine4, corr_spruce4,
           corr_tamarack4, corr_walnut4, corr_oh4,
           corr_gum4, corr_cedar4, corr_poplar4)

# Add to existing dataframe
historical_cors$corr4 <- corr4

historical_cors

#### 6. Load modern predictions ####

# Load modern predictions from model 1
load('out/rf/H/abundance/predicted_modern_rf1.RData')
# Load modern predictions from model 2
load('out/rf/H/abundance/predicted_modern_rf2.RData')
# Load modern predictions from model 3
load('out/rf/H/abundance/predicted_modern_rf3.RData')
# Load modern predictions from model 4
load('out/rf/H/abundance/predicted_modern_rf4.RData')

#### 7. Process modern predictions ####

# Select only necessary columns for each dataframe
pred_modern_rf1 <- dplyr::select(pred_modern_rf1,
                                 x, y,
                                 dplyr::all_of(obs_cols),
                                 dplyr::all_of(pred_cols1))
pred_modern_rf2 <- dplyr::select(pred_modern_rf2,
                                 x, y,
                                 dplyr::all_of(obs_cols),
                                 dplyr::all_of(pred_cols2))
pred_modern_rf3 <- dplyr::select(pred_modern_rf3,
                                 x, y,
                                 dplyr::all_of(obs_cols),
                                 dplyr::all_of(pred_cols3))
pred_modern_rf4 <- dplyr::select(pred_modern_rf4,
                                 x, y,
                                 dplyr::all_of(obs_cols),
                                 dplyr::all_of(pred_cols4))

# Combine dataframes
pred_modern <- pred_modern_rf1 |>
  dplyr::full_join(y = pred_modern_rf2,
                   by = c('x', 'y', obs_cols)) |>
  dplyr::full_join(y = pred_modern_rf3,
                   by = c('x', 'y', obs_cols)) |>
  dplyr::full_join(y = pred_modern_rf4,
                   by = c('x', 'y', obs_cols))

# Check that rows combined correctly
nrow(pred_modern) == nrow(pred_modern_rf1) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_rf2) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_rf3) # should be TRUE
nrow(pred_modern) == nrow(pred_modern_rf4) # should be TRUE

# Calculate observed minus predicted
pred_modern <- pred_modern |>
  dplyr::mutate(Ash_diff1 = Ash - Ash_pred1,
                Ash_diff2 = Ash - Ash_pred2,
                Ash_diff3 = Ash - Ash_pred3,
                Ash_diff4 = Ash - Ash_pred4,
                Basswood_diff1 = Basswood - Basswood_pred1,
                Basswood_diff2 = Basswood - Basswood_pred2,
                Basswood_diff3 = Basswood - Basswood_pred3,
                Basswood_diff4 = Basswood - Basswood_pred4,
                Beech_diff1 = Beech - Beech_pred1,
                Beech_diff2 = Beech - Beech_pred2,
                Beech_diff3 = Beech - Beech_pred3,
                Beech_diff4 = Beech - Beech_pred4,
                Birch_diff1 = Birch - Birch_pred1,
                Birch_diff2 = Birch - Birch_pred2,
                Birch_diff3 = Birch - Birch_pred3,
                Birch_diff4 = Birch - Birch_pred4,
                Cherry_diff1 = Cherry - Cherry_pred1,
                Cherry_diff2 = Cherry - Cherry_pred2,
                Cherry_diff3 = Cherry - Cherry_pred3,
                Cherry_diff4 = Cherry - Cherry_pred4,
                Dogwood_diff1 = Dogwood - Dogwood_pred1,
                Dogwood_diff2 = Dogwood - Dogwood_pred2,
                Dogwood_diff3 = Dogwood - Dogwood_pred3,
                Dogwood_diff4 = Dogwood - Dogwood_pred4,
                Elm_diff1 = Elm - Elm_pred1,
                Elm_diff2 = Elm - Elm_pred2,
                Elm_diff3 = Elm - Elm_pred3,
                Elm_diff4 = Elm - Elm_pred4,
                Fir_diff1 = Fir - Fir_pred1,
                Fir_diff2 = Fir - Fir_pred2,
                Fir_diff3 = Fir - Fir_pred3,
                Fir_diff4 = Fir - Fir_pred4,
                Hemlock_diff1 = Hemlock - Hemlock_pred1,
                Hemlock_diff2 = Hemlock - Hemlock_pred2,
                Hemlock_diff3 = Hemlock - Hemlock_pred3,
                Hemlock_diff4 = Hemlock - Hemlock_pred4,
                Hickory_diff1 = Hickory - Hickory_pred1,
                Hickory_diff2 = Hickory - Hickory_pred2,
                Hickory_diff3 = Hickory - Hickory_pred3,
                Hickory_diff4 = Hickory - Hickory_pred4,
                Ironwood_diff1 = Ironwood - Ironwood_pred1,
                Ironwood_diff2 = Ironwood - Ironwood_pred2,
                Ironwood_diff3 = Ironwood - Ironwood_pred3,
                Ironwood_diff4 = Ironwood - Ironwood_pred4,
                Maple_diff1 = Maple - Maple_pred1,
                Maple_diff2 = Maple - Maple_pred2,
                Maple_diff3 = Maple - Maple_pred3,
                Maple_diff4 = Maple - Maple_pred4,
                Oak_diff1 = Oak - Oak_pred1,
                Oak_diff2 = Oak - Oak_pred2,
                Oak_diff3 = Oak - Oak_pred3,
                Oak_diff4 = Oak - Oak_pred4,
                Pine_diff1 = Pine - Pine_pred1,
                Pine_diff2 = Pine - Pine_pred2,
                Pine_diff3 = Pine - Pine_pred3,
                Pine_diff4 = Pine - Pine_pred4,
                Spruce_diff1 = Spruce - Spruce_pred1,
                Spruce_diff2 = Spruce - Spruce_pred2,
                Spruce_diff3 = Spruce - Spruce_pred3,
                Spruce_diff4 = Spruce - Spruce_pred4,
                Tamarack_diff1 = Tamarack - Tamarack_pred1,
                Tamarack_diff2 = Tamarack - Tamarack_pred2,
                Tamarack_diff3 = Tamarack - Tamarack_pred3,
                Tamarack_diff4 = Tamarack - Tamarack_pred4,
                Walnut_diff1 = Walnut - Walnut_pred1,
                Walnut_diff2 = Walnut - Walnut_pred2,
                Walnut_diff3 = Walnut - Walnut_pred3,
                Walnut_diff4 = Walnut - Walnut_pred4,
                oh_diff1 = oh - oh_pred1,
                oh_diff2 = oh - oh_pred2,
                oh_diff3 = oh - oh_pred3,
                oh_diff4 = oh - oh_pred4,
                gum_diff1 = gum - gum_pred1,
                gum_diff2 = gum - gum_pred2,
                gum_diff3 = gum - gum_pred3,
                gum_diff4 = gum - gum_pred4,
                cedar_diff1 = cedar - cedar_pred1,
                cedar_diff2 = cedar - cedar_pred2,
                cedar_diff3 = cedar - cedar_pred3,
                cedar_diff4 = cedar - cedar_pred4,
                poplar_diff1 = poplar - poplar_pred1,
                poplar_diff2 = poplar - poplar_pred2,
                poplar_diff3 = poplar - poplar_pred3,
                poplar_diff4 = poplar - poplar_pred4)

#### 8. Plot modern predictions ####

## Plot predictions over space with main model

# Ash
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ash_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_ash.png',
                width = 10, height = 10, units = 'cm')

# Basswood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Basswood_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_basswood.png',
                width = 10, height = 10, units = 'cm')

# Beech
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Beech_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_beech.png',
                width = 10, height = 10, units = 'cm')

# Birch
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Birch_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_birch.png',
                width = 10, height = 10, units = 'cm')

# Cherry
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Cherry_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_cherry.png',
                width = 10, height = 10, units = 'cm')

# Dogwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Dogwood_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_dogwood.png',
                width = 10, height = 10, units = 'cm')

# Elm
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Elm_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_elm.png',
                width = 10, height = 10, units = 'cm')

# Fir
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Fir_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_fir.png',
                width = 10, height = 10, units = 'cm')

# Hemlock
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hemlock_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_hemlock.png',
                width = 10, height = 10, units = 'cm')

# Hickory
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hickory_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_hickory.png',
                width = 10, height = 10, units = 'cm')

# Ironwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ironwood_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_ironwood.png',
                width = 10, height = 10, units = 'cm')

# Maple
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Maple_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_maple.png',
                width = 10, height = 10, units = 'cm')

# Oak
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Oak_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_oak.png',
                width = 10, height = 10, units = 'cm')

# Pine
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Pine_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_pine.png',
                width = 10, height = 10, units = 'cm')

# Spruce
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Spruce_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_spruce.png',
                width = 10, height = 10, units = 'cm')

# Tamarack
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Tamarack_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_tamarack.png',
                width = 10, height = 10, units = 'cm')

# Walnut
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Walnut_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_walnut.png',
                width = 10, height = 10, units = 'cm')

# Other hardwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_oh.png',
                width = 10, height = 10, units = 'cm')

# Black gum/sweet gum
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = gum_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_gum.png',
                width = 10, height = 10, units = 'cm')

# Cedar/juniper
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cedar_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_cedar.png',
                width = 10, height = 10, units = 'cm')

# Poplar/tulip poplar
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = poplar_pred1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_allcovar_poplar.png',
                width = 10, height = 10, units = 'cm')

## Plot predictions over space with all models

# Ash
pred_modern |>
  tidyr::pivot_longer(cols = c(Ash_pred1, Ash_pred2,
                               Ash_pred3, Ash_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_ash.png',
                height = 20, width = 20, units = 'cm')

# Basswood
pred_modern |>
  tidyr::pivot_longer(cols = c(Basswood_pred1, Basswood_pred2,
                               Basswood_pred3, Basswood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_basswood.png',
                height = 20, width = 20, units = 'cm')

# Beech
pred_modern |>
  tidyr::pivot_longer(cols = c(Beech_pred1, Beech_pred2,
                               Beech_pred3, Beech_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_beech.png',
                height = 20, width = 20, units = 'cm')

# Birch
pred_modern |>
  tidyr::pivot_longer(cols = c(Birch_pred1, Birch_pred2,
                               Birch_pred3, Birch_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_birch.png',
                height = 20, width = 20, units = 'cm')

# Cherry
pred_modern |>
  tidyr::pivot_longer(cols = c(Cherry_pred1, Cherry_pred2,
                               Cherry_pred3, Cherry_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_cherry.png',
                height = 20, width = 20, units = 'cm')

# Dogwood
pred_modern |>
  tidyr::pivot_longer(cols = c(Dogwood_pred1, Dogwood_pred2,
                               Dogwood_pred3, Dogwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_dogwood.png',
                height = 20, width = 20, units = 'cm')

# Elm
pred_modern |>
  tidyr::pivot_longer(cols = c(Elm_pred1, Elm_pred2,
                               Elm_pred3, Elm_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_elm.png',
                height = 20, width = 20, units = 'cm')

# Fir
pred_modern |>
  tidyr::pivot_longer(cols = c(Fir_pred1, Fir_pred2,
                               Fir_pred3, Fir_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_fir.png',
                height = 20, width = 20, units = 'cm')

# Hemlock
pred_modern |>
  tidyr::pivot_longer(cols = c(Hemlock_pred1, Hemlock_pred2,
                               Hemlock_pred3, Hemlock_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_hemlock.png',
                height = 20, width = 20, units = 'cm')

# Hickory
pred_modern |>
  tidyr::pivot_longer(cols = c(Hickory_pred1, Hickory_pred2,
                               Hickory_pred3, Hickory_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_hickory.png',
                height = 20, width = 20, units = 'cm')

# Ironwood
pred_modern |>
  tidyr::pivot_longer(cols = c(Ironwood_pred1, Ironwood_pred2,
                               Ironwood_pred3, Ironwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_ironwood.png',
                height = 20, width = 20, units = 'cm')

# Maple
pred_modern |>
  tidyr::pivot_longer(cols = c(Maple_pred1, Maple_pred2,
                               Maple_pred3, Maple_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_maple.png',
                height = 20, width = 20, units = 'cm')

# Oak
pred_modern |>
  tidyr::pivot_longer(cols = c(Oak_pred1, Oak_pred2,
                               Oak_pred3, Oak_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_oak.png',
                height = 20, width = 20, units = 'cm')

# Pine
pred_modern |>
  tidyr::pivot_longer(cols = c(Pine_pred1, Pine_pred2,
                               Pine_pred3, Pine_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_pine.png',
                height = 20, width = 20, units = 'cm')

# Spruce
pred_modern |>
  tidyr::pivot_longer(cols = c(Spruce_pred1, Spruce_pred2,
                               Spruce_pred3, Spruce_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_spruce.png',
                height = 20, width = 20, units = 'cm')

# Tamarack
pred_modern |>
  tidyr::pivot_longer(cols = c(Tamarack_pred1, Tamarack_pred2,
                               Tamarack_pred3, Tamarack_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_tamarack.png',
                height = 20, width = 20, units = 'cm')

# Walnut
pred_modern |>
  tidyr::pivot_longer(cols = c(Walnut_pred1, Walnut_pred2,
                               Walnut_pred3, Walnut_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_walnut.png',
                height = 20, width = 20, units = 'cm')

# Other hardwood
pred_modern |>
  tidyr::pivot_longer(cols = c(oh_pred1, oh_pred2,
                               oh_pred3, oh_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_oh.png',
                height = 20, width = 20, units = 'cm')

# Black gum/sweet gum
pred_modern |>
  tidyr::pivot_longer(cols = c(gum_pred1, gum_pred2,
                               gum_pred3, gum_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_gum.png',
                height = 20, width = 20, units = 'cm')

# Cedar/juniper
pred_modern |>
  tidyr::pivot_longer(cols = c(cedar_pred1, cedar_pred2,
                               cedar_pred3, cedar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_cedar.png',
                height = 20, width = 20, units = 'cm')

# Poplar/tulip poplar
pred_modern |>
  tidyr::pivot_longer(cols = c(poplar_pred1, poplar_pred2,
                               poplar_pred3, poplar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'Fraction total\nstems',
                                palette = 'BuPu',
                                direction = 1,
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred_space_facets_poplar.png',
                height = 20, width = 20, units = 'cm')

#### 9. Plot modern predicted vs observed ####

## Main model scatterplot of predicted versus observed per taxon

# Ash
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ash, y = Ash_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ash, y = Ash_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_ash.png',
                height = 10, width = 10, units = 'cm')

# Basswood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Basswood, y = Basswood_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Basswood, y = Basswood_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_basswood.png',
                height = 10, width = 10, units = 'cm')

# Beech
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Beech, y = Beech_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Beech, y = Beech_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_beech.png',
                height = 10, width = 10, units = 'cm')

# Birch
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Birch, y = Birch_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Birch, y = Birch_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_birch.png',
                height = 10, width = 10, units = 'cm')

# Cherry
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Cherry, y = Cherry_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Cherry, y = Cherry_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_cherry.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Dogwood, y = Dogwood_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Dogwood, y = Dogwood_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_dogwood.png',
                height = 10, width = 10, units = 'cm')

# Elm
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Elm, y = Elm_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Elm, y = Elm_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_elm.png',
                height = 10, width = 10, units = 'cm')

# Fir
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Fir, y = Fir_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Fir, y = Fir_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_fir.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hemlock, y = Hemlock_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hemlock, y = Hemlock_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_hemlock.png',
                height = 10, width = 10, units = 'cm')

# Hickory
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hickory, y = Hickory_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hickory, y = Hickory_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_hickory.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ironwood, y = Ironwood_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ironwood, y = Ironwood_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_ironwood.png',
                height = 10, width = 10, units = 'cm')

# Maple
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Maple, y = Maple_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Maple, y = Maple_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_maple.png',
                height = 10, width = 10, units = 'cm')

# Oak
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Oak, y = Oak_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Oak, y = Oak_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_oak.png',
                height = 10, width = 10, units = 'cm')

# Pine
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Pine, y = Pine_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Pine, y = Pine_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_pine.png',
                height = 10, width = 10, units = 'cm')

# Spruce
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Spruce, y = Spruce_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Spruce, y = Spruce_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_spruce.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Tamarack, y = Tamarack_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Tamarack, y = Tamarack_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_tamarack.png',
                height = 10, width = 10, units = 'cm')

# Walnut
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Walnut, y = Walnut_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Walnut, y = Walnut_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_walnut.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = oh, y = oh_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = oh, y = oh_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other hardwod taxa') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_oh.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = gum, y = gum_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = gum, y = gum_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_gum.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = cedar, y = cedar_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = cedar, y = cedar_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_cedar.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = poplar, y = poplar_pred1)) +
  ggplot2::geom_smooth(ggplot2::aes(x = poplar, y = poplar_pred1,
                                    color = 'Best fit'),
                       method = 'lm', se = FALSE) +
  ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                    color = '1:1'),
                       linetype = 'dashed', linewidth = 1) +
  ggplot2::scale_color_discrete(name = '') +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_allcovar_poplar.png',
                height = 10, width = 10, units = 'cm')

## Plot predicted versus observed for each model fit

# Ash
pred_modern |>
  tidyr::pivot_longer(cols = c(Ash_pred1, Ash_pred2,
                               Ash_pred3, Ash_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ash, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ash, y = pred,
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
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_ash.png',
                width = 20, height = 20, units = 'cm')

# Basswood
pred_modern |>
  tidyr::pivot_longer(cols = c(Basswood_pred1, Basswood_pred2,
                               Basswood_pred3, Basswood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Basswood, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Basswood, y = pred,
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
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_basswood.png',
                width = 20, height = 20, units = 'cm')

# Beech
pred_modern |>
  tidyr::pivot_longer(cols = c(Beech_pred1, Beech_pred2,
                               Beech_pred3, Beech_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Beech, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Beech, y = pred,
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
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_beech.png',
                width = 20, height = 20, units = 'cm')

# Birch
pred_modern |>
  tidyr::pivot_longer(cols = c(Birch_pred1, Birch_pred2,
                               Birch_pred3, Birch_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Birch, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Birch, y = pred,
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
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_birch.png',
                width = 20, height = 20, units = 'cm')

# Cherry
pred_modern |>
  tidyr::pivot_longer(cols = c(Cherry_pred1, Cherry_pred2,
                               Cherry_pred3, Cherry_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Cherry, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Cherry, y = pred,
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
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_cherry.png',
                width = 20, height = 20, units = 'cm')

# Dogwood
pred_modern |>
  tidyr::pivot_longer(cols = c(Dogwood_pred1, Dogwood_pred2,
                               Dogwood_pred3, Dogwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Dogwood, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Dogwood, y = pred,
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
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_dogwood.png',
                width = 20, height = 20, units = 'cm')

# Elm
pred_modern |>
  tidyr::pivot_longer(cols = c(Elm_pred1, Elm_pred2,
                               Elm_pred3, Elm_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Elm, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Elm, y = pred,
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
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_elm.png',
                width = 20, height = 20, units = 'cm')

# Fir
pred_modern |>
  tidyr::pivot_longer(cols = c(Fir_pred1, Fir_pred2,
                               Fir_pred3, Fir_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Fir, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Fir, y = pred,
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
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_fir.png',
                width = 20, height = 20, units = 'cm')

# Hemlock
pred_modern |>
  tidyr::pivot_longer(cols = c(Hemlock_pred1, Hemlock_pred2,
                               Hemlock_pred3, Hemlock_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hemlock, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hemlock, y = pred,
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
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_hemlock.png',
                width = 20, height = 20, units = 'cm')

# Hickory
pred_modern |>
  tidyr::pivot_longer(cols = c(Hickory_pred1, Hickory_pred2,
                               Hickory_pred3, Hickory_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Hickory, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Hickory, y = pred,
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
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_hickory.png',
                width = 20, height = 20, units = 'cm')

# Ironwood
pred_modern |>
  tidyr::pivot_longer(cols = c(Ironwood_pred1, Ironwood_pred2,
                               Ironwood_pred3, Ironwood_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Ironwood, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Ironwood, y = pred,
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
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_ironwood.png',
                width = 20, height = 20, units = 'cm')

# Maple
pred_modern |>
  tidyr::pivot_longer(cols = c(Maple_pred1, Maple_pred2,
                               Maple_pred3, Maple_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Maple, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Maple, y = pred,
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
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_maple.png',
                width = 20, height = 20, units = 'cm')

# Oak
pred_modern |>
  tidyr::pivot_longer(cols = c(Oak_pred1, Oak_pred2,
                               Oak_pred3, Oak_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Oak, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Oak, y = pred,
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
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_oak.png',
                width = 20, height = 20, units = 'cm')

# Pine
pred_modern |>
  tidyr::pivot_longer(cols = c(Pine_pred1, Pine_pred2,
                               Pine_pred3, Pine_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Pine, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Pine, y = pred,
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
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_pine.png',
                width = 20, height = 20, units = 'cm')

# Spruce
pred_modern |>
  tidyr::pivot_longer(cols = c(Spruce_pred1, Spruce_pred2,
                               Spruce_pred3, Spruce_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Spruce, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Spruce, y = pred,
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
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_spruce.png',
                width = 20, height = 20, units = 'cm')

# Tamarack
pred_modern |>
  tidyr::pivot_longer(cols = c(Tamarack_pred1, Tamarack_pred2,
                               Tamarack_pred3, Tamarack_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Tamarack, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Tamarack, y = pred,
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
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_tamarack.png',
                width = 20, height = 20, units = 'cm')

# Walnut
pred_modern |>
  tidyr::pivot_longer(cols = c(Walnut_pred1, Walnut_pred2,
                               Walnut_pred3, Walnut_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Walnut, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Walnut, y = pred,
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
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_walnut.png',
                width = 20, height = 20, units = 'cm')

# Other hardwood
pred_modern |>
  tidyr::pivot_longer(cols = c(oh_pred1, oh_pred2,
                               oh_pred3, oh_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = oh, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = oh, y = pred,
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
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_oh.png',
                width = 20, height = 20, units = 'cm')

# Black gum/sweet gum
pred_modern |>
  tidyr::pivot_longer(cols = c(gum_pred1, gum_pred2,
                               gum_pred3, gum_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = gum, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = gum, y = pred,
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
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_gum.png',
                width = 20, height = 20, units = 'cm')

# Cedar/juniper
pred_modern |>
  tidyr::pivot_longer(cols = c(cedar_pred1, cedar_pred2,
                               cedar_pred3, cedar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = cedar, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = cedar, y = pred,
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
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_cedar.png',
                width = 20, height = 20, units = 'cm')

# Poplar/tulip poplar
pred_modern |>
  tidyr::pivot_longer(cols = c(poplar_pred1, poplar_pred2,
                               poplar_pred3, poplar_pred4),
                      names_to = 'fit',
                      values_to = 'pred') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_pred1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_pred2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_pred3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_pred4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = poplar, y = pred)) +
  ggplot2::geom_smooth(ggplot2::aes(x = poplar, y = pred,
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
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 10),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_predvobs_facets_poplar.png',
                width = 20, height = 20, units = 'cm')

## Plot observed - predicted over space for each taxon for primary model
## Shows spatial distribution of model error

# Ash
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ash_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_ash.png',
                width = 10, height = 10, units = 'cm')

# Basswood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Basswood_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_basswood.png',
                width = 10, height = 10, units = 'cm')

# Beech
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Beech_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_beech.png',
                width = 10, height = 10, units = 'cm')

# Birch
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Birch_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_birch.png',
                width = 10, height = 10, units = 'cm')

# Cherry
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Cherry_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_cherry.png',
                width = 10, height = 10, units = 'cm')

# Dogwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Dogwood_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_dogwood.png',
                width = 10, height = 10, units = 'cm')

# Elm
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Elm_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_elm.png',
                width = 10, height = 10, units = 'cm')

# Fir
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Fir_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_fir.png',
                width = 10, height = 10, units = 'cm')

# Hemlock
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hemlock_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_hemlock.png',
                width = 10, height = 10, units = 'cm')

# Hickory
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hickory_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_hickory.png',
                width = 10, height = 10, units = 'cm')

# Ironwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ironwood_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_ironwood.png',
                width = 10, height = 10, units = 'cm')

# Maple
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Maple_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_maple.png',
                width = 10, height = 10, units = 'cm')

# Oak
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Oak_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_oak.png',
                width = 10, height = 10, units = 'cm')

# Pine
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Pine_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_pine.png',
                width = 10, height = 10, units = 'cm')

# Spruce
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Spruce_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_spruce.png',
                width = 10, height = 10, units = 'cm')

# Tamarack
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Tamarack_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_tamarack.png',
                width = 10, height = 10, units = 'cm')

# Walnut
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Walnut_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_walnut.png',
                width = 10, height = 10, units = 'cm')

# Other hardwood
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_oh.png',
                width = 10, height = 10, units = 'cm')

# Black gum/sweet gum
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = gum_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_gum.png',
                width = 10, height = 10, units = 'cm')

# Cedar/juniper
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cedar_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_cedar.png',
                width = 10, height = 10, units = 'cm')

# Poplar/tulip poplar
pred_modern |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = poplar_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_allcovar_poplar.png',
                width = 10, height = 10, units = 'cm')

## Plot observed - predicted for all four models for each taxon

# Ash
pred_modern |>
  tidyr::pivot_longer(cols = c(Ash_diff1, Ash_diff2,
                               Ash_diff3, Ash_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_ash.png',
                width = 20, height = 20, units = 'cm')

# Basswood
pred_modern |>
  tidyr::pivot_longer(cols = c(Basswood_diff1, Basswood_diff2,
                               Basswood_diff3, Basswood_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Basswood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_basswood.png',
                width = 20, height = 20, units = 'cm')

# Beech
pred_modern |>
  tidyr::pivot_longer(cols = c(Beech_diff1, Beech_diff2,
                               Beech_diff3, Beech_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_beech.png',
                width = 20, height = 20, units = 'cm')

# Birch
pred_modern |>
  tidyr::pivot_longer(cols = c(Birch_diff1, Birch_diff2,
                               Birch_diff3, Birch_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_birch.png',
                width = 20, height = 20, units = 'cm')

# Cherry
pred_modern |>
  tidyr::pivot_longer(cols = c(Cherry_diff1, Cherry_diff2,
                               Cherry_diff3, Cherry_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cherry') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_cherry.png',
                width = 20, height = 20, units = 'cm')

# Dogwood
pred_modern |>
  tidyr::pivot_longer(cols = c(Dogwood_diff1, Dogwood_diff2,
                               Dogwood_diff3, Dogwood_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Dogwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_ash.png',
                width = 20, height = 20, units = 'cm')

# Elm
pred_modern |>
  tidyr::pivot_longer(cols = c(Elm_diff1, Elm_diff2,
                               Elm_diff3, Elm_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_elm.png',
                width = 20, height = 20, units = 'cm')

# Fir
pred_modern |>
  tidyr::pivot_longer(cols = c(Fir_diff1, Fir_diff2,
                               Fir_diff3, Fir_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Fir') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_fir.png',
                width = 20, height = 20, units = 'cm')

# Hemlock
pred_modern |>
  tidyr::pivot_longer(cols = c(Hemlock_diff1, Hemlock_diff2,
                               Hemlock_diff3, Hemlock_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_hemlock.png',
                width = 20, height = 20, units = 'cm')

# Hickory
pred_modern |>
  tidyr::pivot_longer(cols = c(Hickory_diff1, Hickory_diff2,
                               Hickory_diff3, Hickory_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hickory') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_hickory.png',
                width = 20, height = 20, units = 'cm')

# Ironwood
pred_modern |>
  tidyr::pivot_longer(cols = c(Ironwood_diff1, Ironwood_diff2,
                               Ironwood_diff3, Ironwood_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ironwood') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_ironwood.png',
                width = 20, height = 20, units = 'cm')

# Maple
pred_modern |>
  tidyr::pivot_longer(cols = c(Maple_diff1, Maple_diff2,
                               Maple_diff3, Maple_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_maple.png',
                width = 20, height = 20, units = 'cm')

# Oak
pred_modern |>
  tidyr::pivot_longer(cols = c(Oak_diff1, Oak_diff2,
                               Oak_diff3, Oak_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_oak.png',
                width = 20, height = 20, units = 'cm')

# Pine
pred_modern |>
  tidyr::pivot_longer(cols = c(Pine_diff1, Pine_diff2,
                               Pine_diff3, Pine_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_pine.png',
                width = 20, height = 20, units = 'cm')

# Spruce
pred_modern |>
  tidyr::pivot_longer(cols = c(Spruce_diff1, Spruce_diff2,
                               Spruce_diff3, Spruce_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_spruce.png',
                width = 20, height = 20, units = 'cm')

# Tamarack
pred_modern |>
  tidyr::pivot_longer(cols = c(Tamarack_diff1, Tamarack_diff2,
                               Tamarack_diff3, Tamarack_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_tamarack.png',
                width = 20, height = 20, units = 'cm')

# Walnut
pred_modern |>
  tidyr::pivot_longer(cols = c(Walnut_diff1, Walnut_diff2,
                               Walnut_diff3, Walnut_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Walnut') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_walnut.png',
                width = 20, height = 20, units = 'cm')

# Other hardwood
pred_modern |>
  tidyr::pivot_longer(cols = c(oh_diff1, oh_diff2,
                               oh_diff3, oh_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_oh.png',
                width = 20, height = 20, units = 'cm')

# Black gum/sweet gum
pred_modern |>
  tidyr::pivot_longer(cols = c(gum_diff1, gum_diff2,
                               gum_diff3, gum_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Black gum/sweet gum') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_gum.png',
                width = 20, height = 20, units = 'cm')

# Cedar/juniper
pred_modern |>
  tidyr::pivot_longer(cols = c(cedar_diff1, cedar_diff2,
                               cedar_diff3, cedar_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Cedar/juniper') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_cedar.png',
                width = 20, height = 20, units = 'cm')

# Poplar/tulip poplar
pred_modern |>
  tidyr::pivot_longer(cols = c(poplar_diff1, poplar_diff2,
                               poplar_diff3, poplar_diff4),
                      names_to = 'fit',
                      values_to = 'diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~fit) +
  ggplot2::scale_fill_gradient2(name = 'Observed-\nPredicted',
                                limits = c(-1, 1)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Poplar/tulip poplar') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/h2m_pred-obs_facets_poplar.png',
                width = 20, height = 20, units = 'cm')

#### 10. Calculate modern r ####

## Correlation coefficients for each taxon with model 1

corr_ash1 <- cor(x = pred_modern$Ash,
                 y = pred_modern$Ash_pred1)
corr_basswood1 <- cor(x = pred_modern$Basswood,
                      y = pred_modern$Basswood_pred1)
corr_beech1 <- cor(x = pred_modern$Beech,
                   y = pred_modern$Beech_pred1)
corr_birch1 <- cor(x = pred_modern$Birch,
                   y = pred_modern$Birch_pred1)
corr_cherry1 <- cor(x = pred_modern$Cherry,
                    y = pred_modern$Cherry_pred1)
corr_dogwood1 <- cor(x = pred_modern$Dogwood,
                     y = pred_modern$Dogwood_pred1)
corr_elm1 <- cor(x = pred_modern$Elm,
                 y = pred_modern$Elm_pred1)
corr_fir1 <- cor(x = pred_modern$Fir,
                 y = pred_modern$Fir_pred1)
corr_hemlock1 <- cor(x = pred_modern$Hemlock,
                     y = pred_modern$Hemlock_pred1)
corr_hickory1 <- cor(x = pred_modern$Hickory,
                     y = pred_modern$Hickory_pred1)
corr_ironwood1 <- cor(x = pred_modern$Ironwood,
                      y = pred_modern$Ironwood_pred1)
corr_maple1 <- cor(x = pred_modern$Maple,
                   y = pred_modern$Maple_pred1)
corr_oak1 <- cor(x = pred_modern$Oak,
                 y = pred_modern$Oak_pred1)
corr_pine1 <- cor(x = pred_modern$Pine,
                  y = pred_modern$Pine_pred1)
corr_spruce1 <- cor(x = pred_modern$Spruce,
                    y = pred_modern$Spruce_pred1)
corr_tamarack1 <- cor(x = pred_modern$Tamarack,
                      y = pred_modern$Tamarack_pred1)
corr_walnut1 <- cor(x = pred_modern$Walnut,
                    y = pred_modern$Walnut_pred1)
corr_oh1 <- cor(x = pred_modern$oh,
                y = pred_modern$oh_pred1)
corr_gum1 <- cor(x = pred_modern$gum,
                 y = pred_modern$gum_pred1)
corr_cedar1 <- cor(x = pred_modern$cedar,
                   y = pred_modern$cedar_pred1)
corr_poplar1 <- cor(x = pred_modern$poplar,
                    y = pred_modern$poplar_pred1)

# Combine correlations
corr1 <- c(corr_ash1, corr_basswood1, corr_beech1,
           corr_birch1, corr_cherry1, corr_dogwood1,
           corr_elm1, corr_fir1, corr_hemlock1,
           corr_hickory1, corr_ironwood1, corr_maple1,
           corr_oak1, corr_pine1, corr_spruce1,
           corr_tamarack1, corr_walnut1, corr_oh1,
           corr_gum1, corr_cedar1, corr_poplar1)

# Combine into dataframe
modern_cors <- as.data.frame(cbind(taxa, corr1))

## Correlation coefficients for each taxon with model 2

corr_ash2 <- cor(x = pred_modern$Ash,
                 y = pred_modern$Ash_pred2)
corr_basswood2 <- cor(x = pred_modern$Basswood,
                      y = pred_modern$Basswood_pred2)
corr_beech2 <- cor(x = pred_modern$Beech,
                   y = pred_modern$Beech_pred2)
corr_birch2 <- cor(x = pred_modern$Birch,
                   y = pred_modern$Birch_pred2)
corr_cherry2 <- cor(x = pred_modern$Cherry,
                    y = pred_modern$Cherry_pred2)
corr_dogwood2 <- cor(x = pred_modern$Dogwood,
                     y = pred_modern$Dogwood_pred2)
corr_elm2 <- cor(x = pred_modern$Elm,
                 y = pred_modern$Elm_pred2)
corr_fir2 <- cor(x = pred_modern$Fir,
                 y = pred_modern$Fir_pred2)
corr_hemlock2 <- cor(x = pred_modern$Hemlock,
                     y = pred_modern$Hemlock_pred2)
corr_hickory2 <- cor(x = pred_modern$Hickory,
                     y = pred_modern$Hickory_pred2)
corr_ironwood2 <- cor(x = pred_modern$Ironwood,
                      y = pred_modern$Ironwood_pred2)
corr_maple2 <- cor(x = pred_modern$Maple,
                   y = pred_modern$Maple_pred2)
corr_oak2 <- cor(x = pred_modern$Oak,
                 y = pred_modern$Oak_pred2)
corr_pine2 <- cor(x = pred_modern$Pine,
                  y = pred_modern$Pine_pred2)
corr_spruce2 <- cor(x = pred_modern$Spruce,
                    y = pred_modern$Spruce_pred2)
corr_tamarack2 <- cor(x = pred_modern$Tamarack,
                      y = pred_modern$Tamarack_pred2)
corr_walnut2 <- cor(x = pred_modern$Walnut,
                    y = pred_modern$Walnut_pred2)
corr_oh2 <- cor(x = pred_modern$oh,
                y = pred_modern$oh_pred2)
corr_gum2 <- cor(x = pred_modern$gum,
                 y = pred_modern$gum_pred2)
corr_cedar2 <- cor(x = pred_modern$cedar,
                   y = pred_modern$cedar_pred2)
corr_poplar2 <- cor(x = pred_modern$poplar,
                    y = pred_modern$poplar_pred2)

# Combine correlations
corr2 <- c(corr_ash2, corr_basswood2, corr_beech2,
           corr_birch2, corr_cherry2, corr_dogwood2,
           corr_elm2, corr_fir2, corr_hemlock2,
           corr_hickory2, corr_ironwood2, corr_maple2,
           corr_oak2, corr_pine2, corr_spruce2,
           corr_tamarack2, corr_walnut2, corr_oh2,
           corr_gum2, corr_cedar2, corr_poplar2)

# Add to existing dataframe
modern_cors$corr2 <- corr2

## Correlation coefficients for each taxon with model 3

corr_ash3 <- cor(x = pred_modern$Ash,
                 y = pred_modern$Ash_pred3)
corr_basswood3 <- cor(x = pred_modern$Basswood,
                      y = pred_modern$Basswood_pred3)
corr_beech3 <- cor(x = pred_modern$Beech,
                   y = pred_modern$Beech_pred3)
corr_birch3 <- cor(x = pred_modern$Birch,
                   y = pred_modern$Birch_pred3)
corr_cherry3 <- cor(x = pred_modern$Cherry,
                    y = pred_modern$Cherry_pred3)
corr_dogwood3 <- cor(x = pred_modern$Dogwood,
                     y = pred_modern$Dogwood_pred3)
corr_elm3 <- cor(x = pred_modern$Elm,
                 y = pred_modern$Elm_pred3)
corr_fir3 <- cor(x = pred_modern$Fir,
                 y = pred_modern$Fir_pred3)
corr_hemlock3 <- cor(x = pred_modern$Hemlock,
                     y = pred_modern$Hemlock_pred3)
corr_hickory3 <- cor(x = pred_modern$Hickory,
                     y = pred_modern$Hickory_pred3)
corr_ironwood3 <- cor(x = pred_modern$Ironwood,
                      y = pred_modern$Ironwood_pred3)
corr_maple3 <- cor(x = pred_modern$Maple,
                   y = pred_modern$Maple_pred3)
corr_oak3 <- cor(x = pred_modern$Oak,
                 y = pred_modern$Oak_pred3)
corr_pine3 <- cor(x = pred_modern$Pine,
                  y = pred_modern$Pine_pred3)
corr_spruce3 <- cor(x = pred_modern$Spruce,
                    y = pred_modern$Spruce_pred3)
corr_tamarack3 <- cor(x = pred_modern$Tamarack,
                      y = pred_modern$Tamarack_pred3)
corr_walnut3 <- cor(x = pred_modern$Walnut,
                    y = pred_modern$Walnut_pred3)
corr_oh3 <- cor(x = pred_modern$oh,
                y = pred_modern$oh_pred3)
corr_gum3 <- cor(x = pred_modern$gum,
                 y = pred_modern$gum_pred3)
corr_cedar3 <- cor(x = pred_modern$cedar,
                   y = pred_modern$cedar_pred3)
corr_poplar3 <- cor(x = pred_modern$poplar,
                    y = pred_modern$poplar_pred3)

# Combine correlations
corr3 <- c(corr_ash3, corr_basswood3, corr_beech3,
           corr_birch3, corr_cherry3, corr_dogwood3,
           corr_elm3, corr_fir3, corr_hemlock3,
           corr_hickory3, corr_ironwood3, corr_maple3,
           corr_oak3, corr_pine3, corr_spruce3,
           corr_tamarack3, corr_walnut3, corr_oh3,
           corr_gum3, corr_cedar3, corr_poplar3)

# Add to existing dataframe
modern_cors$corr3 <- corr3

## Correlation coefficients for each taxon with model 4

corr_ash4 <- cor(x = pred_modern$Ash,
                 y = pred_modern$Ash_pred4)
corr_basswood4 <- cor(x = pred_modern$Basswood,
                      y = pred_modern$Basswood_pred4)
corr_beech4 <- cor(x = pred_modern$Beech,
                   y = pred_modern$Beech_pred4)
corr_birch4 <- cor(x = pred_modern$Birch,
                   y = pred_modern$Birch_pred4)
corr_cherry4 <- cor(x = pred_modern$Cherry,
                    y = pred_modern$Cherry_pred4)
corr_dogwood4 <- cor(x = pred_modern$Dogwood,
                     y = pred_modern$Dogwood_pred4)
corr_elm4 <- cor(x = pred_modern$Elm,
                 y = pred_modern$Elm_pred4)
corr_fir4 <- cor(x = pred_modern$Fir,
                 y = pred_modern$Fir_pred4)
corr_hemlock4 <- cor(x = pred_modern$Hemlock,
                     y = pred_modern$Hemlock_pred4)
corr_hickory4 <- cor(x = pred_modern$Hickory,
                     y = pred_modern$Hickory_pred4)
corr_ironwood4 <- cor(x = pred_modern$Ironwood,
                      y = pred_modern$Ironwood_pred4)
corr_maple4 <- cor(x = pred_modern$Maple,
                   y = pred_modern$Maple_pred4)
corr_oak4 <- cor(x = pred_modern$Oak,
                 y = pred_modern$Oak_pred4)
corr_pine4 <- cor(x = pred_modern$Pine,
                  y = pred_modern$Pine_pred4)
corr_spruce4 <- cor(x = pred_modern$Spruce,
                    y = pred_modern$Spruce_pred4)
corr_tamarack4 <- cor(x = pred_modern$Tamarack,
                      y = pred_modern$Tamarack_pred4)
corr_walnut4 <- cor(x = pred_modern$Walnut,
                    y = pred_modern$Walnut_pred4)
corr_oh4 <- cor(x = pred_modern$oh,
                y = pred_modern$oh_pred4)
corr_gum4 <- cor(x = pred_modern$gum,
                 y = pred_modern$gum_pred4)
corr_cedar4 <- cor(x = pred_modern$cedar,
                   y = pred_modern$cedar_pred4)
corr_poplar4 <- cor(x = pred_modern$poplar,
                    y = pred_modern$poplar_pred4)

# Combine correlations
corr4 <- c(corr_ash4, corr_basswood4, corr_beech4,
           corr_birch4, corr_cherry4, corr_dogwood4,
           corr_elm4, corr_fir4, corr_hemlock4,
           corr_hickory4, corr_ironwood4, corr_maple4,
           corr_oak4, corr_pine4, corr_spruce4,
           corr_tamarack4, corr_walnut4, corr_oh4,
           corr_gum4, corr_cedar4, corr_poplar4)

# Add to existing dataframe
modern_cors$corr4 <- corr4

modern_cors

#### 11. Compare predictions in space ####

# Remove observation columns from historical and modern prediction dataframes
pred_historical <- dplyr::select(pred_historical,
                                 -Ash, -Basswood, -Beech,
                                 -Birch, -Cherry, -Dogwood,
                                 -Elm, -Fir, -Hemlock,
                                 -Hickory, -Ironwood, -Maple,
                                 -Oak, -Pine, -Spruce,
                                 -Tamarack, -Walnut, -oh,
                                 -gum, -cedar, -poplar)
pred_modern <- dplyr::select(pred_modern,
                             -Ash, -Basswood, -Beech,
                             -Birch, -Cherry, -Dogwood,
                             -Elm, -Fir, -Hemlock,
                             -Hickory, -Ironwood, -Maple,
                             -Oak, -Pine, -Spruce,
                             -Tamarack, -Walnut, -oh,
                             -gum, -cedar, -poplar)

# Store and remove coordinates to add back later
x_historical <- pred_historical$x
y_historical <- pred_historical$y
x_modern <- pred_modern$x
y_modern <- pred_modern$y

pred_historical <- dplyr::select(pred_historical,
                                 -x, -y)
pred_modern <- dplyr::select(pred_modern,
                             -x, -y)

# Add H or M to the end of all remaining columns
colnames(pred_historical) <- paste(colnames(pred_historical), 'H', sep = '_')
colnames(pred_modern) <- paste(colnames(pred_modern), 'M', sep = '_')

# Add coordinates back to dataframes
pred_historical$x <- x_historical
pred_historical$y <- y_historical
pred_modern$x <- x_modern
pred_modern$y <- y_modern

# Combine historical and modern dataframes
preds <- pred_historical |>
  dplyr::full_join(y = pred_modern,
                   by = c('x', 'y'))

# Find difference between perdictions between historic and modenr
preds <- preds |>
  dplyr::mutate(Ash_pred_diff1 = Ash_pred1_M - Ash_pred1_H,
                Ash_pred_diff2 = Ash_pred2_M - Ash_pred2_H,
                Ash_pred_diff3 = Ash_pred3_M - Ash_pred3_H,
                Ash_pred_diff4 = Ash_pred4_M - Ash_pred4_H,
                Basswood_pred_diff1 = Basswood_pred1_M - Basswood_pred1_H,
                Basswood_pred_diff2 = Basswood_pred2_M - Basswood_pred2_H,
                Basswood_pred_diff3 = Basswood_pred3_M - Basswood_pred3_H,
                Basswood_pred_diff4 = Basswood_pred4_M - Basswood_pred4_H,
                Beech_pred_diff1 = Beech_pred1_M - Beech_pred1_H,
                Beech_pred_diff2 = Beech_pred2_M - Beech_pred2_H,
                Beech_pred_diff3 = Beech_pred3_M - Beech_pred3_H,
                Beech_pred_diff4 = Beech_pred4_M - Beech_pred4_H,
                Birch_pred_diff1 = Birch_pred1_M - Birch_pred1_H,
                Birch_pred_diff2 = Birch_pred2_M - Birch_pred2_H,
                Birch_pred_diff3 = Birch_pred3_M - Birch_pred3_H,
                Birch_pred_diff4 = Birch_pred4_M - Birch_pred4_H,
                Cherry_pred_diff1 = Cherry_pred1_M - Cherry_pred1_H,
                Cherry_pred_diff2 = Cherry_pred2_M - Cherry_pred2_H,
                Cherry_pred_diff3 = Cherry_pred3_M - Cherry_pred3_H,
                Cherry_pred_diff4 = Cherry_pred4_M - Cherry_pred4_H,
                Dogwood_pred_diff1 = Dogwood_pred1_M - Dogwood_pred1_H,
                Dogwood_pred_diff2 = Dogwood_pred2_M - Dogwood_pred2_H,
                Dogwood_pred_diff3 = Dogwood_pred3_M - Dogwood_pred3_H,
                Dogwood_pred_diff4 = Dogwood_pred4_M - Dogwood_pred4_H,
                Elm_pred_diff1 = Elm_pred1_M - Elm_pred1_H,
                Elm_pred_diff2 = Elm_pred2_M - Elm_pred2_H,
                Elm_pred_diff3 = Elm_pred3_M - Elm_pred3_H,
                Elm_pred_diff4 = Elm_pred4_M - Elm_pred4_H,
                Fir_pred_diff1 = Fir_pred1_M - Fir_pred1_H,
                Fir_pred_diff2 = Fir_pred2_M - Fir_pred2_H,
                Fir_pred_diff3 = Fir_pred3_M - Fir_pred3_H,
                Fir_pred_diff4 = Fir_pred4_M - Fir_pred4_H,
                Hemlock_pred_diff1 = Hemlock_pred1_M - Hemlock_pred1_H,
                Hemlock_pred_diff2 = Hemlock_pred2_M - Hemlock_pred2_H,
                Hemlock_pred_diff3 = Hemlock_pred3_M - Hemlock_pred3_H,
                Hemlock_pred_diff4 = Hemlock_pred4_M - Hemlock_pred4_H,
                Hickory_pred_diff1 = Hickory_pred1_M - Hickory_pred1_H,
                Hickory_pred_diff2 = Hickory_pred2_M - Hickory_pred2_H,
                Hickory_pred_diff3 = Hickory_pred3_M - Hickory_pred3_H,
                Hickory_pred_diff4 = Hickory_pred4_M - Hickory_pred4_H,
                Ironwood_pred_diff1 = Ironwood_pred1_M - Ironwood_pred1_H,
                Ironwood_pred_diff2 = Ironwood_pred2_M - Ironwood_pred2_H,
                Ironwood_pred_diff3 = Ironwood_pred3_M - Ironwood_pred3_H,
                Ironwood_pred_diff4 = Ironwood_pred4_M - Ironwood_pred4_H,
                Maple_pred_diff1 = Maple_pred1_M - Maple_pred1_H,
                Maple_pred_diff2 = Maple_pred2_M - Maple_pred2_H,
                Maple_pred_diff3 = Maple_pred3_M - Maple_pred3_H,
                Maple_pred_diff4 = Maple_pred4_M - Maple_pred4_H,
                Oak_pred_diff1 = Oak_pred1_M - Oak_pred1_H,
                Oak_pred_diff2 = Oak_pred2_M - Oak_pred2_H,
                Oak_pred_diff3 = Oak_pred3_M - Oak_pred3_H,
                Oak_pred_diff4 = Oak_pred4_M - Oak_pred4_H,
                Pine_pred_diff1 = Pine_pred1_M - Pine_pred1_H,
                Pine_pred_diff2 = Pine_pred2_M - Pine_pred2_H,
                Pine_pred_diff3 = Pine_pred3_M - Pine_pred3_H,
                Pine_pred_diff4 = Pine_pred4_M - Pine_pred4_H,
                Spruce_pred_diff1 = Spruce_pred1_M - Spruce_pred1_H,
                Spruce_pred_diff2 = Spruce_pred2_M - Spruce_pred2_H,
                Spruce_pred_diff3 = Spruce_pred3_M - Spruce_pred3_H,
                Spruce_pred_diff4 = Spruce_pred4_M - Spruce_pred4_H,
                Tamarack_pred_diff1 = Tamarack_pred1_M - Tamarack_pred1_H,
                Tamarack_pred_diff2 = Tamarack_pred2_M - Tamarack_pred2_H,
                Tamarack_pred_diff3 = Tamarack_pred3_M - Tamarack_pred3_H,
                Tamarack_pred_diff4 = Tamarack_pred4_M - Tamarack_pred4_H,
                Walnut_pred_diff1 = Walnut_pred1_M - Walnut_pred1_H,
                Walnut_pred_diff2 = Walnut_pred2_M - Walnut_pred2_H,
                Walnut_pred_diff3 = Walnut_pred3_M - Walnut_pred3_H,
                Walnut_pred_diff4 = Walnut_pred4_M - Walnut_pred4_H,
                oh_pred_diff1 = oh_pred1_M - oh_pred1_H,
                oh_pred_diff2 = oh_pred2_M - oh_pred2_H,
                oh_pred_diff3 = oh_pred3_M - oh_pred3_H,
                oh_pred_diff4 = oh_pred4_M - oh_pred4_H,
                gum_pred_diff1 = gum_pred1_M - gum_pred1_H,
                gum_pred_diff2 = gum_pred2_M - gum_pred2_H,
                gum_pred_diff3 = gum_pred3_M - gum_pred3_H,
                gum_pred_diff4 = gum_pred4_M - gum_pred4_H,
                cedar_pred_diff1 = cedar_pred1_M - cedar_pred1_H,
                cedar_pred_diff2 = cedar_pred2_M - cedar_pred2_H,
                cedar_pred_diff3 = cedar_pred3_M - cedar_pred3_H,
                cedar_pred_diff4 = cedar_pred4_M - cedar_pred4_H,
                poplar_pred_diff1 = poplar_pred1_M - poplar_pred1_H,
                poplar_pred_diff2 = poplar_pred2_M - poplar_pred2_H,
                poplar_pred_diff3 = poplar_pred3_M - poplar_pred3_H,
                poplar_pred_diff4 = poplar_pred4_M - poplar_pred4_H)

## Plot differences in prediction in space with main model

# Ash
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ash_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_ash.png',
                height = 10, width = 10, units = 'cm')

# Basswood
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Basswood_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_basswood.png',
                height = 10, width = 10, units = 'cm')

# Beech
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Beech_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_beech.png',
                height = 10, width = 10, units = 'cm')

# Birch
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Birch_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_birch.png',
                height = 10, width = 10, units = 'cm')

# Cherry
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Cherry_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_cherry.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Dogwood_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_dogwood.png',
                height = 10, width = 10, units = 'cm')

# Elm
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Elm_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_elm.png',
                height = 10, width = 10, units = 'cm')

# Fir
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Fir_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_fir.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hemlock_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_hemlock.png',
                height = 10, width = 10, units = 'cm')

# Hickory
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Hickory_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_hickory.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Ironwood_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_ironwood.png',
                height = 10, width = 10, units = 'cm')

# Maple
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Maple_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_maple.png',
                height = 10, width = 10, units = 'cm')

# Oak
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Oak_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_oak.png',
                height = 10, width = 10, units = 'cm')

# Pine
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Pine_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_pine.png',
                height = 10, width = 10, units = 'cm')

# Spruce
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Spruce_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_spruce.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Tamarack_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_tamarack.png',
                height = 10, width = 10, units = 'cm')

# Walnut
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Walnut_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_walnut.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_oh.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = gum_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_gum.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = cedar_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_cedar.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
preds |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = poplar_pred_diff1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_allcovar_poplar.png',
                height = 10, width = 10, units = 'cm')

## Plot difference in prediction in space with all models

# Ash
preds |>
  tidyr::pivot_longer(cols = c(Ash_pred_diff1, Ash_pred_diff2,
                               Ash_pred_diff3, Ash_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ash_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ash_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ash_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ash_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_ash.png',
                height = 20, width = 20, units = 'cm')

# Basswood
preds |>
  tidyr::pivot_longer(cols = c(Basswood_pred_diff1, Basswood_pred_diff2,
                               Basswood_pred_diff3, Basswood_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Basswood_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Basswood_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_basswood.png',
                height = 20, width = 20, units = 'cm')

# Beech
preds |>
  tidyr::pivot_longer(cols = c(Beech_pred_diff1, Beech_pred_diff2,
                               Beech_pred_diff3, Beech_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Beech_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Beech_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Beech_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Beech_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_beech.png',
                height = 20, width = 20, units = 'cm')

# Birch
preds |>
  tidyr::pivot_longer(cols = c(Birch_pred_diff1, Birch_pred_diff2,
                               Birch_pred_diff3, Birch_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Birch_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Birch_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Birch_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Birch_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_birch.png',
                height = 20, width = 20, units = 'cm')

# Cherry
preds |>
  tidyr::pivot_longer(cols = c(Cherry_pred_diff1, Cherry_pred_diff2,
                               Cherry_pred_diff3, Cherry_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Cherry_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Cherry_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_cherry.png',
                height = 20, width = 20, units = 'cm')

# Dogwood
preds |>
  tidyr::pivot_longer(cols = c(Dogwood_pred_diff1, Dogwood_pred_diff2,
                               Dogwood_pred_diff3, Dogwood_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Dogwood_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Dogwood_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_dogwood.png',
                height = 20, width = 20, units = 'cm')

# Elm
preds |>
  tidyr::pivot_longer(cols = c(Elm_pred_diff1, Elm_pred_diff2,
                               Elm_pred_diff3, Elm_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Elm_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Elm_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Elm_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Elm_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_elm.png',
                height = 20, width = 20, units = 'cm')

# Fir
preds |>
  tidyr::pivot_longer(cols = c(Fir_pred_diff1, Fir_pred_diff2,
                               Fir_pred_diff3, Fir_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Fir_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Fir_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Fir_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Fir_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_fir.png',
                height = 20, width = 20, units = 'cm')

# Hemlock
preds |>
  tidyr::pivot_longer(cols = c(Hemlock_pred_diff1, Hemlock_pred_diff2,
                               Hemlock_pred_diff3, Hemlock_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hemlock_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hemlock_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_hemlock.png',
                height = 20, width = 20, units = 'cm')

# Hickory
preds |>
  tidyr::pivot_longer(cols = c(Hickory_pred_diff1, Hickory_pred_diff2,
                               Hickory_pred_diff3, Hickory_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Hickory_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Hickory_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_hickory.png',
                height = 20, width = 20, units = 'cm')

# Ironwood
preds |>
  tidyr::pivot_longer(cols = c(Ironwood_pred_diff1, Ironwood_pred_diff2,
                               Ironwood_pred_diff3, Ironwood_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Ironwood_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Ironwood_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_ironwood.png',
                height = 20, width = 20, units = 'cm')

# Maple
preds |>
  tidyr::pivot_longer(cols = c(Maple_pred_diff1, Maple_pred_diff2,
                               Maple_pred_diff3, Maple_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Maple_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Maple_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Maple_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Maple_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_maple.png',
                height = 20, width = 20, units = 'cm')

# Oak
preds |>
  tidyr::pivot_longer(cols = c(Oak_pred_diff1, Oak_pred_diff2,
                               Oak_pred_diff3, Oak_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Oak_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Oak_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Oak_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Oak_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_oak.png',
                height = 20, width = 20, units = 'cm')

# Pine
preds |>
  tidyr::pivot_longer(cols = c(Pine_pred_diff1, Pine_pred_diff2,
                               Pine_pred_diff3, Pine_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Pine_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Pine_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Pine_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Pine_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_pine.png',
                height = 20, width = 20, units = 'cm')

# Spruce
preds |>
  tidyr::pivot_longer(cols = c(Spruce_pred_diff1, Spruce_pred_diff2,
                               Spruce_pred_diff3, Spruce_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Spruce_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Spruce_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_spruce.png',
                height = 20, width = 20, units = 'cm')

# Tamarack
preds |>
  tidyr::pivot_longer(cols = c(Tamarack_pred_diff1, Tamarack_pred_diff2,
                               Tamarack_pred_diff3, Tamarack_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Tamarack_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Tamarack_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_tamarack.png',
                height = 20, width = 20, units = 'cm')

# Walnut
preds |>
  tidyr::pivot_longer(cols = c(Walnut_pred_diff1, Walnut_pred_diff2,
                               Walnut_pred_diff3, Walnut_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'Walnut_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'Walnut_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_walnut.png',
                height = 20, width = 20, units = 'cm')

# Other hardwood
preds |>
  tidyr::pivot_longer(cols = c(oh_pred_diff1, oh_pred_diff2,
                               oh_pred_diff3, oh_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'oh_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'oh_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'oh_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'oh_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_oh.png',
                height = 20, width = 20, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::pivot_longer(cols = c(gum_pred_diff1, gum_pred_diff2,
                               gum_pred_diff3, gum_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'gum_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'gum_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'gum_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'gum_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_gum.png',
                height = 20, width = 20, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::pivot_longer(cols = c(cedar_pred_diff1, cedar_pred_diff2,
                               cedar_pred_diff3, cedar_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'cedar_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'cedar_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'cedar_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'cedar_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_cedar.png',
                height = 20, width = 20, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::pivot_longer(cols = c(poplar_pred_diff1, poplar_pred_diff2,
                               poplar_pred_diff3, poplar_pred_diff4),
                      names_to = 'fit', values_to = 'pred_diff') |>
  dplyr::mutate(fit = dplyr::if_else(fit == 'poplar_pred_diff1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'poplar_pred_diff2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'poplar_pred_diff3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'poplar_pred_diff4', 'Climate + soil +\ncoordinates', fit)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pred_diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔFraction\ntotal stems',
                                low = '#e94c1f',
                                high = '#125a56',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~fit) +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Change in predicted relative abundance\nfrom historic to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred_facet_poplar.png',
                height = 20, width = 20, units = 'cm')

#### 12. Compare differences in space ####

# Calculate change in prediction accuracy
preds <- preds |>
  dplyr::mutate(delta_accuracy_ash1 = abs(Ash_diff1_H - Ash_diff1_M),
                delta_accuracy_ash2 = abs(Ash_diff2_H - Ash_diff2_M),
                delta_accuracy_ash3 = abs(Ash_diff3_H - Ash_diff3_M),
                delta_accuracy_ash4 = abs(Ash_diff4_H - Ash_diff4_M),
                delta_accuracy_basswood1 = abs(Basswood_diff1_H - Basswood_diff1_M),
                delta_accuracy_basswood2 = abs(Basswood_diff2_H - Basswood_diff2_M),
                delta_accuracy_basswood3 = abs(Basswood_diff3_H - Basswood_diff3_M),
                delta_accuracy_basswood4 = abs(Basswood_diff4_H - Basswood_diff4_M),
                delta_accuracy_beech1 = abs(Beech_diff1_H - Beech_diff1_M),
                delta_accuracy_beech2 = abs(Beech_diff2_H - Beech_diff2_M),
                delta_accuracy_beech3 = abs(Beech_diff3_H - Beech_diff3_M),
                delta_accuracy_beech4 = abs(Beech_diff4_H - Beech_diff4_M),
                delta_accuracy_birch1 = abs(Birch_diff1_H - Birch_diff1_M),
                delta_accuracy_birch2 = abs(Birch_diff2_H - Birch_diff2_M),
                delta_accuracy_birch3 = abs(Birch_diff3_H - Birch_diff3_M),
                delta_accuracy_birch4 = abs(Birch_diff4_H - Birch_diff4_M),
                delta_accuracy_cherry1 = abs(Cherry_diff1_H - Cherry_diff1_M),
                delta_accuracy_cherry2 = abs(Cherry_diff2_H - Cherry_diff2_M),
                delta_accuracy_cherry3 = abs(Cherry_diff3_H - Cherry_diff3_M),
                delta_accuracy_cherry4 = abs(Cherry_diff4_H - Cherry_diff4_M),
                delta_accuracy_dogwood1 = abs(Dogwood_diff1_H - Dogwood_diff1_M),
                delta_accuracy_dogwood2 = abs(Dogwood_diff2_H - Dogwood_diff2_M),
                delta_accuracy_dogwood3 = abs(Dogwood_diff3_H - Dogwood_diff3_M),
                delta_accuracy_dogwood4 = abs(Dogwood_diff4_H - Dogwood_diff4_M),
                delta_accuracy_elm1 = abs(Elm_diff1_H - Elm_diff1_M),
                delta_accuracy_elm2 = abs(Elm_diff2_H - Elm_diff2_M),
                delta_accuracy_elm3 = abs(Elm_diff3_H - Elm_diff3_M),
                delta_accuracy_elm4 = abs(Elm_diff4_H - Elm_diff4_M),
                delta_accuracy_fir1 = abs(Fir_diff1_H - Fir_diff1_M),
                delta_accuracy_fir2 = abs(Fir_diff2_H - Fir_diff2_M),
                delta_accuracy_fir3 = abs(Fir_diff3_H - Fir_diff3_M),
                delta_accuracy_fir4 = abs(Fir_diff4_H - Fir_diff4_M),
                delta_accuracy_hemlock1 = abs(Hemlock_diff1_H - Hemlock_diff1_M),
                delta_accuracy_hemlock2 = abs(Hemlock_diff2_H - Hemlock_diff2_M),
                delta_accuracy_hemlock3 = abs(Hemlock_diff3_H - Hemlock_diff3_M),
                delta_accuracy_hemlock4 = abs(Hemlock_diff4_H - Hemlock_diff4_M),
                delta_accuracy_hickory1 = abs(Hickory_diff1_H - Hickory_diff1_M),
                delta_accuracy_hickory2 = abs(Hickory_diff2_H - Hickory_diff2_M),
                delta_accuracy_hickory3 = abs(Hickory_diff3_H - Hickory_diff3_M),
                delta_accuracy_hickory4 = abs(Hickory_diff4_H - Hickory_diff4_M),
                delta_accuracy_ironwood1 = abs(Ironwood_diff1_H - Ironwood_diff1_M),
                delta_accuracy_ironwood2 = abs(Ironwood_diff2_H - Ironwood_diff2_M),
                delta_accuracy_ironwood3 = abs(Ironwood_diff3_H - Ironwood_diff3_M),
                delta_accuracy_ironwood4 = abs(Ironwood_diff4_H - Ironwood_diff4_M),
                delta_accuracy_maple1 = abs(Maple_diff1_H - Maple_diff1_M),
                delta_accuracy_maple2 = abs(Maple_diff2_H - Maple_diff2_M),
                delta_accuracy_maple3 = abs(Maple_diff3_H - Maple_diff3_M),
                delta_accuracy_maple4 = abs(Maple_diff4_H - Maple_diff4_M),
                delta_accuracy_oak1 = abs(Oak_diff1_H - Oak_diff1_M),
                delta_accuracy_oak2 = abs(Oak_diff2_H - Oak_diff2_M),
                delta_accuracy_oak3 = abs(Oak_diff3_H - Oak_diff3_M),
                delta_accuracy_oak4 = abs(Oak_diff4_H - Oak_diff4_M),
                delta_accuracy_pine1 = abs(Pine_diff1_H - Pine_diff1_M),
                delta_accuracy_pine2 = abs(Pine_diff2_H - Pine_diff2_M),
                delta_accuracy_pine3 = abs(Pine_diff3_H - Pine_diff3_M),
                delta_accuracy_pine4 = abs(Pine_diff4_H - Pine_diff4_M),
                delta_accuracy_spruce1 = abs(Spruce_diff1_H - Spruce_diff1_M),
                delta_accuracy_spruce2 = abs(Spruce_diff2_H - Spruce_diff2_M),
                delta_accuracy_spruce3 = abs(Spruce_diff3_H - Spruce_diff3_M),
                delta_accuracy_spruce4 = abs(Spruce_diff4_H - Spruce_diff4_M),
                delta_accuracy_tamarack1 = abs(Tamarack_diff1_H - Tamarack_diff1_M),
                delta_accuracy_tamarack2 = abs(Tamarack_diff2_H - Tamarack_diff2_M),
                delta_accuracy_tamarack3 = abs(Tamarack_diff3_H - Tamarack_diff3_M),
                delta_accuracy_tamarack4 = abs(Tamarack_diff4_H - Tamarack_diff4_M),
                delta_accuracy_walnut1 = abs(Walnut_diff1_H - Walnut_diff1_M),
                delta_accuracy_walnut2 = abs(Walnut_diff2_H - Walnut_diff2_M),
                delta_accuracy_walnut3 = abs(Walnut_diff3_H - Walnut_diff3_M),
                delta_accuracy_walnut4 = abs(Walnut_diff4_H - Walnut_diff4_M),
                delta_accuracy_oh1 = abs(oh_diff1_H - oh_diff1_M),
                delta_accuracy_oh2 = abs(oh_diff2_H - oh_diff2_M),
                delta_accuracy_oh3 = abs(oh_diff3_H - oh_diff3_M),
                delta_accuracy_oh4 = abs(oh_diff4_H - oh_diff4_M),
                delta_accuracy_gum1 = abs(gum_diff1_H - gum_diff1_M),
                delta_accuracy_gum2 = abs(gum_diff2_H - gum_diff2_M),
                delta_accuracy_gum3 = abs(gum_diff3_H - gum_diff3_M),
                delta_accuracy_gum4 = abs(gum_diff4_H - gum_diff4_M),
                delta_accuracy_cedar1 = abs(cedar_diff1_H - cedar_diff1_M),
                delta_accuracy_cedar2 = abs(cedar_diff2_H - cedar_diff2_M),
                delta_accuracy_cedar3 = abs(cedar_diff3_H - cedar_diff3_M),
                delta_accuracy_cedar4 = abs(cedar_diff4_H - cedar_diff4_M),
                delta_accuracy_poplar1 = abs(poplar_diff1_H - poplar_diff1_M),
                delta_accuracy_poplar2 = abs(poplar_diff2_H - poplar_diff2_M),
                delta_accuracy_poplar3 = abs(poplar_diff3_H - poplar_diff3_M),
                delta_accuracy_poplar4 = abs(poplar_diff4_H - poplar_diff4_M),
                hm_worse_ash1 = dplyr::if_else(abs(Ash_diff1_H) > abs(Ash_diff1_M), 'Historic', 'Modern'),
                hm_worse_basswood1 = dplyr::if_else(abs(Basswood_diff1_H) > abs(Basswood_diff1_M), 'Historic', 'Modern'),
                hm_worse_beech1 = dplyr::if_else(abs(Beech_diff1_H) > abs(Beech_diff1_M), 'Historic', 'Modern'),
                hm_worse_birch1 = dplyr::if_else(abs(Birch_diff1_H) > abs(Birch_diff1_M), 'Historic', 'Modern'),
                hm_worse_cherry1 = dplyr::if_else(abs(Cherry_diff1_H) > abs(Cherry_diff1_M), 'Historic', 'Modern'),
                hm_worse_dogwood1 = dplyr::if_else(abs(Dogwood_diff1_H) > abs(Dogwood_diff1_M), 'Historic', 'Modern'),
                hm_worse_elm1 = dplyr::if_else(abs(Elm_diff1_H) > abs(Elm_diff1_M), 'Historic', 'Modern'),
                hm_worse_fir1 = dplyr::if_else(abs(Fir_diff1_H) > abs(Fir_diff1_M), 'Historic', 'Modern'),
                hm_worse_hemlock1 = dplyr::if_else(abs(Hemlock_diff1_H) > abs(Hemlock_diff1_M), 'Historic', 'Modern'),
                hm_worse_hickory1 = dplyr::if_else(abs(Hickory_diff1_H) > abs(Hickory_diff1_M), 'Historic', 'Modern'),
                hm_worse_ironwood1 = dplyr::if_else(abs(Ironwood_diff1_H) > abs(Ironwood_diff1_M), 'Historic', 'Modern'),
                hm_worse_maple1 = dplyr::if_else(abs(Maple_diff1_H) > abs(Maple_diff1_M), 'Historic', 'Modern'),
                hm_worse_oak1 = dplyr::if_else(abs(Oak_diff1_H) > abs(Oak_diff1_M), 'Historic', 'Modern'),
                hm_worse_pine1 = dplyr::if_else(abs(Pine_diff1_H) > abs(Pine_diff1_M), 'Historic', 'Modern'),
                hm_worse_spruce1 = dplyr::if_else(abs(Spruce_diff1_H) > abs(Spruce_diff1_M), 'Historic', 'Modern'),
                hm_worse_tamarack1 = dplyr::if_else(abs(Tamarack_diff1_H) > abs(Tamarack_diff1_M), 'Historic', 'Modern'),
                hm_worse_walnut1 = dplyr::if_else(abs(Walnut_diff1_H) > abs(Walnut_diff1_M), 'Historic', 'Modern'),
                hm_worse_oh1 = dplyr::if_else(abs(oh_diff1_H) > abs(oh_diff1_M), 'Historic', 'Modern'),
                hm_worse_gum1 = dplyr::if_else(abs(gum_diff1_H) > abs(gum_diff1_M), 'Historic', 'Modern'),
                hm_worse_cedar1 = dplyr::if_else(abs(cedar_diff1_H) > abs(cedar_diff1_M), 'Historic', 'Modern'),
                hm_worse_poplar1 = dplyr::if_else(abs(poplar_diff1_H) > abs(poplar_diff1_M), 'Historic', 'Modern'),
                h_sign_ash1 = dplyr::if_else(Ash_diff1_H > 0, 'under', 'over'),
                h_sign_basswood1 = dplyr::if_else(Basswood_diff1_H > 0, 'under', 'over'),
                h_sign_beech1 = dplyr::if_else(Beech_diff1_H > 0, 'under', 'over'),
                h_sign_birch1 = dplyr::if_else(Birch_diff1_H > 0, 'under', 'over'),
                h_sign_cherry1 = dplyr::if_else(Cherry_diff1_H > 0, 'under', 'over'),
                h_sign_dogwood1 = dplyr::if_else(Dogwood_diff1_H > 0, 'under', 'over'),
                h_sign_elm1 = dplyr::if_else(Elm_diff1_H > 0, 'under', 'over'),
                h_sign_fir1 = dplyr::if_else(Fir_diff1_H > 0, 'under', 'over'),
                h_sign_hemlock1 = dplyr::if_else(Hemlock_diff1_H > 0, 'under', 'over'),
                h_sign_hickory1 = dplyr::if_else(Hickory_diff1_H > 0, 'under', 'over'),
                h_sign_ironwood1 = dplyr::if_else(Ironwood_diff1_H > 0, 'under', 'over'),
                h_sign_maple1 = dplyr::if_else(Maple_diff1_H > 0, 'under', 'over'),
                h_sign_oak1 = dplyr::if_else(Oak_diff1_H > 0, 'under', 'over'),
                h_sign_pine1 = dplyr::if_else(Pine_diff1_H > 0, 'under', 'over'),
                h_sign_spruce1 = dplyr::if_else(Spruce_diff1_H > 0, 'under', 'over'),
                h_sign_tamarack1 = dplyr::if_else(Tamarack_diff1_H > 0, 'under', 'over'),
                h_sign_walnut1 = dplyr::if_else(Walnut_diff1_H > 0, 'under', 'over'),
                h_sign_oh1 = dplyr::if_else(oh_diff1_H > 0, 'under', 'over'),
                h_sign_gum1 = dplyr::if_else(gum_diff1_H > 0, 'under', 'over'),
                h_sign_cedar1 = dplyr::if_else(cedar_diff1_H > 0, 'under', 'over'),
                h_sign_poplar1 = dplyr::if_else(poplar_diff1_H > 0, 'under', 'over'),
                m_sign_ash1 = dplyr::if_else(Ash_diff1_M > 0, 'under', 'over'),
                m_sign_basswood1 = dplyr::if_else(Basswood_diff1_M > 0, 'under', 'over'),
                m_sign_beech1 = dplyr::if_else(Beech_diff1_M > 0, 'under', 'over'),
                m_sign_birch1 = dplyr::if_else(Birch_diff1_M > 0, 'under', 'over'),
                m_sign_cherry1 = dplyr::if_else(Cherry_diff1_M > 0, 'under', 'over'),
                m_sign_dogwood1 = dplyr::if_else(Dogwood_diff1_M > 0, 'under', 'over'),
                m_sign_elm1 = dplyr::if_else(Elm_diff1_M > 0, 'under', 'over'),
                m_sign_fir1 = dplyr::if_else(Fir_diff1_M > 0, 'under', 'over'),
                m_sign_hemlock1 = dplyr::if_else(Hemlock_diff1_M > 0, 'under', 'over'),
                m_sign_hickory1 = dplyr::if_else(Hickory_diff1_M > 0, 'under', 'over'),
                m_sign_ironwood1 = dplyr::if_else(Ironwood_diff1_M > 0, 'under', 'over'),
                m_sign_maple1 = dplyr::if_else(Maple_diff1_M > 0, 'under', 'over'),
                m_sign_oak1 = dplyr::if_else(Oak_diff1_M > 0, 'under', 'over'),
                m_sign_pine1 = dplyr::if_else(Pine_diff1_M > 0, 'under', 'over'),
                m_sign_spruce1 = dplyr::if_else(Spruce_diff1_M > 0, 'under', 'over'),
                m_sign_tamarack1 = dplyr::if_else(Tamarack_diff1_M > 0, 'under', 'over'),
                m_sign_walnut1 = dplyr::if_else(Walnut_diff1_M > 0, 'under', 'over'),
                m_sign_oh1 = dplyr::if_else(oh_diff1_M > 0, 'under', 'over'),
                m_sign_gum1 = dplyr::if_else(gum_diff1_M > 0, 'under', 'over'),
                m_sign_cedar1 = dplyr::if_else(cedar_diff1_M > 0, 'under', 'over'),
                m_sign_poplar1 = dplyr::if_else(poplar_diff1_M > 0, 'under', 'over'))

## Plot absolute change in prediction accuracy with main model for each taxon

# Ash
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_ash1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_ash.png',
                height = 10, width = 10, units = 'cm')

# Basswood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_basswood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_basswood.png',
                height = 10, width = 10, units = 'cm')

# Beech
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_beech1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_beech.png',
                height = 10, width = 10, units = 'cm')

# Birch
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_birch1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_birch.png',
                height = 10, width = 10, units = 'cm')

# Cherry
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_cherry1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_cherry.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_dogwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_dogwood.png',
                height = 10, width = 10, units = 'cm')

# Elm
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_elm1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_elm.png',
                height = 10, width = 10, units = 'cm')

# Fir
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_fir1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_fir.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_hemlock1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_hemlock.png',
                height = 10, width = 10, units = 'cm')

# Hickory
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_hickory1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_hickory.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_ironwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_ironwood.png',
                height = 10, width = 10, units = 'cm')

# Maple
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_maple1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_maple.png',
                height = 10, width = 10, units = 'cm')

# Oak
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_oak1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_oak.png',
                height = 10, width = 10, units = 'cm')

# Pine
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_pine1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_pine.png',
                height = 10, width = 10, units = 'cm')

# Spruce
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_spruce1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_spruce.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_tamarack1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_tamarack.png',
                height = 10, width = 10, units = 'cm')

# Walnut
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_walnut1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_walnut.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_oh1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_oh.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_gum1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_gum.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_cedar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_cedar.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_poplar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔObserved -\npredicted',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Difference in prediction accuracy between\nhistoric and modern periods') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_poplar.png',
                height = 10, width = 10, units = 'cm')

## Plot with change in direction:
## negative = modern worse, positive = historic worse

# Ash
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_ash1 == 'Historic', 1, -1),
                delta = delta_accuracy_ash1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_ash2.png',
                height = 10, width = 10, units = 'cm')

# Basswood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_basswood1 == 'Historic', 1, -1),
                delta = delta_accuracy_basswood1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_basswood2.png',
                height = 10, width = 10, units = 'cm')

# Beech
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_beech1 == 'Historic', 1, -1),
                delta = delta_accuracy_beech1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_beech2.png',
                height = 10, width = 10, units = 'cm')

# Birch
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_birch1 == 'Historic', 1, -1),
                delta = delta_accuracy_birch1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_birch2.png',
                height = 10, width = 10, units = 'cm')

# Cherry
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_cherry1 == 'Historic', 1, -1),
                delta = delta_accuracy_cherry1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_cherry2.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_dogwood1 == 'Historic', 1, -1),
                delta = delta_accuracy_dogwood1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_dogwood2.png',
                height = 10, width = 10, units = 'cm')

# Elm
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_elm1 == 'Historic', 1, -1),
                delta = delta_accuracy_elm1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_elm2.png',
                height = 10, width = 10, units = 'cm')

# Fir
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_fir1 == 'Historic', 1, -1),
                delta = delta_accuracy_fir1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_fir2.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_hemlock1 == 'Historic', 1, -1),
                delta = delta_accuracy_hemlock1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_hemlock2.png',
                height = 10, width = 10, units = 'cm')

# Hickory
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_hickory1 == 'Historic', 1, -1),
                delta = delta_accuracy_hickory1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_hickory2.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_ironwood1 == 'Historic', 1, -1),
                delta = delta_accuracy_ironwood1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_ironwood2.png',
                height = 10, width = 10, units = 'cm')

# Maple
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_maple1 == 'Historic', 1, -1),
                delta = delta_accuracy_maple1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_maple2.png',
                height = 10, width = 10, units = 'cm')

# Oak
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_oak1 == 'Historic', 1, -1),
                delta = delta_accuracy_oak1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_oak2.png',
                height = 10, width = 10, units = 'cm')

# Pine
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_pine1 == 'Historic', 1, -1),
                delta = delta_accuracy_pine1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_pine2.png',
                height = 10, width = 10, units = 'cm')

# Spruce
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_spruce1 == 'Historic', 1, -1),
                delta = delta_accuracy_spruce1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_spruce2.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_tamarack1 == 'Historic', 1, -1),
                delta = delta_accuracy_tamarack1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_tamarack2.png',
                height = 10, width = 10, units = 'cm')

# Walnut
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_walnut1 == 'Historic', 1, -1),
                delta = delta_accuracy_walnut1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_walnut2.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_oh1 == 'Historic', 1, -1),
                delta = delta_accuracy_oh1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_oh2.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_gum1 == 'Historic', 1, -1),
                delta = delta_accuracy_gum1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_gum2.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_cedar1 == 'Historic', 1, -1),
                delta = delta_accuracy_cedar1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_cedar2.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_poplar1 == 'Historic', 1, -1),
                delta = delta_accuracy_poplar1 * worse) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_poplar2.png',
                height = 10, width = 10, units = 'cm')

## Plot change with direction and whether density was over or underpredicted in each period

# Ash
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_ash1 == 'Historic', 1, -1),
                delta = delta_accuracy_ash1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_ash1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_ash1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_ash3.png',
                height = 20, width = 20, units = 'cm')

# Basswood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_basswood1 == 'Historic', 1, -1),
                delta = delta_accuracy_basswood1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_basswood1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_basswood1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_basswood3.png',
                height = 20, width = 20, units = 'cm')

# Beech
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_beech1 == 'Historic', 1, -1),
                delta = delta_accuracy_beech1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_beech1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_beech1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_beech3.png',
                height = 20, width = 20, units = 'cm')

# Birch
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_birch1 == 'Historic', 1, -1),
                delta = delta_accuracy_birch1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_birch1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_birch1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_birch3.png',
                height = 20, width = 20, units = 'cm')

# Cherry
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_cherry1 == 'Historic', 1, -1),
                delta = delta_accuracy_cherry1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_cherry1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_cherry1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_cherry3.png',
                height = 20, width = 20, units = 'cm')

# Dogwood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_dogwood1 == 'Historic', 1, -1),
                delta = delta_accuracy_dogwood1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_dogwood1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_dogwood1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_dogwood3.png',
                height = 20, width = 20, units = 'cm')

# Elm
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_elm1 == 'Historic', 1, -1),
                delta = delta_accuracy_elm1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_elm1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_elm1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_elm3.png',
                height = 20, width = 20, units = 'cm')

# Fir
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_fir1 == 'Historic', 1, -1),
                delta = delta_accuracy_fir1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_fir1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_fir1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_fir3.png',
                height = 20, width = 20, units = 'cm')

# Hemlock
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_hemlock1 == 'Historic', 1, -1),
                delta = delta_accuracy_hemlock1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_hemlock1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_hemlock1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_hemlock3.png',
                height = 20, width = 20, units = 'cm')

# Hickory
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_hickory1 == 'Historic', 1, -1),
                delta = delta_accuracy_hickory1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_hickory1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_hickory1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_hickory3.png',
                height = 20, width = 20, units = 'cm')

# Ironwood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_ironwood1 == 'Historic', 1, -1),
                delta = delta_accuracy_ironwood1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_ironwood1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_ironwood1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_ironwood3.png',
                height = 20, width = 20, units = 'cm')

# Maple
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_maple1 == 'Historic', 1, -1),
                delta = delta_accuracy_maple1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_maple1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_maple1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_maple3.png',
                height = 20, width = 20, units = 'cm')

# Oak
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_oak1 == 'Historic', 1, -1),
                delta = delta_accuracy_oak1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_oak1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_oak1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Observed -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_oak3.png',
                height = 20, width = 20, units = 'cm')

# Pine
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_pine1 == 'Historic', 1, -1),
                delta = delta_accuracy_pine1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_pine1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_pine1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_pine3.png',
                height = 20, width = 20, units = 'cm')

# Spruce
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_spruce1 == 'Historic', 1, -1),
                delta = delta_accuracy_spruce1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_spruce1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_spruce1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_spruce3.png',
                height = 20, width = 20, units = 'cm')

# Tamarack
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_tamarack1 == 'Historic', 1, -1),
                delta = delta_accuracy_tamarack1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_tamarack1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_tamarack1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_tamarack3.png',
                height = 20, width = 20, units = 'cm')

# Other hardwood
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_oh1 == 'Historic', 1, -1),
                delta = delta_accuracy_oh1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_oh1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_oh1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_oh3.png',
                height = 20, width = 20, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_gum1 == 'Historic', 1, -1),
                delta = delta_accuracy_gum1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_gum1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_gum1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_gum3.png',
                height = 20, width = 20, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_cedar1 == 'Historic', 1, -1),
                delta = delta_accuracy_cedar1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_cedar1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_cedar1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_cedar3.png',
                height = 20, width = 20, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::drop_na() |>
  dplyr::mutate(worse = dplyr::if_else(hm_worse_poplar1 == 'Historic', 1, -1),
                delta = delta_accuracy_poplar1 * worse) |>
  dplyr::mutate(h_sign1 = dplyr::if_else(h_sign_poplar1 == 'under', 'Underprediction in\nhistoric period', 'Overprediction in\nhistoric period'),
                m_sign1 = dplyr::if_else(m_sign_poplar1 == 'under', 'Underprediction in\nmodern period', 'Overprediction in\nmodern period')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'ΔObserved -\npredicted',
                                limits = c(-2, 2)) +
  ggplot2::facet_grid(h_sign1~m_sign1) +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Change in prediction accuracy from historic to modern period\nPositive = better prediction in modern period\nNegative = worse prediction in modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 strip.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_pred-obs_allcovar_poplar3.png',
                height = 20, width = 20, units = 'cm')

## Where was historical vs modern prediction worse?

# Ash
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_ash1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_ash.png',
                width = 10, height = 10, units = 'cm')

# Basswood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_basswood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_basswood.png',
                width = 10, height = 10, units = 'cm')

# Beech
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_beech1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_beech.png',
                width = 10, height = 10, units = 'cm')

# Birch
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_birch1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_birch.png',
                width = 10, height = 10, units = 'cm')

# Cherry
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_cherry1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_cherry.png',
                width = 10, height = 10, units = 'cm')

# Dogwood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_dogwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_dogwood.png',
                width = 10, height = 10, units = 'cm')

# Elm
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_elm1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_elm.png',
                width = 10, height = 10, units = 'cm')

# Fir
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_fir1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_fir.png',
                width = 10, height = 10, units = 'cm')

# Hemlock
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_hemlock1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_hemlock.png',
                width = 10, height = 10, units = 'cm')

# Hickory
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_hickory1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Whether prediction is worse\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_hickory.png',
                width = 10, height = 10, units = 'cm')

# Ironwood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_ironwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_ironwood.png',
                width = 10, height = 10, units = 'cm')

# Maple
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_maple1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_maple.png',
                width = 10, height = 10, units = 'cm')

# Oak
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_oak1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_oak.png',
                width = 10, height = 10, units = 'cm')

# Pine
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_pine1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_pine.png',
                width = 10, height = 10, units = 'cm')

# Spruce
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_spruce1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_spruce.png',
                width = 10, height = 10, units = 'cm')

# Tamarack
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_tamarack1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_tamarack.png',
                width = 10, height = 10, units = 'cm')

# Walnut
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_walnut1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_walnut.png',
                width = 10, height = 10, units = 'cm')

# Other hardwood
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_oh1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_oh.png',
                width = 10, height = 10, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_gum1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_gum.png',
                width = 10, height = 10, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_cedar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_cedar.png',
                width = 10, height = 10, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hm_worse_poplar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_discrete(name = '') +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Whether prediction is better\nin historical or modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_which_better_accuracy_poplar.png',
                width = 10, height = 10, units = 'cm')

## Plot difference in accuracy where historic is worse

# Ash
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_ash1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_ash1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_ash.png',
                height = 10, width = 10, units = 'cm')

# Basswood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_basswood1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_basswood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_basswood.png',
                height = 10, width = 10, units = 'cm')

# Beech
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_beech1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_beech1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_beech.png',
                height = 10, width = 10, units = 'cm')

# Birch
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_birch1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_birch1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_birch.png',
                height = 10, width = 10, units = 'cm')

# Cherry
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_cherry1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_cherry1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_cherry.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_dogwood1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_dogwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_dogwood.png',
                height = 10, width = 10, units = 'cm')

# Elm
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_elm1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_elm1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_elm.png',
                height = 10, width = 10, units = 'cm')

# Fir
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_fir1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_fir1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_fir.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_hemlock1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_hemlock1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_hemlock.png',
                height = 10, width = 10, units = 'cm')

# Hickory
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_hickory1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_hickory1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_hickory.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_ironwood1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_ironwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_ironwood.png',
                height = 10, width = 10, units = 'cm')

# Maple
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_maple1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_maple1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_maple.png',
                height = 10, width = 10, units = 'cm')

# Oak
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_oak1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_oak1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_oak.png',
                height = 10, width = 10, units = 'cm')

# Pine
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_pine1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_pine1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_pine.png',
                height = 10, width = 10, units = 'cm')

# Spruce
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_spruce1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_spruce1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_spruce.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_tamarack1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_tamarack1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_tamarack.png',
                height = 10, width = 10, units = 'cm')

# Walnut
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_walnut1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_walnut1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_walnut.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_oh1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_oh1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 1)) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_oh.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_gum1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_gum1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_gum.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_cedar1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_cedar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_cedar.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_poplar1 == 'Historic') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_poplar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Change in prediction accuracy where\nmodern prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_modern_better_poplar.png',
                height = 10, width = 10, units = 'cm')

## Plot difference in accuracy where modern is worse

# Ash
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_ash1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_ash1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Ash',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_ash.png',
                height = 10, width = 10, units = 'cm')

# Basswood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_basswood1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_basswood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Basswood',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_basswood.png',
                height = 10, width = 10, units = 'cm')

# Beech
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_beech1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_beech1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Beech',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_beech.png',
                height = 10, width = 10, units = 'cm')

# Birch
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_birch1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_birch1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Birch',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_birch.png',
                height = 10, width = 10, units = 'cm')

# Cherry
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_cherry1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_cherry1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Cherry',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_cherry.png',
                height = 10, width = 10, units = 'cm')

# Dogwood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_dogwood1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_dogwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Dogwood',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_dogwood.png',
                height = 10, width = 10, units = 'cm')

# Elm
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_elm1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_elm1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Elm',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_elm.png',
                height = 10, width = 10, units = 'cm')

# Fir
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_fir1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_fir1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Fir',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_fir.png',
                height = 10, width = 10, units = 'cm')

# Hemlock
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_hemlock1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_hemlock1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Hemlock',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_hemlock.png',
                height = 10, width = 10, units = 'cm')

# Hickory
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_hickory1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_hickory1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Hickory',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_hickory.png',
                height = 10, width = 10, units = 'cm')

# Ironwood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_ironwood1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_ironwood1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Ironwood',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_ironwood.png',
                height = 10, width = 10, units = 'cm')

# Maple
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_maple1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_maple1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Maple',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_maple.png',
                height = 10, width = 10, units = 'cm')

# Oak
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_oak1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_oak1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Oak',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_oak.png',
                height = 10, width = 10, units = 'cm')

# Pine
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_pine1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_pine1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Pine',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_pine.png',
                height = 10, width = 10, units = 'cm')

# Spruce
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_spruce1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_spruce1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Spruce',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_spruce.png',
                height = 10, width = 10, units = 'cm')

# Tamarack
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_tamarack1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_tamarack1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Tamarack',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_tamarack.png',
                height = 10, width = 10, units = 'cm')

# Walnut
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_walnut1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_walnut1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Walnut',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_walnut.png',
                height = 10, width = 10, units = 'cm')

# Other hardwood
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_oh1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_oh1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_oh.png',
                height = 10, width = 10, units = 'cm')

# Black gum/sweet gum
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_gum1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_gum1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Black gum/sweet gum',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_gum.png',
                height = 10, width = 10, units = 'cm')

# Cedar/juniper
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_cedar1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_cedar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Cedar/juniper',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_cedar.png',
                height = 10, width = 10, units = 'cm')

# Poplar/tulip poplar
preds |>
  tidyr::drop_na() |>
  dplyr::filter(hm_worse_poplar1 == 'Modern') |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = delta_accuracy_poplar1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'ΔFraction\ntotal stems',
                                palette = 'Reds',
                                direction = 1,
                                limits = c(0, 2)) +
  ggplot2::ggtitle('Poplar/tulip poplar',
                   subtitle = 'Change in prediction accuracy where\nhistoric prediction is better') +
  ggplot2::theme_void() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/comp_change_historic_better_poplar.png',
                height = 10, width = 10, units = 'cm')

#### 13. Compare r ####

# Rename columns for historical and modern predictions
colnames(historical_cors) <- c('taxon', 'corr1_historical',
                               'corr2_historical', 'corr3_historical',
                               'corr4_historical')
colnames(modern_cors) <- c('taxon', 'corr1_modern',
                           'corr2_modern', 'corr3_modern',
                           'corr4_modern')

# Combine
both_cors <- historical_cors |>
  dplyr::full_join(y = modern_cors,
                   by = 'taxon') |>
  dplyr::mutate(corr1_historical = as.numeric(corr1_historical),
                corr1_modern = as.numeric(corr1_modern))

## Plot comparison of correlation coefficient

both_cors |>
  tidyr::pivot_longer(corr1_historical:corr4_modern,
                      names_to = 'fit_period',
                      values_to = 'correlation') |>
  dplyr::mutate(fit = sub(pattern = '_.*', replacement = '', x = fit_period),
                period = sub(pattern = '.*_', replacement = '', x = fit_period),
                fit = dplyr::if_else(fit == 'corr1', 'Climate + soil', fit),
                fit = dplyr::if_else(fit == 'corr2', 'Climate only', fit),
                fit = dplyr::if_else(fit == 'corr3', 'Four covariates', fit),
                fit = dplyr::if_else(fit == 'corr4', 'Climate + soil +\ncoordinates', fit),
                period = dplyr::if_else(period == 'historical', 'Historical', 'Modern')) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = fit, y = correlation,
                                 fill = period),
                    stat = 'identity',
                    position = ggplot2::position_dodge(width = 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation coefficient') +
  ggplot2::scale_fill_discrete(name = 'Prediction period') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8, angle = 90),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/rf/H/abundance/pred/pred_corr_comparison.png',
                height = 20, width = 40, units = 'cm')
