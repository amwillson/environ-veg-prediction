#### STEP 2-6

## Plotting change in climate variables over the last 100 years

## 1. Load data
## 2. Find differences
## 3. Plot differences

rm(list = ls())

#### 1. Load data ####

# Load historical climate variables
load('data/intermediate/PLS/xydata.RData')

# Load modern climate variables
load('data/intermediate/FIA/xydata.RData')

# Map of study region
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

#### 2. Find differences ####

# Extract columns of interest
historical_clim <- dplyr::select(xydata,
                                 x, y,
                                 ppt_sum:vpdmax)
modern_clim <- dplyr::select(dplyr::ungroup(xydata_modern),
                             x, y,
                             ppt_sum:vpdmax)

# Rename columns
colnames(historical_clim) <- c('x', 'y',
                               'h_ppt_sum', 'h_tmean_mean',
                               'h_ppt_sd', 'h_ppt_cv',
                               'h_tmean_sd', 'h_tmean_cv',
                               'h_tmin', 'h_tmax',
                               'h_vpdmax')
colnames(modern_clim) <- c('x', 'y',
                           'm_ppt_sum', 'm_tmean_mean',
                           'm_ppt_sd', 'm_ppt_cv',
                           'm_tmean_sd', 'm_tmean_cv',
                           'm_tmin', 'm_tmax',
                           'm_vpdmax')

# Combine
clim <- historical_clim |>
  dplyr::full_join(y = modern_clim,
                   by = c('x', 'y')) |>
  tidyr::drop_na()

# Calculate differences
clim <- clim |>
  dplyr::mutate(diff_ppt_sum = m_ppt_sum - h_ppt_sum,
                diff_tmean_mean = m_tmean_mean - h_tmean_mean,
                diff_ppt_cv = m_ppt_cv - h_ppt_cv,
                diff_tmean_sd = m_tmean_sd - h_tmean_sd,
                diff_tmin = m_tmin - h_tmin,
                diff_tmax = m_tmax - h_tmax,
                diff_vpdmax = m_vpdmax - h_vpdmax)

#### 3. Plot differences ####

## PPT
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_ppt_sum)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'mm/year',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in total annual precipitation from\nhistorical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/ppt_change.png',
                height = 10, width = 10, units = 'cm')

## TMEAN
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_tmean_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = '°C',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in mean annual temperature from\nhistorical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/tmean_change.png',
                height = 10, width = 10, units = 'cm')

## PPT CV
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_ppt_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'CV',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in precipitation seasonality from\nhistorical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/ppt_cv_change.png',
                height = 10, width = 10, units = 'cm')

## TMEAN SD
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_tmean_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'SD (°C)',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in temperature seasonality from\nhistorical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/tmean_sd_change.png',
                height = 10, width = 10, units = 'cm')

## TMIN
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_tmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = '°C',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in minimum annual temperature from\nhistorical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/tmin_change.png',
                height = 10, width = 10, units = 'cm')

## TMAX
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_tmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = '°C',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in maximum annual temperature from\nhistorical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/tmax_change.png',
                height = 10, width = 10, units = 'cm')

## VPDMAX
clim |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff_vpdmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = '°C',
                                low = scales::muted('blue'),
                                high = scales::muted('red')) +
  ggplot2::ggtitle('Change in maximum annual vapor pressure deficit\nfrom historical to modern period') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/vpdmax_change.png',
                height = 10, width = 10, units = 'cm')
