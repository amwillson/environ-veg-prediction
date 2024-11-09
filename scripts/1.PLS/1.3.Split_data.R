#### STEP 1-3

## Subsetting in-sample and out-of-sample data in PLS
## Out-of-sample grid cells should have corresponding grid cells in FIA

## 1. Load data
## 2. Combine grids
## 3. Define sample sizes
## 4. Select out-of-sample
## 5. Save

## Input: data/processed/PLS/xydata.RData
## PLS combined dataset with vegetation, climate, and soil data
## This is the dataframe we are splitting between in-sample and oos

## Input: data/processed/FIA/gridded_all_plots.RData
## FIA vegetation dataset
## Contains the grid cells that are available in the modern (FIA) period
## Used to select corresponding grid cells in both time periods

## Output: data/processed/PLS/xydata_in.RData
## PLS-era vegetation, climate, and soil data for only in-sample grid cells
## Used in 4.1.fit_density_allcovar.R, 4.2.fit_density_climcovar.R,
## 4.3.fit_density_redcovar.R, 4.4.fit_density_xycovar.R,
## 4.8.fit_abundance_allcovar.R, 4.9.fit_abundance_climcovar.R,
## 4.10.fit_abundance_redcovar.R, 4.11.fit_abundance_xycovar.R

## Output: data/processed/PLS/xydata_out.RData
## PLS-era vegetation, climate, and soil data for only out-of-sample grid cells
## Used in 4.5.density_historical_predictions.R,
## 4.6.density_modern_predictions.R, 4.12.abundance_historical_predictions.R,
## 4.13.abundance_modern_predictions.R, 5.5.density_historical_predictions.R,
## 5.6.density_modern_predictions.R, 5.12.abundance_historical_predictions.R,
## 5.13.abundance_modern_predictions.R

rm(list = ls())

# Set seed
set.seed(1)

#### 1. Load data ####

# Load collated PLS data
load('data/processed/PLS/xydata.RData')

# Load FIA gridded data
# (no environmental variables but that doesn't matter)
load('data/processed/FIA/gridded_all_plots.RData')

# Coordinate pairs
pls_coords <- xydata |>
  dplyr::select(x, y) |>
  dplyr::distinct()

fia_coords <- stem_density_agg2 |>
  dplyr::ungroup() |>
  dplyr::select(x, y) |>
  dplyr::distinct()

#### 2. Combine grids ####

# PLS and FIA densities
pls_density <- xydata |>
  dplyr::select(x, y, total_density) |>
  dplyr::distinct() |>
  dplyr::rename(pls_density = total_density)

nrow(pls_density) == nrow(pls_coords) # should BE TRUE

fia_density <- stem_density_agg2 |>
  dplyr::ungroup() |>
  dplyr::select(x, y, total_stem_density) |>
  dplyr::distinct() |>
  dplyr::rename(fia_density = total_stem_density)

nrow(fia_density) == nrow(fia_coords) # should be TRUE

# Combine PLS and FIA to find overlap in grid cells
pls_fia <- pls_density |>
  dplyr::full_join(y = fia_density,
                   by = c('x', 'y'))

#### 3. Define sample sizes ####

# Number of grid cells in both datasets
n_pls_fia <- length(which(!is.na(pls_fia$pls_density) & !is.na(pls_fia$fia_density)))
# Number of grid cells in PLS only
n_pls_only <- length(which(!is.na(pls_fia$pls_density) & is.na(pls_fia$fia_density)))
# Number of grid cells in FIA only
n_fia_only <- length(which(is.na(pls_fia$pls_density) & !is.na(pls_fia$fia_density)))

n_fia_only + n_pls_only + n_pls_fia == nrow(pls_fia) # should be TRUE

# Number of grid cells in PLS
n_pls <- n_pls_fia + n_pls_only
# Number of grid cells in FIA
n_fia <- n_pls_fia + n_fia_only

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot spatial distribution of each dataset
pls_fia |>
  dplyr::mutate(dataset = dplyr::if_else(!is.na(pls_density) & !is.na(fia_density), 'both', NA),
                dataset = dplyr::if_else(!is.na(pls_density) & is.na(fia_density), 'PLS', dataset),
                dataset = dplyr::if_else(is.na(pls_density) & !is.na(fia_density), 'FIA', dataset)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = dataset)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_brewer(palette = 'Dark2',
                             name = '') +
  ggplot2::theme_void()

# Number of grid cells we want in in-sample set
n_in <- round(n_pls * 0.7)
n_out <- round(n_pls * 0.3)
n_in + n_out == n_pls # should be TRUE

# Add column with pls-era ecosystems
pls_fia <- pls_fia |>
  dplyr::mutate(ecosystem = dplyr::if_else(pls_density < 0.5, 'Prairie', NA),
                ecosystem = dplyr::if_else(pls_density >= 0.5 & pls_density <= 47, 'Savanna', ecosystem),
                ecosystem = dplyr::if_else(pls_density > 47, 'Forest', ecosystem))

# Plot ecosystem distributions
pls_fia |>
  dplyr::filter(!is.na(ecosystem)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ecosystem)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_brewer(palette = 'Accent',
                             name = '') +
  ggplot2::theme_void()

# Subset PLS data with each ecosystem type
pls_fia_p <- dplyr::filter(pls_fia, !is.na(pls_density) & ecosystem == 'Prairie')
pls_fia_s <- dplyr::filter(pls_fia, !is.na(pls_density) & ecosystem == 'Savanna')
pls_fia_f <- dplyr::filter(pls_fia, !is.na(pls_density) & ecosystem == 'Forest')

nrow(pls_fia_p) + nrow(pls_fia_s) + nrow(pls_fia_f) == nrow(pls_density) # should be TRUE

# Number of cells with each ecosystem type
n_p <- nrow(pls_fia_p)
n_s <- nrow(pls_fia_s)
n_f <- nrow(pls_fia_f)

# Proportion of cells with each ecosystem type
p_p <- n_p / n_pls
p_s <- n_s / n_pls
p_f <- n_f / n_pls

# Number of grid cells of each ecosystem type we want to have in
# common between PLS and FIA for out-of-sample  dataset
nn_p <- round(n_out * p_p)
nn_s <- round(n_out * p_s)
nn_f <- round(n_out * p_f)

# Filter by grid cells occurring in both datasets
# and by ecosystem type
pls_fia_prairie <- pls_fia |>
  dplyr::filter(!is.na(pls_density) & !is.na(fia_density) &
                  ecosystem == 'Prairie')
pls_fia_savanna <- pls_fia |>
  dplyr::filter(!is.na(pls_density) & !is.na(fia_density) &
                  ecosystem == 'Savanna')
pls_fia_forest <- pls_fia |>
  dplyr::filter(!is.na(pls_density) & !is.na(fia_density) &
                  ecosystem == 'Forest')

# Actual number of grid cells of each ecosystem type in common between datasets
na_p <- nrow(pls_fia_prairie)
na_s <- nrow(pls_fia_savanna)
na_f <- nrow(pls_fia_forest)

# If the number of actual grid cells is less than the desired ones,
# we will need to pull more from the PLS dataset only
extra_p <- nn_p - na_p

#### 4. Select out-of-sample ####

# Prairie cells that match between PLS and FIA
pls_fia_prairie <- pls_fia_prairie |>
  dplyr::mutate(oos = TRUE) |>
  dplyr::select(x, y, oos)

# Extra PLS cells that don't have an FIA counterpart
pls_fia_prairie2 <- pls_fia |>
  dplyr::filter(!is.na(pls_density) & is.na(fia_density) &
                  ecosystem == 'Prairie') |>
  dplyr::slice_sample(n = extra_p, replace = FALSE) |>
  dplyr::mutate(oos = TRUE) |>
  dplyr::select(x, y, oos)

# Combine prairie samples
oos_prairie <- rbind(pls_fia_prairie, pls_fia_prairie2)

# Savanna cells randomly chosen
oos_savanna <- pls_fia_savanna |>
  dplyr::slice_sample(n = nn_s, replace = FALSE) |>
  dplyr::mutate(oos = TRUE) |>
  dplyr::select(x, y, oos)

# Forest cells randomly chosen
oos_forest <- pls_fia_forest |>
  dplyr::slice_sample(n = nn_f, replace = FALSE) |>
  dplyr::mutate(oos = TRUE) |>
  dplyr::select(x, y, oos)

nrow(oos_prairie) == nn_p # should be TRUE
nrow(oos_savanna) == nn_s # should be TRUE
nrow(oos_forest) == nn_f # should be TRUE

# Combine
oos <- rbind(oos_prairie, oos_savanna, oos_forest)

# Add to PLS dataset
pls_in_oos <- xydata |>
  dplyr::left_join(y = oos,
                   by = c('x', 'y')) |>
  dplyr::mutate(oos = dplyr::if_else(is.na(oos), FALSE, oos))

# Plot in-sample data
pls_in_oos |>
  dplyr::filter(oos == FALSE) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('In-sample grid cells') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot out-of-sample data
pls_in_oos |>
  dplyr::filter(oos == TRUE) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Out-of-sample grid cells') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot together
pls_in_oos |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oos)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('In- and out-of-sample grid cells') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Split in-sample and oos data
pls_in <- pls_in_oos |>
  dplyr::filter(oos == FALSE) |>
  dplyr::select(-oos)
pls_oos <- pls_in_oos |>
  dplyr::filter(oos == TRUE) |>
  dplyr::select(-oos)

#### 5. Save ####

# Save
save(pls_in,
     file = 'data/processed/PLS/xydata_in.RData')
save(pls_oos,
     file = 'data/processed/PLS/xydata_out.RData')
