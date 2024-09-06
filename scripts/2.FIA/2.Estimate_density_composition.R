## Linking plots through time
## Need to ID the same plots over time to be able to use PalEON grid cell assignments

rm(list = ls())

# Load data
load('data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData')

# Add unique coordinates column
TOTAL_FIA <- TOTAL_FIA |>
  dplyr::mutate(loc = paste0(LAT, '_', LON))

# Load PalEON coordinates & grid mapping
paleon_coords <- readr::read_csv('PalEON_FIA_workflow/data/conversions/fia_paleongrid_albers.csv')

# Match FIA plots to PalEON grid cells
matched_plots <- TOTAL_FIA |>
  dplyr::select(PLT_CN, LAT, LON, loc, STATECD) |>
  dplyr::rename(CN = PLT_CN) |>
  dplyr::left_join(y = paleon_coords,
                   by = c('STATECD', 'CN')) |>
  tidyr::drop_na() |>
  dplyr::select(-CN, -LAT, -LON, -STATECD) |>
  dplyr::distinct()

# Use FIA coordinates to match any plots with the same coordinates to the grid cell
TOTAL_FIA_matched <- TOTAL_FIA |>
  dplyr::left_join(y = matched_plots,
                   by = 'loc')

# Take every plot that has been matched
yes_matched <- TOTAL_FIA_matched |>
  dplyr::filter(!is.na(x))

# Coordinates for plots that have been matched
yes_matched_coords <- yes_matched |>
  dplyr::select(LAT, LON) |>
  dplyr::distinct()

# Take every plot that has NOT been matched
no_matched <- TOTAL_FIA_matched |>
  dplyr::filter(is.na(x))

# Coordinates for plots that have not been matched
no_matched_coords <- no_matched |>
  dplyr::select(LAT, LON) |>
  dplyr::distinct()

# Look at where the plots are located
states <- sf::st_as_sf(maps::map(database = 'state', regions = c('illinois', 'indiana',
                                                                 'michigan', 'minnesota',
                                                                 'wisconsin'),
                                 fill = TRUE, plot = FALSE))

p1 <- yes_matched_coords |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = LON, y = LAT), alpha = 0.5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Plots assigned to grid cells') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
p1

p2 <- no_matched_coords |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = LON, y = LAT), alpha = 0.5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Plots not assigned to grid cells') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
p2

cowplot::plot_grid(p1, p2, nrow = 1)

# Assign grid cells according to closest plot with grid cell assignment
dists <- fields::rdist(no_matched_coords, yes_matched_coords)
# Find closest plot with grid cell coordinates to each plot without grid cell coordinates
closest_plot <- apply(dists, 1, which.min)
# Remove dists
rm(dists)

select_yes_matched_coords <- dplyr::slice(yes_matched_coords, closest_plot)

new_matched_coords <- cbind(no_matched_coords, select_yes_matched_coords)
colnames(new_matched_coords) <- c('old_LAT', 'old_LON', 'new_LAT', 'new_LON')

no_matched2 <- no_matched |>
  dplyr::rename(old_LAT = LAT,
                old_LON = LON) |>
  dplyr::left_join(y = new_matched_coords,
                   by = c('old_LAT', 'old_LON')) |>
  dplyr::select(-old_LAT, -old_LON) |>
  dplyr::rename(LAT = new_LAT,
                LON = new_LON) |>
  dplyr::select(-x, -y, -cell, -loc) |>
  dplyr::mutate(loc = paste0(LAT, '_', LON)) |>
  dplyr::left_join(y = matched_plots,
                   by = 'loc') |>
  dplyr::select(colnames(yes_matched))

TOTAL_FIA_matched2 <- rbind(yes_matched, no_matched2)

### Now make plot-level density with and without the extra matched pltos
### Then make grid-level density and composition with and without the extra matched plots
### Compare with and without extra matched plots to present as alternatives during lab meeting