## Reclassifying plots into PalEON grid cells using swapped/fuzzed coordinates
## The purpose is to have a rough estimate of bias and uncertainty from
## using swapped & fuzzed coordinates.
## Our findings suggest that the uncertainty could be substantial, even when
## aggregating to a gridded product. For now, we are setting this aside though
## and using the unswapped & unfuzzed data

## 1. Load and process swapped/fuzzed FIA data
## 2. Subset for the one inventory cycle used in previous PalEON project
## 3. Make PalEON regularized grid
## 4. Add grid cells that didn't exist in PalEON product that are within regularized grid
## 5. Find which grid cell each plot is closest to
## 6. Add PalEON taxon classifications
## 7. Calculate plot-level stem density
## 8. Average plots in each grid cell
## 9. Load and format previous PalEON project density
## 10. Compare gridded output from unswapped/unfuzzed and swapped/fuzzed coordinates

rm(list = ls())

#### 1. Load and process swapped/fuzzed FIA data ####

# Load swapped & fuzzed data (from main workflow)
load('data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData')

# Load unswapped & unfuzzed data (minus coordinates)
paleon_FIA <- readr::read_csv('PalEON_FIA_workflow/data/input/full_fia_long_v0.1.csv')

# Remove states we don't care about
paleon_FIA <- dplyr::filter(paleon_FIA, statecd %in% unique(TOTAL_FIA$STATECD))

# Rename columns from our swapped & fuzzed data
TOTAL_FIA <- dplyr::rename(TOTAL_FIA,
                           time = MEASYEAR,
                           plot = PLOT,
                           cycle = CYCLE,
                           subcycle = SUBCYCLE,
                           statecd = STATECD,
                           plt_cn = PLT_CN,
                           tree_cn = TREE_CN,
                           dbh = DIA,
                           spcd = SPCD,
                           tpa_unadj = TPA_UNADJ)

#### 2. Subset to include one inventory cycle ####

# Subset to only the inventories included in PalEON
# Can be accomplished by doing a full join and removing NAs from the paleon_FIA df
TOTAL_FIA_sub <- TOTAL_FIA |>
  # Join by all common columns
  dplyr::full_join(y = paleon_FIA, by = c('time', 'plot', 'cycle', 'subcycle',
                                          'statecd', 'plt_cn', 'tree_cn',
                                          'spcd', 'tpa_unadj')) |>
  # Drop NAs from column that has no NAs in the paleon_FIA df
  tidyr::drop_na(drybio_bole)

# Convert to EPSG:3175
TOTAL_FIA_sub <- sf::st_as_sf(TOTAL_FIA_sub, 
                              coords = c('LON', 'LAT'), 
                              crs = 'EPSG:4269')
TOTAL_FIA_sub <- sf::st_transform(TOTAL_FIA_sub, crs = 'EPSG:3175')
TOTAL_FIA_sub <- sfheaders::sf_to_df(TOTAL_FIA_sub, fill = TRUE)
TOTAL_FIA_sub <- dplyr::select(TOTAL_FIA_sub, -sfg_id, -point_id)

# Plot to make sure things look correct
states <- sf::st_as_sf(maps::map(database = 'state', 
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

TOTAL_FIA_sub |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Unique plots
unique_plots <- TOTAL_FIA_sub |>
  dplyr::select(plt_cn, x, y) |>
  dplyr::distinct()

# Remove paleon data
rm(paleon_FIA)

#### 3. Make PalEON grid ####

# Read in grid cell coordinates
# Grid cell assignment was conducted using unswapped/unfuzzed data
paleon_coords <- readr::read_csv('PalEON_FIA_workflow/data/conversions/fia_paleongrid_albers.csv')

# Remove plot and state IDs
paleon_coords <- dplyr::select(paleon_coords, x:cell)

# Sequence of full grid coordinates in x and y directions
all_x <- seq(from = min(paleon_coords$x), to = max(paleon_coords$x), by = 8000)
all_y <- seq(from = min(paleon_coords$y), to = max(paleon_coords$y), by = 8000)

# Grid with all possible grid cells
all_grid <- expand.grid(all_x, all_y)
colnames(all_grid) <- c('x', 'y')

# Add grid ID numbers for grid cells that already have them
all_grid <- all_grid |>
  dplyr::left_join(y = paleon_coords,
                   by = c('x', 'y')) |>
  dplyr::distinct()

# Current grid IDs
grid_ids <- unique(all_grid$cell)

#### 4. Add grid cells that didn't exist ####

# Make new grid IDs that definitely don't overlap the current ones
# to fill in the gaps
new_grid_ids <- seq(from = (max(grid_ids, na.rm = TRUE) + 1),
                    by = 1,
                    length.out = length(which(is.na(all_grid$cell))))

# Add new grid ID numbers to cells that currently don't have one
count <- 0
for(i in 1:nrow(all_grid)){
  if(is.na(all_grid$cell[i])){
    count <- count + 1
    all_grid$cell[i] <- new_grid_ids[count]
  }
}

# Check that there are no missing grid cell IDs
any(is.na(all_grid))
# Check that there are no duplicated grid cell IDs
any(duplicated(all_grid$cell))

#### 5. Find which grid cell each plot is closest to ####

# Subset for coordinates only
plot_coords <- dplyr::select(unique_plots, x, y)
grid_coords <- dplyr::select(all_grid, x, y)

# Find the distance between each plot and each grid centroid
dists <- fields::rdist(plot_coords, grid_coords)
# Find the closest grid centroid to each plot
closest_cell <- apply(dists, 1, which.min)
rm(dists)

# Make new dataframe of corresponding grid cells to the PLS points
unique_plots_cell <- all_grid |>
  dplyr::rename(grid_x = x,
                grid_y = y) |>
  dplyr::slice(closest_cell)
unique_plots_cell <- cbind(unique_plots, unique_plots_cell)
unique_plots_cell <- dplyr::rename(unique_plots_cell,
                                   plot_x = x,
                                   plot_y = y)

# Are the plot coordinates within the grid cell?
unique_plots_cell2 <- unique_plots_cell |>
  dplyr::mutate(cell_in = dplyr::if_else(plot_x >= grid_x - 4000 & plot_x <= grid_x + 4000 &
                                           plot_y >= grid_y - 4000 & plot_y <= grid_y + 4000,
                                         'yes', 'no'))
length(which(unique_plots_cell2$cell_in == 'no'))

# Join to data
grid_TOTAL_FIA <- TOTAL_FIA_sub |>
  dplyr::rename(plot_x = x,
                plot_y = y) |>
  dplyr::left_join(y = unique_plots_cell,
                   by = c('plt_cn', 'plot_x',
                          'plot_y'))

#### 6. Add PalEON taxon classifications ####

paleon_taxa <- readr::read_csv('PalEON_FIA_workflow/data/conversions/FIA_conversion_v03.csv')

grid_TOTAL_FIA_taxon <- paleon_taxa |>
  dplyr::select(spcd, PalEON) |>
  dplyr::right_join(y = grid_TOTAL_FIA, by = 'spcd') |>
  dplyr::rename(taxon = PalEON)

#### 7. Calculate plot-level stem density ####

# Calculate density per plot
stem_density <- grid_TOTAL_FIA_taxon |>
  # Count number of trees
  dplyr::count(statecd, # in the same state
               cell, # in the same grid cell
               grid_x, grid_y, # in the same plot
               plt_cn, # in the same plot
               taxon, # that are the same PalEON taxon
               spcd, # that are the same species
               tpa_unadj) |> # with the same tree per acre conversion
  # Calculate density = number of trees * tree per acre conversion * acre to hectare conversion
  dplyr::mutate(ind_density = n * tpa_unadj * (1/0.404686)) |>
  # Group by same variables to retain them 
  # (except tpa and ind. species because we need to sum over)
  dplyr::group_by(statecd, cell, grid_x, grid_y,
                  plt_cn, taxon) |>
  # Sum density of trees of the same species and plot
  # across conversion factors
  dplyr::summarize(density = sum(ind_density))

# Total stem density
total_stem_density <- stem_density |>
  dplyr::group_by(plt_cn) |>
  dplyr::summarize(total_density = sum(density))

# Add to taxon-level dataframe
stem_density <- stem_density |>
  dplyr::left_join(y = total_stem_density,
                   by = 'plt_cn')

#### 8. Average plots in each grid cell ####

# Stem density
stem_density_agg <- stem_density |>
  # Group by
  dplyr::group_by(statecd, # state
                  cell, # grid cell
                  grid_x, grid_y, # coordinates
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(density))

# Total stem density
total_stem_density_agg <- stem_density_agg |>
  dplyr::group_by(cell) |>
  dplyr::summarize(total_stem_density = sum(stem_density))

stem_density_agg <- stem_density_agg |>
  dplyr::left_join(y = total_stem_density_agg,
                   by = 'cell')

# Check plots
stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9) +
  ggplot2::theme_void()

stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = total_stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9) +
  ggplot2::theme_void()

# Fractional composition at grid level
fractional_composition_agg <- stem_density_agg |>
  dplyr::mutate(fcomp = stem_density / total_stem_density)

# Plots
fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = as.factor(cell), y = fcomp, fill = taxon),
                    stat = 'identity', position = 'stack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = fcomp)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'D', direction = -1) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_void()

#### 9. Load and format previous PalEON project density ####

# Load PalEON gridded product
paleon_density <- readr::read_csv('PalEON_FIA_workflow/data/input/species_plot_parameters_v0.1.csv')

paleon_coords <- readr::read_csv('PalEON_FIA_workflow/data/conversions/fia_paleongrid_albers.csv')

# Format
paleon_density <- paleon_density |>
  # remove columns we don't care about
  dplyr::select(-dbh, -basal_area, -n)

# Add taxon classifications
paleon_density_taxon <- paleon_taxa |>
  dplyr::select(spcd, PalEON) |>
  dplyr::right_join(y = paleon_density, by = 'spcd') |>
  dplyr::rename(taxon = PalEON) |>
  # sum over species in the same taxon
  dplyr::group_by(taxon, plt_cn) |>
  dplyr::summarize(density = sum(density))

# Add grid cell ID/coordinates
grid_paleon_density <- paleon_coords |>
  dplyr::rename(statecd = STATECD,
                plt_cn = CN,
                grid_x = x,
                grid_y = y) |>
  dplyr::right_join(y = paleon_density_taxon,
                    by = 'plt_cn') |>
  # remove states we don't care about
  dplyr::filter(statecd %in% c(17, 18, 26, 27, 55))

# Average plots in each grid cell
paleon_stem_density_agg <- grid_paleon_density |>
  dplyr::group_by(statecd, # state
                  cell, # grid cell
                  grid_x, grid_y, # coordinates
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(density))

# Total stem density
paleon_total_stem_density_agg <- paleon_stem_density_agg |>
  dplyr::group_by(cell) |>
  dplyr::summarize(total_stem_density = sum(stem_density))

paleon_stem_density_agg <- paleon_stem_density_agg |>
  dplyr::left_join(y = paleon_total_stem_density_agg,
                   by = 'cell')

# Fractional composition at grid level
paleon_fractional_composition_agg <- paleon_stem_density_agg |>
  dplyr::mutate(fcomp = stem_density / total_stem_density)

#### 10. Compare gridded output from unswapped/unfuzzed and swapped/fuzzed coordinates ####

# Compare unswapped/unfuzzed with swapped/fuzzed

# Stem density
both_stem_density_agg <- stem_density_agg |>
  dplyr::full_join(y = paleon_stem_density_agg,
                   by = c('statecd', 'cell',
                          'grid_x', 'grid_y',
                          'taxon')) |>
  dplyr::rename(sf_stem_density = stem_density.x,
                usuf_stem_density = stem_density.y,
                sf_total_stem_density = total_stem_density.x,
                usuf_total_stem_density = total_stem_density.y)

# How close are they
both_stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sf_stem_density,
                                   y = usuf_stem_density,
                                   color = taxon),
                      alpha = 0.5) +
  ggplot2::geom_abline() +
  ggplot2::theme_minimal()

both_stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sf_total_stem_density,
                                   y = usuf_total_stem_density),
                      alpha = 0.5) +
  ggplot2::geom_abline() +
  ggplot2::theme_minimal()

both_stem_density_agg |>
  dplyr::mutate(diff = abs(sf_stem_density - usuf_stem_density),
                diff = dplyr::if_else(is.na(sf_stem_density), usuf_stem_density, diff),
                diff = dplyr::if_else(is.na(usuf_stem_density), sf_stem_density, diff)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_distiller(palette = 'Blues', direction = 1, transform = 'sqrt') +
  ggplot2::theme_void()

both_stem_density_agg |>
  dplyr::select(grid_x, grid_y, sf_total_stem_density, usuf_total_stem_density) |>
  dplyr::distinct() |>
  dplyr::mutate(diff = abs(sf_total_stem_density - usuf_total_stem_density),
                diff = dplyr::if_else(is.na(sf_total_stem_density), usuf_total_stem_density, diff),
                diff = dplyr::if_else(is.na(usuf_total_stem_density), sf_total_stem_density, diff)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Blues', direction = 1, transform = 'sqrt') +
  ggplot2::theme_void()

# Fractional composition
both_fractional_composition_agg <- fractional_composition_agg |>
  dplyr::full_join(y = paleon_fractional_composition_agg,
                   by = c('statecd', 'cell',
                          'grid_x', 'grid_y',
                          'taxon')) |>
  dplyr::rename(sf_fcomp = fcomp.x,
                usuf_fcomp = fcomp.y)

# How close are they
both_fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sf_fcomp,
                                   y = usuf_fcomp,
                                   color = taxon),
                      alpha = 0.5) +
  ggplot2::geom_abline() +
  ggplot2::theme_minimal()

both_fractional_composition_agg |>
  dplyr::mutate(diff = abs(sf_fcomp - usuf_fcomp),
                diff = dplyr::if_else(is.na(sf_fcomp), usuf_fcomp, diff),
                diff = dplyr::if_else(is.na(usuf_fcomp), sf_fcomp, diff)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_distiller(palette = 'Blues', direction = 1, transform = 'sqrt') +
  ggplot2::theme_void()

# Save
save(both_stem_density_agg, both_fractional_composition_agg,
     file = 'PalEON_FIA_workflow/data/intermediate/8km_sf_usuf_parameters.RData')
