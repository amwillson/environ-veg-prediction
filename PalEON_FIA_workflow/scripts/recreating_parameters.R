## Recreating stem density and fractional composition using
## former PalEON protocol

## 1. Loading in newest version of swapped and fuzzed data and
##    unswapped and unfuzzed data from previous PalEON without coordinates
## 2. Subset swapped/fuzzed data to include only the data in
##    the PalEON set (removes other inventory cycles)
## 3. Assign grid centroid coordinates to swapped/fuzzed data using
##    mapping from previous PalEON done with unswapped/unfuzzed coordinates
## 4. Assign PalEON taxon classifications
## 5. Calculate taxon-level and total stem density using PalEON protocol
## 6. Calculate fractional composition from stem density
## 7. Average plots in each grid cell
## 8. Process PalEON output
## 9. Compare with previous PalEON products

rm(list = ls())

#### 1. Load data ####

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

#### 2. Subset swapped/fuzzed for one inventory cycle ####

# Subset to only the inventories included in PalEON
# Can be accomplished by doing a full join and removing NAs from the paleon_FIA df
TOTAL_FIA_sub <- TOTAL_FIA |>
  # Join by all common columns
  dplyr::full_join(y = paleon_FIA, by = c('time', 'plot', 'cycle', 'subcycle',
                                          'statecd', 'plt_cn', 'tree_cn',
                                          'spcd', 'tpa_unadj')) |>
  # Drop NAs from column that has no NAs in the paleon_FIA df
  tidyr::drop_na(drybio_bole)

# Plot to make sure things look correct
states <- sf::st_as_sf(maps::map(database = 'state', 
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))

states <- sf::st_transform(states, crs = 'EPSG:4269')

TOTAL_FIA_sub |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = LON, y = LAT)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Convert to EPSG:3175
TOTAL_FIA_sub <- sf::st_as_sf(TOTAL_FIA_sub, 
                              coords = c('LON', 'LAT'), 
                              crs = 'EPSG:4269')
TOTAL_FIA_sub <- sf::st_transform(TOTAL_FIA_sub, crs = 'EPSG:3175')
TOTAL_FIA_sub <- sfheaders::sf_to_df(TOTAL_FIA_sub, fill = TRUE)
TOTAL_FIA_sub <- dplyr::select(TOTAL_FIA_sub, -sfg_id, -point_id)

# Plot again
states <- sf::st_transform(states, crs = 'EPSG:3175')

TOTAL_FIA_sub |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

#### 3. Assign coordinates ####

# Read in grid cell coordinates
# Grid cell assignment was conducted using unswapped/unfuzzed data
paleon_coords <- readr::read_csv('PalEON_FIA_workflow/data/conversions/fia_paleongrid_albers.csv')

# Add grid cell ID/coordinates
grid_TOTAL_FIA_sub <- paleon_coords |>
  dplyr::rename(statecd = STATECD,
                plt_cn = CN,
                grid_x = x,
                grid_y = y) |>
  dplyr::right_join(y = TOTAL_FIA_sub,
                   by = c('statecd', 'plt_cn')) |>
  dplyr::rename(point_x = x,
                point_y = y)

# Plot grid cell x and swapped/fuzzed x against each other
grid_TOTAL_FIA_sub |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = point_x)) +
  ggplot2::geom_abline()

# Plot grid cell y and swapped/fuzzed y against each other
grid_TOTAL_FIA_sub |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_y, y = point_y)) +
  ggplot2::geom_abline()

## There are some coords that seem really far off the 1:1 line
## We expect some to be pretty far away but not THAT far

# Points more than 8000 m (= 8 km = ~5 mi)
grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  
  dplyr::filter(diff_x > 8000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  dplyr::filter(diff_y > 8000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

# Points more than 10000 m (= 10 km = ~6.2 mi)
grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  
  dplyr::filter(diff_x > 10000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  dplyr::filter(diff_y > 10000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

# Points more than 25000 m (= 25 km = ~ 15.5 mi)
grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  
  dplyr::filter(diff_x > 25000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  dplyr::filter(diff_y > 25000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

# Points more than 50000 m (= 50 km = ~31 mi)
grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  
  dplyr::filter(diff_x > 50000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  dplyr::filter(diff_y > 50000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

# Points more than 100000 m (= 100 km = ~62 mi)
#### How much does density and fractional composition change by taking out these larger distance plots?
grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  
  dplyr::filter(diff_x > 100000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  dplyr::filter(diff_y > 100000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

# Points more than 200000 m (= 200 km = ~124 mi)
grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  
  dplyr::filter(diff_x > 200000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

grid_TOTAL_FIA_sub |>
  dplyr::mutate(diff_x = abs(grid_x - point_x),
                diff_y = abs(grid_y - point_y)) |>
  dplyr::filter(diff_y > 200000) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = as.factor(plt_cn), shape = 'grid')) +
  ggplot2::geom_point(ggplot2::aes(x = point_x, y = point_y, color = as.factor(plt_cn), shape = 'point')) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')

#### 4. Assign PalEON taxon classifications ####

paleon_taxa <- readr::read_csv('PalEON_FIA_workflow/data/conversions/FIA_conversion_v03.csv')

grid_TOTAL_FIA_taxon <- paleon_taxa |>
  dplyr::select(spcd, PalEON) |>
  dplyr::right_join(y = grid_TOTAL_FIA_sub, by = 'spcd') |>
  dplyr::rename(taxon = PalEON)

#### 5. Calculate density per plot ####

stem_density <- grid_TOTAL_FIA_taxon |>
  # Count number of trees
  dplyr::count(statecd, # in the same state
               cell, # in the same grid cell
               grid_x, grid_y, # that have the same coordinates
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

#### 6. Calculate fractional composition per plot ####

# Fractional composition = individual species density / total density
fractional_composition <- stem_density |>
  dplyr::mutate(comp = density / total_density)

# Do all plots add up to 1?
fractional_composition |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = as.factor(plt_cn), 
                                 y = comp, 
                                 fill = as.factor(taxon)),
                    stat = 'identity', 
                    position = 'stack',
                    show.legend = FALSE) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

#### 7. Average plots in each grid cell ####

# Stem density
stem_density_agg <- stem_density |>
  # Group by
  dplyr::group_by(statecd, # state
                  cell, # grid cell
                  grid_x, grid_y, # coordinates
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(density))

# Fractional composition - need to aggregate density first
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

#### 8. Process PalEON output ####

# Load PalEON gridded product
paleon_density <- readr::read_csv('PalEON_FIA_workflow/data/input/species_plot_parameters_v0.1.csv')

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

# Check plots
paleon_stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9) +
  ggplot2::theme_void()

paleon_stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = total_stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9) +
  ggplot2::theme_void()

# Fractional composition at grid level
paleon_fractional_composition_agg <- paleon_stem_density_agg |>
  dplyr::mutate(fcomp = stem_density / total_stem_density)

# Plots
paleon_fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = as.factor(cell), y = fcomp, fill = taxon),
                    stat = 'identity', position = 'stack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

paleon_fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = grid_x, y = grid_y, fill = fcomp)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'D', direction = -1) +
  ggplot2::theme_void()

#### 9. Compare with previous PalEON products ####

# Stem density
both_stem_density_agg <- stem_density_agg |>
  dplyr::full_join(y = paleon_stem_density_agg,
                   by = c('statecd', 'cell',
                          'grid_x', 'grid_y',
                          'taxon')) |>
  dplyr::rename(stem_density = stem_density.x,
                paleon_stem_density = stem_density.y,
                total_stem_density = total_stem_density.x,
                paleon_total_stem_density = total_stem_density.y)

# How close are they?
both_stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stem_density,
                                   y = paleon_stem_density,
                                   color = taxon)) +
  ggplot2::geom_abline() +
  ggplot2::theme_minimal()

both_stem_density_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_stem_density,
                                   y = paleon_total_stem_density)) +
  ggplot2::geom_abline() +
  ggplot2::theme_minimal()

# Fractional composition
both_fractional_composition_agg <- fractional_composition_agg |>
  dplyr::full_join(y = paleon_fractional_composition_agg,
                   by = c('statecd', 'cell',
                          'grid_x', 'grid_y',
                          'taxon')) |>
  dplyr::rename(fcomp = fcomp.x,
                paleon_fcomp = fcomp.y)

both_fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = fcomp,
                                   y = paleon_fcomp,
                                   color = taxon)) +
  ggplot2::geom_abline() +
  ggplot2::theme_minimal()
