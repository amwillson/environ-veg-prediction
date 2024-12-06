#### STEP 2-2

## Processing FIA data to produce gridded estimates of
## total stem density and fractional composition
## Done two ways:
## With only the plots that we know the location of from previous
## PalEON work (so we know which grid cell they fall within)
## And with all plots, estimating the grid cell they fall in 
## Where we don't know the exact coordinates

## 1. Load joined FIA data
## 2. Remove all stems < 20.32 cm DBH (consistent with PLS)
## 3. Remove all plots with any Douglas fir stems (because doug fir can't occur in PLS)
## 4. Connect plots through time
## 5. Assign known plots to grid cells
## 6. Assign known plots from other time periods to grid cells
## 7. Assign grid cells to plots without grid cell ID
## 8. Assign PalEON taxon classifications
## 9. Calculate density per plot per cycle
## 10. Check whether density changes over inventory cycles
## 11. Average over inventory cycles for each plot
## 12. Average plots in each grid cell
## 13. Plot plot-level variables
## 14. Plot grid-level variables
## 15. Save

## Input: data/intermediate/FIA/combined_COND_PLOT_TREE_SPECIES.RData
## Contains all the tree and plot information necessary
## for calculating grid-level total stem density and relative
## abundance

## Input: data/conversions/fia_paleongrid_albers.csv
## Contains the standard PalEON 8 x 8 km grid that I aggregate
## FIA plots to

## Input: data/conversions/FIA_conversion_v03.csv
## Contains the FIA species code-to-PLS taxon conversions
## previously used by the PalEON team

## Output: data/intermediate/FIA/gridded_only_known_plots.RData
## Contains gridded FIA total stem density and relative
## abundance for only the plots that we know which grid cell
## they are in from unswapped/unfuzzed coordinates
## This is NOT currently used because I favored using all the data
## even if their exact locations are more uncertain

## Output: data/intermediate/FIA/gridded_all_plots.RData
## Contains gridded FIA total stem density and relative
## abundance for all plots, with the uncertain plots
## assigned to a grid cell based on swapped & fuzzed
## coordinates
## Used in 2.3.Collate_modern_data.R

rm(list = ls())

#### 1. Load joined FIA data ####

# Load data
load('data/intermediate/FIA/combined_COND_PLOT_TREE_SPECIES.RData')

#### 2. Remove all stems < 20.32 cm DBH ####

TOTAL_FIA <- dplyr::filter(TOTAL_FIA, DIA >= 20.32)

#### 3. Remove all plots with any Douglas fir stems ####

# Douglas fir = 202

# Find plots with SPCD == 202
doug_fir_plots <- unique(TOTAL_FIA$PLT_CN[which(TOTAL_FIA$SPCD == 202)])

# Filter out these plots
TOTAL_FIA <- dplyr::filter(TOTAL_FIA, !(PLT_CN %in% doug_fir_plots))

#### 4. Connect plots through time ####

# Make table of current and previous plot associations
curr_prev_plot <- TOTAL_FIA |>
  dplyr::select(PLT_CN, PREV_PLT_CN) |>
  dplyr::distinct()

curr_prev_plot$ID <- NA

ID <- 1

for(i in 1:nrow(curr_prev_plot)){
  # Is there a previous plot number?
  if(is.na(curr_prev_plot$PREV_PLT_CN[i])){
    # If there isn't, then the current plot is the first one
    # and gets a unique ID
    curr_prev_plot$ID[i] <- ID
    # Increment the ID by one
    ID <- ID + 1
    # If there is a previous plot number, then
  }else{
    # Identify what the previous plot number is
    prev_plt_cn <- curr_prev_plot$PREV_PLT_CN[i]
    # Does this plot appear previously in our database?
    # Sometimes there was a previous plot, but we didn't keep that plot
    if(any(curr_prev_plot$PLT_CN == prev_plt_cn)){
      # Find previous plot ID
      prev_plot_id <- curr_prev_plot[which(curr_prev_plot$PLT_CN == prev_plt_cn),]
      # Add the plot ID to the current row
      # One ID for any instance of the same plot through time
      curr_prev_plot$ID[i] <- prev_plot_id$ID
      # If the plot is not in the database previously despite having a prior record
    }else{
      # It gets a unique ID
      curr_prev_plot$ID[i] <- ID
      # Increment the ID by one
      ID <- ID + 1
    }
  }
  # Progress
  #print(i/nrow(curr_prev_plot))
}

# Join plot ID to the FIA dataset
TOTAL_FIA_plot <- TOTAL_FIA |>
  dplyr::left_join(y = curr_prev_plot,
                   by = c('PLT_CN', 'PREV_PLT_CN')) |>
  dplyr::rename(PLOT_ID = ID)

#### 5. Assign known plots grid cells ####

# Load PalEON coordinates & grid mapping
paleon_coords <- readr::read_csv('data/conversions/fia_paleongrid_albers.csv')

# Match FIA plots to PalEON grid cells
matched_plots <- TOTAL_FIA_plot |>
  dplyr::select(PLT_CN, LAT, LON, PLOT_ID, STATECD) |>
  dplyr::rename(CN = PLT_CN) |>
  dplyr::left_join(y = paleon_coords,
                   by = c('STATECD', 'CN')) |>
  tidyr::drop_na() |>
  dplyr::select(-CN, -LAT, -LON, -STATECD) |>
  dplyr::distinct()

#### 6. Assign known plots from other time periods grid cells ####

# Use FIA coordinates to match any plots with the same coordinates to the grid cell
TOTAL_FIA_matched <- TOTAL_FIA_plot |>
  dplyr::left_join(y = matched_plots,
                   by = 'PLOT_ID')

## Which plots have and haven't  been assigned a grid cell?
## These are grid cells that don't have unswapped/unfuzzed coordinates from
## the PalEON data product

# Take every plot that has been matched
yes_matched <- TOTAL_FIA_matched |>
  dplyr::filter(!is.na(x))

# Coordinates for plots that have been matched
yes_matched_coords <- yes_matched |>
  dplyr::select(LAT, LON, PLOT_ID) |>
  dplyr::distinct()

# Take every plot that has NOT been matched
no_matched <- TOTAL_FIA_matched |>
  dplyr::filter(is.na(x))

# Coordinates for plots that have not been matched
no_matched_coords <- no_matched |>
  dplyr::select(LAT, LON, PLOT_ID) |>
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

#### 7. Assign grid cells to plots without them ####

## Note that this is assuming the swapped/fuzzed coordinates are correct
## This is NOT an appropriate assumption, which is why we calculate grid cell-level
## metrics in two ways below

# Assign grid cells according to closest plot with grid cell assignment
dists <- fields::rdist(dplyr::select(no_matched_coords, LAT, LON), 
                       dplyr::select(yes_matched_coords, LAT, LON))
# Find closest plot with grid cell coordinates to each plot without grid cell coordinates
closest_plot <- apply(dists, 1, which.min)
# Remove dists
rm(dists)

# Coordinates that we will assign to the un-matched plots
select_yes_matched_coords <- dplyr::slice(yes_matched_coords, closest_plot)

# Add coordinates to the unmatched plots coordinates
new_matched_coords <- cbind(no_matched_coords, select_yes_matched_coords)
colnames(new_matched_coords) <- c('old_LAT', 'old_LON', 'old_PLOT_ID', 'new_LAT', 'new_LON', 'new_PLOT_ID')

# See how close coordinates are to each other
new_matched_coords |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = old_LAT, y = new_LAT)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = old_LAT, y = new_LAT),
                       method = 'lm', se = FALSE) +
  ggplot2::theme_minimal()
new_matched_coords |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = old_LON, y = new_LON)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = old_LON, y = new_LON),
                       method = 'lm', se = FALSE) +
  ggplot2::theme_minimal()

# Remove columns we don't need
new_matched_coords <- dplyr::select(new_matched_coords, old_PLOT_ID, new_PLOT_ID)

# Assigning coordinates to full dataframe with each individual tree
## This isn't matching anything?
no_matched2 <- no_matched |>
  # Rename to correspond with new_matched_coords
  dplyr::rename(old_PLOT_ID = PLOT_ID) |>
  # Add new PLOT_ID assignment
  dplyr::left_join(y = new_matched_coords,
                   by = 'old_PLOT_ID') |>
  # Rename PLOT_ID to match with matched_plots
  dplyr::rename(PLOT_ID = new_PLOT_ID) |>
  # Remove columns we are replacing
  dplyr::select(-x, -y, -cell) |>
  # Add grid-level variables
  dplyr::left_join(y = matched_plots,
                   by = 'PLOT_ID') |>
  # Remove PLOT_ID because this is actually the WRONG
  # PLOT_ID. This is the PLOT_ID for the nearest plot
  # with known coords. We want to keep the old PLOT_ID
  dplyr::select(-PLOT_ID) |>
  dplyr::rename(PLOT_ID = old_PLOT_ID) |>
  # Make columns in the same order as the other subset
  dplyr::select(colnames(yes_matched))

# Combine matched and (previously) unmatched plots into one dataframe
TOTAL_FIA_matched2 <- rbind(yes_matched, no_matched2)

# Now, there should be NAs in the cell field
# in TOTAL_FIA_matched but not in TOTAL_FIA_matched2
any(is.na(TOTAL_FIA_matched$cell)) # should be TRUE
any(is.na(TOTAL_FIA_matched2$cell)) # should be FALSE

# Filter out all the rows with grid = NA in the first dataframe
TOTAL_FIA_matched <- dplyr::filter(TOTAL_FIA_matched, !is.na(cell))

#### 8. Assign PalEON taxon classifications ####

# Read in PalEON taxon assignments
paleon_taxa <- readr::read_csv('data/conversions/FIA_conversion_v03.csv')

# For some reason there are two elm rows. Remove one
paleon_taxa <- paleon_taxa[-243,]

# Add PalEON taxa to first FIA database version
TOTAL_FIA_matched_taxon <- paleon_taxa |>
  dplyr::select(spcd, PalEON) |>
  dplyr::rename(SPCD = spcd) |>
  dplyr::right_join(y = TOTAL_FIA_matched, by = 'SPCD') |>
  dplyr::rename(taxon = PalEON) |>
  # Manually assign other hardwood to SPCDs not included in PalEON translation table
  dplyr::mutate(taxon = dplyr::if_else(SPCD %in% c(6918, 6955), 'Other hardwood', taxon)) |>
  # Manually group taxa that are grouped in PLS era
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'Alder', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Black gum', 'Black gum/sweet gum', taxon),
                taxon = dplyr::if_else(taxon == 'Buckeye', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Hackberry', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Poplar', 'Poplar/tulip poplar', taxon),
                taxon = dplyr::if_else(taxon == 'Sweet gum', 'Black gum/sweet gum', taxon),
                taxon = dplyr::if_else(taxon == 'Sycamore', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Tulip poplar', 'Poplar/tulip poplar', taxon),
                taxon = dplyr::if_else(taxon == 'Willow', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Unknown tree', 'Other hardwood', taxon))

# Add PalEON taxa to second FIA database version
TOTAL_FIA_matched_taxon2 <- paleon_taxa |>
  dplyr::select(spcd, PalEON) |>
  dplyr::rename(SPCD = spcd) |>
  dplyr::right_join(y = TOTAL_FIA_matched2, by = 'SPCD') |>
  dplyr::rename(taxon = PalEON) |>
  dplyr::mutate(taxon = dplyr::if_else(SPCD %in% c(6918, 6955), 'Other hardwood', taxon)) |>
  # Manually group taxa that are grouped in PLS era
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'Alder', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Black gum', 'Black gum/sweet gum', taxon),
                taxon = dplyr::if_else(taxon == 'Buckeye', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Hackberry', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Poplar', 'Poplar/tulip poplar', taxon),
                taxon = dplyr::if_else(taxon == 'Sweet gum', 'Black gum/sweet gum', taxon),
                taxon = dplyr::if_else(taxon == 'Sycamore', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Tulip poplar', 'Poplar/tulip poplar', taxon),
                taxon = dplyr::if_else(taxon == 'Willow', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'Unknown tree', 'Other hardwood', taxon))

#### 9. Calculate density per plot per cycle ####

stem_density <- TOTAL_FIA_matched_taxon |>
  # Combine state and inventory cycle because cycle IDs are only applicable at state level
  #dplyr::mutate(CYCLE = paste0(STATECD, CYCLE)) |>
  # Count number of trees
  dplyr::count(STATECD, # in the same state
               CYCLE, # in the same inventory cycle
               cell, # in the same grid cell
               x, y, # that have the same coordinates
               PLOT_ID, # in the same location
               PLT_CN, # in the same plot
               taxon, # that are the same PalEON taxon
               SPCD, # that are the same species
               TPA_UNADJ) |> # with the same tree per acre conversion
  # Calculate density = number of trees * tree per acre conversion * acre to hectare conversion
  dplyr::mutate(ind_density = n * TPA_UNADJ * (1/0.404686)) |>
  # Group by same variables to retain them
  # (except tpa and ind. species because we need to sum over)
  dplyr::group_by(STATECD, CYCLE, cell, x, y, PLOT_ID, PLT_CN, taxon) |>
  # Sum density of trees of the same taxon and plot
  # across conversion factors
  dplyr::summarize(density = sum(ind_density))

stem_density2 <- TOTAL_FIA_matched_taxon2 |>
  # Count number of trees
  dplyr::count(STATECD, # in the same state
               CYCLE, # in the same inventory cycle
               cell, # in the same grid cell
               x, y, # that have the same coordinates
               PLOT_ID, # in the same location
               PLT_CN, # in the same plot
               taxon, # that are the same PalEON taxon
               SPCD, # that are the same species
               TPA_UNADJ) |> # with the same tree per acre conversion
  # Calculate density = number of trees * tree per acre conversion * acre to hectare conversion
  dplyr::mutate(ind_density = n * TPA_UNADJ * (1/0.404686)) |>
  # Group by same variables to retain them
  # (except tpa and ind. species because we need to sum over)
  dplyr::group_by(STATECD, CYCLE, cell, x, y, PLOT_ID, PLT_CN, taxon) |>
  # Sum density of trees of the same taxon and plot
  # across conversion factors
  dplyr::summarize(density = sum(ind_density))

# Total stem density
total_stem_density <- stem_density |>
  dplyr::group_by(STATECD, CYCLE, cell, x, y, PLOT_ID, PLT_CN) |>
  dplyr::summarize(total_density = sum(density))
total_stem_density2 <- stem_density2 |>
  dplyr::group_by(STATECD, CYCLE, cell, x, y, PLOT_ID, PLT_CN) |>
  dplyr::summarize(total_density = sum(density))

# Add to taxon-level dataframes
stem_density <- stem_density |>
  dplyr::left_join(y = total_stem_density,
                   by = c('STATECD', 'CYCLE', 'cell', 'x', 'y', 'PLOT_ID', 'PLT_CN'))
stem_density2 <- stem_density2 |>
  dplyr::left_join(y = total_stem_density2,
                   by = c('STATECD', 'CYCLE', 'cell', 'x', 'y', 'PLOT_ID', 'PLT_CN'))

#### 10. Check whether stem density  changes over inventory cycles ####

# Unique inventory years and plot IDs
year_id <- TOTAL_FIA |>
  dplyr::select(PLT_CN, MEASYEAR) |>
  dplyr::distinct()

# Function to get point density
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

# Add unique inventory years and plot IDs to stem density dataframe
# Need this in order to look at changes over time
stem_density_year <- stem_density |>
  dplyr::left_join(y = year_id,
                   by = 'PLT_CN')

# Estimate point density for color on figure
stem_density_year$point_density <- get_density(x = stem_density_year$MEASYEAR,
                                               y = stem_density_year$density,
                                               n = 100)

# Plot stem density over time to make sure there
# are no meaningful changes over time
# Flat smooths (blue) indicate no change over time
stem_density_year |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = MEASYEAR, y = density,
                                   color = point_density),
                      alpha = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(x = MEASYEAR, y = density),
                       se = FALSE, method = 'lm') +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('Time') + ggplot2::ylab('Stem density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

# Add time and plot id to the second stem density dataframe
stem_density_year2 <- stem_density2 |>
  dplyr::left_join(y = year_id,
                   by = 'PLT_CN')

# Get point density for plotting
stem_density_year2$point_density <- get_density(x = stem_density_year2$MEASYEAR,
                                                y = stem_density_year2$density,
                                                n = 100)

# Check whether stem density changes over time in second dataframe
stem_density_year2 |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = MEASYEAR, y = density,
                                   color = point_density),
                      alpha = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(x = MEASYEAR, y = density),
                       se = FALSE, method = 'lm') +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('Time') + ggplot2::ylab('Stem density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

# Remove taxon-specific columns
# Since the taxon columns are in long format, this allows us to have
# one row per plot for looking at total density
stem_density_year <- stem_density_year |>
  dplyr::select(STATECD, CYCLE, cell, x, y, PLOT_ID, PLT_CN, total_density, MEASYEAR) |>
  dplyr::distinct()

# Point density again
stem_density_year$point_density <- get_density(x = stem_density_year$MEASYEAR,
                                               y = stem_density_year$total_density,
                                               n = 100)

# Plot total density over time to make sure there are no patterns
stem_density_year |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = MEASYEAR, y = total_density,
                                   color = point_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = MEASYEAR, y = total_density),
                       se = FALSE, method = 'lm') +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9) +
  ggplot2::xlab('Time') + ggplot2::ylab('Total stem density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

# Repeat getting total stem density from the second dataframe
stem_density_year2 <- stem_density_year2 |>
  dplyr::select(STATECD, CYCLE, cell, x, y, PLOT_ID, PLT_CN, total_density, MEASYEAR) |>
  dplyr::distinct()

# Get point density for plotting
stem_density_year2$point_density <- get_density(x = stem_density_year2$MEASYEAR,
                                                y = stem_density_year2$total_density,
                                                n = 100)

# Plot total stem density over time to make sure there are no pattern
# over time again with dataframe 2
stem_density_year2 |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = MEASYEAR, y = total_density,
                                   color = point_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = MEASYEAR, y = total_density),
                       se = FALSE, method = 'lm') +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9) +
  ggplot2::xlab('Time') + ggplot2::ylab('Total stem density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

#### 11. Average over inventory cycles for each plot ####

## Stem density

stem_density_time_av <- stem_density |>
  # Group by
  dplyr::group_by(STATECD, # state
                  cell, # grid cell
                  x, y, # coordinates
                  PLOT_ID, # location (identifies unique plots across inventories)
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(density))

stem_density_time_av2 <- stem_density2 |>
  # Group by
  dplyr::group_by(STATECD, # state
                  cell, # grid cell
                  x, y, # coordinates
                  PLOT_ID, # location (identifies unique plots across inventories)
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(density))

# Total stem density
total_stem_density_time_av <- stem_density_time_av |>
  dplyr::group_by(STATECD, cell, x, y, PLOT_ID) |>
  dplyr::summarize(total_stem_density = sum(stem_density))
total_stem_density_time_av2 <- stem_density_time_av2 |>
  dplyr::group_by(STATECD, cell, x, y, PLOT_ID) |>
  dplyr::summarize(total_stem_density = sum(stem_density))

# Add total stem density to dataframes
stem_density_time_av <- stem_density_time_av |>
  dplyr::left_join(y = total_stem_density_time_av,
                   by = c('STATECD', 'cell', 'x', 'y', 'PLOT_ID'))
stem_density_time_av2 <- stem_density_time_av2 |>
  dplyr::left_join(y = total_stem_density_time_av2,
                   by = c('STATECD', 'cell', 'x', 'y', 'PLOT_ID'))

#### 12. Average plots in each grid cell ####

## Now that we have the average stem density of each taxon in each
## plot over time, we average each plot in the grid cell
## This order is important: it ensures equal weight is given to each
## plot location, regardless of how many re-sample inventories it has

# Stem density
stem_density_agg <- stem_density_time_av |>
  # Remove state so that grid cells can cross state lines
  dplyr::select(-STATECD) |>
  # Group by
  dplyr::group_by(cell, # grid cell
                  x, y, # coordinates
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(stem_density))
stem_density_agg2 <- stem_density_time_av2 |>
  # Remove state so that grid cells can cross state lines
  dplyr::select(-STATECD) |>
  # Group by
  dplyr::group_by(cell, # grid cell
                  x, y, # coordinates
                  taxon) |> # taxon
  dplyr::summarize(stem_density = mean(stem_density))

# Total stem density
total_stem_density_agg <- stem_density_agg |>
  dplyr::group_by(cell, x, y) |>
  dplyr::summarize(total_stem_density = sum(stem_density))
total_stem_density_agg2 <- stem_density_agg2 |>
  dplyr::group_by(cell, x, y) |>
  dplyr::summarize(total_stem_density = sum(stem_density))

# Add total stem density to stem density dataframes
stem_density_agg <- stem_density_agg |>
  dplyr::left_join(y = total_stem_density_agg,
                   by = c('cell', 'x', 'y'))
stem_density_agg2 <- stem_density_agg2 |>
  dplyr::left_join(y = total_stem_density_agg2,
                   by = c('cell', 'x', 'y'))

# Fractional composition at grid level
fractional_composition_agg <- stem_density_agg |>
  dplyr::mutate(fcomp = stem_density / total_stem_density)
fractional_composition_agg2 <- stem_density_agg2 |>
  dplyr::mutate(fcomp = stem_density / total_stem_density)

#### 13. Plot plot-level variables ####

# Convert map CRS
states <- sf::st_transform(states, crs = 'EPSG:3175')

## With only definitely matched plots
p1 <- stem_density_time_av |>
  dplyr::mutate(stem_density = dplyr::if_else(stem_density > 750, 750, stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = stem_density), 
                      shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_color_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Stem density\n(stems/ha)') +
  ggplot2::ggtitle('Only plots with unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p1

## With all plots
p2 <- stem_density_time_av2 |>
  dplyr::mutate(stem_density = dplyr::if_else(stem_density > 750, 750, stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = stem_density),
                      shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_color_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Stem density\n(stems/ha)') +
  ggplot2::ggtitle('All plots') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p2

# Plots that DON'T have unswapped/unfuzzed coordinates
locs_sf <- unique(stem_density_time_av2$PLOT_ID)[which(!(unique(stem_density_time_av2$PLOT_ID) %in% unique(stem_density_time_av$PLOT_ID)))]
p3 <- stem_density_time_av2 |>
  dplyr::filter(PLOT_ID %in% locs_sf) |>
  dplyr::mutate(stem_density = dplyr::if_else(stem_density > 750, 750, stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = stem_density),
                      shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_color_viridis_c(option = 'G', direction = -1, end = 0.9,
                                 name = 'Stem density\n(stems/ha)') +
  ggplot2::ggtitle('Only plots WITHOUT unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p3

# Plot together
pp1 <- cowplot::plot_grid(p1, p3, nrow = 1)
pp1

## Total density per plot as above
p4 <- stem_density_time_av |>
  dplyr::select(-stem_density, -taxon) |>
  dplyr::distinct() |>
  dplyr::mutate(total_stem_density = dplyr::if_else(total_stem_density > 750, 750, total_stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = total_stem_density),
                      size = 1) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Total stem density\n(stems/ha)') +
  ggplot2::ggtitle('Only plots with unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p4

p5 <- stem_density_time_av2 |>
  dplyr::select(-stem_density, -taxon) |>
  dplyr::distinct() |>
  dplyr::mutate(total_stem_density = dplyr::if_else(total_stem_density > 750, 750, total_stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = total_stem_density),
                      size = 1) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Total stem density\n(stems/ha)') +
  ggplot2::ggtitle('All plots') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p5

p6 <- stem_density_time_av2 |>
  dplyr::filter(PLOT_ID %in% locs_sf) |>
  dplyr::select(-stem_density, -taxon) |>
  dplyr::distinct() |>
  dplyr::mutate(total_stem_density = dplyr::if_else(total_stem_density > 750, 750, total_stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = total_stem_density),
                      size = 1) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_viridis_c(option = 'G', direction = -1, end = 0.9,
                                 name = 'Total stem density\n(stems/ha)') +
  ggplot2::ggtitle('Only plots WITHOUT unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p6

pp2 <- cowplot::plot_grid(p4, p6, nrow = 1)
pp2

#### 14. Plot grid-level variables ####

## Taxon level stem density
p7 <- stem_density_agg |>
  dplyr::mutate(stem_density = dplyr::if_else(stem_density > 750, 750, stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Stem density\n(stems/ha)') +
  ggplot2::ggtitle('Gridded using only plots with unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p7

p8 <- stem_density_agg2 |>
  dplyr::mutate(stem_density = dplyr::if_else(stem_density > 750, 750, stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Stem density\n(stems/ha)') +
  ggplot2::ggtitle('Gridded using all plots') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p8

# Plot together
pp3 <- cowplot::plot_grid(p7, p8, nrow = 1)
pp3

# How close are the grid-level estimates using different methods?
stem_density_agg_comp <- stem_density_agg |>
  dplyr::rename(stem_density1 = stem_density) |>
  dplyr::full_join(y = stem_density_agg2,
                   by = c('cell', 'x', 'y', 'taxon')) |>
  dplyr::rename(stem_density2 = stem_density) |>
  dplyr::mutate(stem_density1 = dplyr::if_else(stem_density1 > 750, 750, stem_density1),
                stem_density2 = dplyr::if_else(stem_density2 > 750, 750, stem_density2)) |>
  dplyr::filter(!is.na(stem_density1) & !is.na(stem_density2))

stem_density_agg_comp$point_density <- get_density(x = stem_density_agg_comp$stem_density1,
                                                   y = stem_density_agg_comp$stem_density2,
                                                   n = 100)  

stem_density_agg_comp |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stem_density1, y = stem_density2,
                                   color = point_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = stem_density1, y = stem_density2),
                       se = FALSE, color = 'blue', method = 'lm') +
  ggplot2::geom_abline() +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9,
                                 transform = 'log10'
                                 ) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('Grid cells using only plots with unswapped/unfuzzed coordinates') +
  ggplot2::ylab('Grid cells using all plots') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

# How close are they in space?
stem_density_agg_comp |>
  dplyr::mutate(diff = stem_density1 - stem_density2) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(low = 'darkred',
                                high = 'dodgerblue3',
                                name = 'Stems/ha') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::ggtitle('Difference in stem density between methods') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## Total stem density
p9 <- stem_density_agg |>
  dplyr::select(-taxon, -stem_density) |>
  dplyr::distinct() |>
  dplyr::mutate(total_stem_density = dplyr::if_else(total_stem_density > 750, 750, total_stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Total stem density\n(stems/ha)') +
  ggplot2::ggtitle('Gridded using only plots with unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p9

p10 <- stem_density_agg2 |>
  dplyr::select(-taxon, -stem_density) |>
  dplyr::distinct() |>
  dplyr::mutate(total_stem_density = dplyr::if_else(total_stem_density > 750, 750, total_stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G', direction = -1, end = 0.9,
                                name = 'Total stem density\n(stems/ha)') +
  ggplot2::ggtitle('Gridded using all plots') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p10

pp4 <- cowplot::plot_grid(p9, p10, nrow = 1)
pp4

# Figure for paper of total stem density
stem_density_agg2 |>
  dplyr::select(-taxon, -stem_density) |>
  dplyr::distinct() |>
  dplyr::mutate(total_stem_density = dplyr::if_else(total_stem_density > 750, 750, total_stem_density)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_stem_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Greens',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::ggtitle('Modern total stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/modern_total_stem_density.png',
                height = 10, width = 10, units = 'cm')  

# How close are they in total stem density?
stem_density_agg_comp <- stem_density_agg |>
  dplyr::select(-taxon, -stem_density) |>
  dplyr::distinct() |>
  dplyr::rename(total_stem_density1 = total_stem_density) |>
  dplyr::full_join(y = stem_density_agg2 |> dplyr::select(-taxon, -stem_density) |> dplyr::distinct(),
                   by = c('cell', 'x', 'y')) |>
  dplyr::rename(total_stem_density2 = total_stem_density) |>
  dplyr::mutate(total_stem_density1 = dplyr::if_else(total_stem_density1 > 750, 750, total_stem_density1),
                total_stem_density2 = dplyr::if_else(total_stem_density2 > 750, 750, total_stem_density2)) |>
  dplyr::filter(!is.na(total_stem_density1) & !is.na(total_stem_density2))

stem_density_agg_comp$point_density <- get_density(x = stem_density_agg_comp$total_stem_density1,
                                                   y = stem_density_agg_comp$total_stem_density2,
                                                   n = 100)  

stem_density_agg_comp |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = total_stem_density1, y = total_stem_density2,
                                   color = point_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = total_stem_density1, y = total_stem_density2),
                       se = FALSE, color = 'blue', method = 'lm') +
  ggplot2::geom_abline() +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9,
                                 ) +
  ggplot2::xlab('Grid cells using only plots with unswapped/unfuzzed coordinates') +
  ggplot2::ylab('Grid cells using all plots') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

# Magnitude of difference between methods in space
stem_density_agg_comp |>
  dplyr::mutate(diff = total_stem_density1 - total_stem_density2) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(low = 'darkred',
                                high = 'dodgerblue3',
                                name = 'Stems/ha') +
  ggplot2::ggtitle('Difference in stem density between methods') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## Fractional composition
p11 <- fractional_composition_agg |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fcomp)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'D', direction = -1,
                                limits = c(0, 1),
                                name = 'Fraction total\nstems') +
  ggplot2::ggtitle('Gridded using only plots with unswapped/unfuzzed coordinates') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p11

p12 <- fractional_composition_agg2 |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fcomp)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_viridis_c(option = 'D', direction = -1,
                                limits = c(0, 1),
                                name = 'Fraction total\nstems') +
  ggplot2::ggtitle('Gridded using all plots') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
p12

pp5 <- cowplot::plot_grid(p11, p12, nrow = 1)
pp5

# How close are fractional composition estimates to each other?
fractional_composition_agg_comp <- fractional_composition_agg |>
  dplyr::rename(fcomp1 = fcomp) |>
  dplyr::full_join(y = fractional_composition_agg2,
                   by = c('cell', 'x', 'y', 'taxon')) |>
  dplyr::rename(fcomp2 = fcomp) |>
  dplyr::filter(!is.na(fcomp1) & !is.na(fcomp2))
fractional_composition_agg_comp$point_density <- get_density(x = fractional_composition_agg_comp$fcomp1,
                                                             y = fractional_composition_agg_comp$fcomp2,
                                                             n = 100)  

fractional_composition_agg_comp |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = fcomp1, y = fcomp2,
                                   color = point_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = fcomp1, y = fcomp2),
                       se = FALSE, color = 'blue', method = 'lm') +
  ggplot2::geom_abline() +
  ggplot2::scale_color_viridis_c(option = 'C',
                                 begin = 0.1, end = 0.9,
                                 transform = 'log1p'
                                 ) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('Grid cells using only plots with unswapped/unfuzzed coordinates') +
  ggplot2::ylab('Grid cells using all plots') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = 'none')

# Difference between fractional composition between methods in space
fractional_composition_agg_comp |>
  dplyr::mutate(diff = fcomp1 - fcomp2) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'darkgrey') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(low = 'darkred',
                                high = 'dodgerblue3',
                                name = 'Fraction total\nstems') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::ggtitle('Difference in fractional composition between methods') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### 15. Save ####

# Make cutoff at 750 stems/ha for each dataset
stem_density_agg <- dplyr::mutate(stem_density_agg,
                                  total_stem_density = 
                                    dplyr::if_else(total_stem_density > 750,
                                                   750,
                                                   total_stem_density))

stem_density_agg2 <- dplyr::mutate(stem_density_agg2,
                                   total_stem_density = 
                                     dplyr::if_else(total_stem_density > 750,
                                                    750,
                                                    total_stem_density))

# Save dataframes
save(stem_density_agg, fractional_composition_agg,
     file = 'data/intermediate/FIA/gridded_only_known_plots.RData')

save(stem_density_agg2, fractional_composition_agg2,
     file = 'data/intermediate/FIA/gridded_all_plots.RData')
