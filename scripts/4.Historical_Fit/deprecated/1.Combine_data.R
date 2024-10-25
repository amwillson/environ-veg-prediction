## Combine all data sources
## All states, all variables

rm(list = ls())

#### Point level ####

# Load PLS point level data that has been matched to the grid level
# (this will make it easier to split into training and testing sets)
load('data/processed/PLS/illinois_matched.RData')
load('data/processed/PLS/indiana_matched.RData')
load('data/processed/PLS/lowmichigan_matched.RData')
load('data/processed/PLS/upmichigan_matched.RData')
load('data/processed/PLS/minnesota_matched.RData')
load('data/processed/PLS/wisconsin_matched.RData')

# Check that the columns are the same between dataframes
all(colnames(illinois_matched) == colnames(indiana_matched))
all(colnames(indiana_matched) == colnames(lowmichigan_matched))
all(colnames(lowmichigan_matched) == colnames(upmichigan_matched))
all(colnames(upmichigan_matched) == colnames(minnesota_matched))
all(colnames(minnesota_matched) == colnames(wisconsin_matched))

# Combine if all column names are the same
PLS_point <- rbind(illinois_matched, indiana_matched,
                   lowmichigan_matched, upmichigan_matched,
                   minnesota_matched, wisconsin_matched)

# Order that columns should be in for the ecosystem (wide) dataframes
col_order <- colnames(illinois_ecosystem_matched)

# Add columns for third and fourth trees where applicable and
# shuffle columns
indiana_ecosystem_matched <- indiana_ecosystem_matched |>
  dplyr::mutate(three = NA,
                four = NA) |>
  dplyr::select(tidyselect::all_of(col_order))
lowmichigan_ecosystem_matched <- lowmichigan_ecosystem_matched |>
  dplyr::mutate(three = NA,
                four = NA) |>
  dplyr::select(tidyselect::all_of(col_order))
upmichigan_ecosystem_matched <- upmichigan_ecosystem_matched |>
  dplyr::mutate(three = NA,
                four = NA) |>
  dplyr::select(tidyselect::all_of(col_order))

# Check that all column names are the same between dataframes
all(colnames(illinois_ecosystem_matched) == colnames(indiana_ecosystem_matched))
all(colnames(indiana_ecosystem_matched) == colnames(lowmichigan_ecosystem_matched))
all(colnames(lowmichigan_ecosystem_matched) == colnames(upmichigan_ecosystem_matched))
all(colnames(upmichigan_ecosystem_matched) == colnames(minnesota_ecosystem_matched))
all(colnames(minnesota_ecosystem_matched) == colnames(wisconsin_ecosystem_matched))

# If the columns are the same, combine
PLS_ecosystem_point <- rbind(illinois_ecosystem_matched, indiana_ecosystem_matched,
                             lowmichigan_ecosystem_matched, upmichigan_ecosystem_matched,
                             minnesota_ecosystem_matched, wisconsin_ecosystem_matched)

# Remove state-level dataframes
rm(illinois_ecosystem_matched, illinois_matched, indiana_ecosystem_matched,
   indiana_matched, lowmichigan_ecosystem_matched, lowmichigan_matched,
   upmichigan_ecosystem_matched, upmichigan_matched, minnesota_ecosystem_matched,
   minnesota_matched, wisconsin_ecosystem_matched, wisconsin_matched, col_order)

# Load soil data
load('data/processed/soils/processed_soil_il.RData')
load('data/processed/soils/processed_soil_in.RData')
load('data/processed/soils/processed_soil_mi.RData')
load('data/processed/soils/processed_soil_mn.RData')
load('data/processed/soils/processed_soil_wi.RData')

# Check column names are the same
all(colnames(IL_soil) == colnames(IN_soil))
all(colnames(IN_soil) == colnames(MI_soil))
all(colnames(MI_soil) == colnames(MN_soil))
all(colnames(MN_soil) == colnames(WI_soil))

# Combine
soil <- rbind(IL_soil, IN_soil, MI_soil, MN_soil, WI_soil)

# Reformat
soil <- soil |>
  dplyr::select(-soil_x, -soil_y) |>
  dplyr::rename(x = pls_x,
                y = pls_y)

# Join
ecosystem_point <- PLS_ecosystem_point |>
  dplyr::left_join(y = soil, by = c('x', 'y'))
point <- PLS_point |>
  dplyr::left_join(y = soil, by = c('x', 'y'))

# Load flood data
load('data/processed/soils/processed_flood_il.RData')
load('data/processed/soils/processed_flood_in.RData')
load('data/processed/soils/processed_flood_mi.RData')
load('data/processed/soils/processed_flood_mn.RData')
load('data/processed/soils/processed_flood_wi.RData')

# Check column names are the same
all(colnames(IL_flood) == colnames(IN_flood))
all(colnames(IN_flood) == colnames(MI_flood))
all(colnames(MI_flood) == colnames(MN_flood))
all(colnames(MN_flood) == colnames(WI_flood))

# Combine
flood <- rbind(IL_flood, IN_flood, MI_flood, MN_flood, WI_flood)

# Reformat
flood <- flood |>
  dplyr::select(-soil_x, -soil_y) |>
  dplyr::rename(x = pls_x,
                y = pls_y)

# Join
ecosystem_point <- ecosystem_point |>
  dplyr::left_join(y = flood, by = c('x', 'y'))
point <- point |>
  dplyr::left_join(y = flood, by = c('x', 'y'))

# Load climate data
load('data/processed/climate/processed_climate_il.RData')
load('data/processed/climate/processed_climate_in.RData')
load('data/processed/climate/processed_climate_mi.RData')
load('data/processed/climate/processed_climate_mn.RData')
load('data/processed/climate/processed_climate_wi.RData')

# Check column names are the same
all(colnames(IL_clim) == colnames(IN_clim))
all(colnames(IN_clim) == colnames(MI_clim))
all(colnames(MI_clim) == colnames(MN_clim))
all(colnames(MN_clim) == colnames(WI_clim))

# Combine
clim <- rbind(IL_clim, IN_clim, MI_clim, MN_clim, WI_clim)

# Reformat
clim <- clim |>
  dplyr::select(-prism_x, -prism_y) |>
  dplyr::rename(x = pls_x,
                y = pls_y)

# Join
ecosystem_point <- ecosystem_point |>
  dplyr::left_join(y = clim, by = c('x', 'y'))
point <- point |>
  dplyr::left_join(y = clim, by = c('x', 'y'))

# Load topographic data
load('data/processed/topography/processed_topo_il.RData')
load('data/processed/topography/processed_topo_in.RData')
load('data/processed/topography/processed_topo_mi.RData')
load('data/processed/topography/processed_topo_mn.RData')
load('data/processed/topography/processed_topo_wi.RData')

# Check column names are the same
all(colnames(IL_topo) == colnames(IN_topo))
all(colnames(IN_topo) == colnames(MI_topo))
all(colnames(MI_topo) == colnames(MN_topo))
all(colnames(MN_topo) == colnames(WI_topo))

# Combine
topo <- rbind(IL_topo, IN_topo, MI_topo, MN_topo, WI_topo)

# Reformat
topo <- topo |>
  dplyr::select(-topo_x, -topo_y) |>
  dplyr::rename(x = pls_x,
                y = pls_y)

# Join
ecosystem_point <- ecosystem_point |>
  dplyr::left_join(y = topo, by = c('x', 'y'))
point <- point |>
  dplyr::left_join(y = topo, by = c('x', 'y'))

# Save
save(ecosystem_point, point,
     file = 'data/processed/point_joined.RData')

#### Grid level ####
rm(list = ls())

# Load PLS gridded data
load('data/processed/PLS/8km.RData')

# Reproject
comp_dens <- sf::st_as_sf(comp_dens, coords = c('x', 'y'),
                          crs = 'EPSG:3175')
comp_dens <- sf::st_transform(comp_dens, crs = 'EPSG:4326')
comp_dens <- sfheaders::sf_to_df(comp_dens, fill = TRUE)
comp_dens <- dplyr::select(comp_dens, -sfg_id, -point_id)

# Load soil data
load('data/processed/soils/gridded_soil.RData')

# Join
grid <- comp_dens |>
  dplyr::rename(grid_id = id) |>
  dplyr::left_join(y = soil_grid, by = c('grid_id', 'x', 'y'))

# Load floodplain data
load('data/processed/soils/gridded_flood.RData')

# Join
grid <- grid |>
  dplyr::left_join(y = flood_grid, by = c('grid_id', 'x', 'y'))

# Load climate data
load('data/processed/climate/gridded_climate.RData')

# Join
grid <- grid |>
  dplyr::left_join(y = clim_grid, by = c('grid_id', 'x', 'y'))

# Load topography data
load('data/processed/topography/gridded_topography.RData')

# Join
grid <- grid |>
  dplyr::left_join(y = topo_grid, by = c('grid_id', 'x', 'y'))

# Save
save(grid, file = 'data/processed/grid_joined.RData')
