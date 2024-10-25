## Formatting and filtering MODIS data

rm(list = ls())

#### Cell 1: h10v04 ####

# Load array
load('data/intermediate/R_format/modis_h10v04.RData')

# Melt to dataframe
modis_df <- reshape2::melt(modis)
# Add column names
colnames(modis_df) <- c('Year', 'Dataset', 'y', 'x', 'value')

# Remove array
rm(modis)

# Pivot wider so each dataset has a column
modis_wide <- tidyr::pivot_wider(modis_df,
                                 names_from = 'Dataset',
                                 values_from = 'value')

# Remove df
rm(modis_df)
gc()

# Filter for:
# 1. LW = 2 (land)
# 2. QC %in% c(0, 9) (land and climate-induced change to forest)
# 3. x & y within extent of study region
modis_filter <- modis_wide |>
  dplyr::filter(LW == 2) |>
  dplyr::filter(QC %in% c(0, 9)) |>
  dplyr::filter(x >= -59000 & x <= 1061000) |>
  dplyr::filter(y >= 70000 & y <= 1478000)

modis_filter <- modis_filter |>
  dplyr::mutate(LC_Type1 = as.character(LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '1', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '2', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '3', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '4', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '5', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '6', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '7', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '8', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '9', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '10', 'prairie', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '11', 'wetland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '12', 'cropland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '13', 'urban', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '14', 'mosaic', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '15', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '16', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '17', 'water', LC_Type1),
                LC_Type2 = as.character(LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '1', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '2', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '3', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '4', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '5', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '6', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '7', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '8', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '9', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '10', 'prairie', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '11', 'wetland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '12', 'cropland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '13', 'urban', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '14', 'mosaic', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '15', 'non-vegetated', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '0', 'water', LC_Type2),
                LC_Type3 = as.character(LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '1', 'prairie', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '2', 'shrubland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '3', 'cropland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '4', 'savanna', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '5', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '6', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '7', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '8', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '9', 'non-vegetated', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '10', 'urban', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '0', 'water', LC_Type3)) |>
  dplyr::select(-LW, -QC)

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana', 'michigan',
                                            'minnesota', 'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

modis_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = LC_Type1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~Year) +
  ggplot2::theme_void()

# Save
modis_h10v04 <- modis_filter
save(modis_h10v04, file = 'data/intermediate/dataframes/modis_h10v04.RData')

#### Cell 2: h10v05 ####

rm(list = ls())

# Load array
load('data/intermediate/R_format/modis_h10v05.RData')

# Melt to dataframe
modis_df <- reshape2::melt(modis)
# Add column names
colnames(modis_df) <- c('Year', 'Dataset', 'y', 'x', 'value')

# Remove array
rm(modis)

# Pivot wider so each dataset has a column
modis_wide <- tidyr::pivot_wider(modis_df,
                                 names_from = 'Dataset',
                                 values_from = 'value')

# Remove df
rm(modis_df)
gc()

# Filter for:
# 1. LW = 2 (land)
# 2. QC %in% c(0, 9) (land and climate-induced change to forest)
# 3. x & y within extent of study region
modis_filter <- modis_wide |>
  dplyr::filter(LW == 2) |>
  dplyr::filter(QC %in% c(0, 9)) |>
  dplyr::filter(x >= -59000 & x <= 1061000) |>
  dplyr::filter(y >= 70000 & y <= 1478000)

modis_filter <- modis_filter |>
  dplyr::mutate(LC_Type1 = as.character(LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '1', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '2', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '3', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '4', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '5', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '6', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '7', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '8', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '9', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '10', 'prairie', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '11', 'wetland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '12', 'cropland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '13', 'urban', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '14', 'mosaic', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '15', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '16', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '17', 'water', LC_Type1),
                LC_Type2 = as.character(LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '1', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '2', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '3', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '4', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '5', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '6', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '7', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '8', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '9', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '10', 'prairie', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '11', 'wetland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '12', 'cropland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '13', 'urban', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '14', 'mosaic', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '15', 'non-vegetated', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '0', 'water', LC_Type2),
                LC_Type3 = as.character(LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '1', 'prairie', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '2', 'shrubland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '3', 'cropland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '4', 'savanna', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '5', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '6', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '7', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '8', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '9', 'non-vegetated', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '10', 'urban', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '0', 'water', LC_Type3)) |>
  dplyr::select(-LW, -QC)

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana', 'michigan',
                                            'minnesota', 'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

modis_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = LC_Type1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~Year) +
  ggplot2::theme_void()

# Save
modis_h10v05 <- modis_filter
save(modis_h10v05, file = 'data/intermediate/dataframes/modis_h10v05.RData')

#### Cell 3: h11v04 ####

rm(list = ls())

# Load array
load('data/intermediate/R_format/modis_h11v04.RData')

# Melt to dataframe
modis_df <- reshape2::melt(modis)
# Add column names
colnames(modis_df) <- c('Year', 'Dataset', 'y', 'x', 'value')

# Remove array
rm(modis)

# Pivot wider so each dataset has a column
modis_wide <- tidyr::pivot_wider(modis_df,
                                 names_from = 'Dataset',
                                 values_from = 'value')

# Remove df
rm(modis_df)
gc()

# Filter for:
# 1. RW = 2 (land)
# 2. QC %in% c(0, 9) (land and climate-induced change to forest)
# 3. x & y within extent of study region
modis_filter <- modis_wide |>
  dplyr::filter(LW == 2) |>
  dplyr::filter(QC %in% c(0, 9)) |>
  dplyr::filter(x >= -59000 & x <= 1061000) |>
  dplyr::filter(y >= 70000 & y <= 1478000)

modis_filter <- modis_filter |>
  dplyr::mutate(LC_Type1 = as.character(LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '1', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '2', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '3', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '4', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '5', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '6', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '7', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '8', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '9', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '10', 'prairie', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '11', 'wetland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '12', 'cropland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '13', 'urban', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '14', 'mosaic', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '15', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '16', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '17', 'water', LC_Type1),
                LC_Type2 = as.character(LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '1', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '2', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '3', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '4', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '5', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '6', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '7', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '8', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '9', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '10', 'prairie', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '11', 'wetland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '12', 'cropland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '13', 'urban', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '14', 'mosaic', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '15', 'non-vegetated', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '0', 'water', LC_Type2),
                LC_Type3 = as.character(LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '1', 'prairie', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '2', 'shrubland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '3', 'cropland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '4', 'savanna', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '5', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '6', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '7', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '8', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '9', 'non-vegetated', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '10', 'urban', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '0', 'water', LC_Type3)) |>
  dplyr::select(-LW, -QC)

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana', 'michigan',
                                            'minnesota', 'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

modis_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = LC_Type1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~Year) +
  ggplot2::theme_void()

modis_h11v04 <- modis_filter
save(modis_h11v04, file = 'data/intermediate/dataframes/modis_h11v04.RData')

#### Cell 4: h11v05 ####

rm(list = ls())

# Load array
load('data/intermediate/R_format/modis_h11v05.RData')

# Melt to dataframe
modis_df <- reshape2::melt(modis)
# Add column names
colnames(modis_df) <- c('Year', 'Dataset', 'y', 'x', 'value')

# Remove array
rm(modis)

# Pivot wider so each dataset has a column
modis_wide <- tidyr::pivot_wider(modis_df,
                                 names_from = 'Dataset',
                                 values_from = 'value')

# Remove df
rm(modis_df)
gc()

# Filter for:
# 1. LW = 2 (land)
# 2. QC %in% c(0, 9) (land and climate-induced change to forest)
# 3. x & y within extent of study region
modis_filter <- modis_wide |>
  dplyr::filter(LW == 2) |>
  dplyr::filter(QC %in% c(0, 9)) |>
  dplyr::filter(x >= -59000 & x <= 1061000) |>
  dplyr::filter(y >= 70000 & y <= 1478000)

modis_filter <- modis_filter |>
  dplyr::mutate(LC_Type1 = as.character(LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '1', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '2', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '3', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '4', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '5', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '6', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '7', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '8', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '9', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '10', 'prairie', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '11', 'wetland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '12', 'cropland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '13', 'urban', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '14', 'mosaic', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '15', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '16', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '17', 'water', LC_Type1),
                LC_Type2 = as.character(LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '1', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '2', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '3', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '4', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '5', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '6', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '7', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '8', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '9', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '10', 'prairie', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '11', 'wetland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '12', 'cropland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '13', 'urban', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '14', 'mosaic', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '15', 'non-vegetated', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '0', 'water', LC_Type2),
                LC_Type3 = as.character(LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '1', 'prairie', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '2', 'shrubland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '3', 'cropland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '4', 'savanna', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '5', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '6', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '7', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '8', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '9', 'non-vegetated', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '10', 'urban', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '0', 'water', LC_Type3)) |>
  dplyr::select(-LW, -QC)

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

modis_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = LC_Type1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~Year) +
  ggplot2::theme_void()

# Save
modis_h11v05 <- modis_filter
save(modis_h11v05, file = 'data/intermediate/dataframes/modis_h11v05.RData')

#### Cell 5: h12v04 ####

rm(list = ls())

# Load array
load('data/intermediate/R_format/modis_h12v04.RData')

# Melt to dataframe
modis_df <- reshape2::melt(modis)
# Add column names
colnames(modis_df) <- c('Year', 'Dataset', 'y', 'x', 'value')

# Remove array
rm(modis)

# Pivot wider so each dataset has a column
modis_wide <- tidyr::pivot_wider(modis_df,
                                 names_from = 'Dataset',
                                 values_from = 'value')

# Remove df
rm(modis_df)
gc()

# Filter for:
# 1. LW = 2 (land)
# 2. QC %in% c(0, 9) (land and climate-induced change to forest)
# 3. x & y within extent of study region
modis_filter <- modis_wide |>
  dplyr::filter(LW == 2) |>
  dplyr::filter(QC %in% c(0, 9)) |>
  dplyr::filter(x >= -59000 & x <= 1061000) |>
  dplyr::filter(y >= 70000 & y <= 1478000)

modis_filter <- modis_filter |>
  dplyr::mutate(LC_Type1 = as.character(LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '1', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '2', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '3', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '4', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '5', 'forest', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '6', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '7', 'shrubland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '8', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '9', 'savanna', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '10', 'prairie', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '11', 'wetland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '12', 'cropland', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '13', 'urban', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '14', 'mosaic', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '15', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '16', 'non-vegetated', LC_Type1),
                LC_Type1 = dplyr::if_else(LC_Type1 == '17', 'water', LC_Type1),
                LC_Type2 = as.character(LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '1', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '2', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '3', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '4', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '5', 'forest', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '6', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '7', 'shrubland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '8', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '9', 'savanna', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '10', 'prairie', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '11', 'wetland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '12', 'cropland', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '13', 'urban', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '14', 'mosaic', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '15', 'non-vegetated', LC_Type2),
                LC_Type2 = dplyr::if_else(LC_Type2 == '0', 'water', LC_Type2),
                LC_Type3 = as.character(LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '1', 'prairie', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '2', 'shrubland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '3', 'cropland', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '4', 'savanna', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '5', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '6', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '7', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '8', 'forest', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '9', 'non-vegetated', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '10', 'urban', LC_Type3),
                LC_Type3 = dplyr::if_else(LC_Type3 == '0', 'water', LC_Type3)) |>
  dplyr::select(-LW, -QC)

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

modis_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = LC_Type1)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~Year) +
  ggplot2::theme_void()

# Save
modis_h12v04 <- modis_filter
save(modis_h12v04, file = 'data/intermediate/dataframes/modis_h12v04.RData')
