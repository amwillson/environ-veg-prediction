## Collating and splitting data for random forest

rm(list = ls())

# Climate data
load('data/processed/climate/processed_climate_il.RData')
load('data/processed/climate/processed_climate_in.RData')
load('data/processed/climate/processed_climate_mi.RData')
load('data/processed/climate/processed_climate_mn.RData')
load('data/processed/climate/processed_climate_wi.RData')

# PLS data
load('data/processed/PLS/total.RData')

# Soils data
load('data/processed/soils/processed_flood_il.RData')
load('data/processed/soils/processed_flood_in.RData')
load('data/processed/soils/processed_flood_mi.RData')
load('data/processed/soils/processed_flood_mn.RData')
load('data/processed/soils/processed_flood_wi.RData')
load('data/processed/soils/processed_soil_il.RData')
load('data/processed/soils/processed_soil_in.RData')
load('data/processed/soils/processed_soil_mi.RData')
load('data/processed/soils/processed_soil_mn.RData')
load('data/processed/soils/processed_soil_wi.RData')

# Topography data
load('data/processed/topography/processed_topo_il.RData')
load('data/processed/topography/processed_topo_in.RData')
load('data/processed/topography/processed_topo_mi.RData')
load('data/processed/topography/processed_topo_mn.RData')
load('data/processed/topography/processed_topo_wi.RData')

# Formatting
IL_clim <- IL_clim |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(prism_x, prism_y))
IN_clim <- IN_clim |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(prism_x, prism_y))
MI_clim <- MI_clim |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(prism_x, prism_y))
MN_clim <- MN_clim |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(prism_x, prism_y))
WI_clim <- WI_clim |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(prism_x, prism_y))
clim <- rbind(IL_clim, IN_clim, MI_clim, MN_clim, WI_clim)
IL_soil <- IL_soil |>
  dplyr::left_join(y = IL_flood, by = c('pls_x', 'pls_y', 'soil_x', 'soil_y')) |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(soil_x, soil_y))
IN_soil <- IN_soil |>
  dplyr::left_join(y = IN_flood, by = c('pls_x', 'pls_y', 'soil_x', 'soil_y')) |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(soil_x, soil_y))
MI_soil <- MI_soil |>
  dplyr::left_join(y = MI_flood, by = c('pls_x', 'pls_y', 'soil_x', 'soil_y')) |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(soil_x, soil_y))
MN_soil <- MN_soil |>
  dplyr::left_join(y = MN_flood, by = c('pls_x', 'pls_y', 'soil_x', 'soil_y')) |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(soil_x, soil_y))
WI_soil <- WI_soil |>
  dplyr::left_join(y = WI_flood, by = c('pls_x', 'pls_y', 'soil_x', 'soil_y')) |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(soil_x, soil_y))
soil <- rbind(IL_soil, IN_soil, MI_soil, MN_soil, WI_soil)
IL_topo <- IL_topo |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(topo_x, topo_y))
IN_topo <- IN_topo |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(topo_x, topo_y))
MI_topo <- MI_topo |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(topo_x, topo_y))
MN_topo <- MN_topo |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(topo_x, topo_y))
WI_topo <- WI_topo |>
  dplyr::rename(x = pls_x,
                y = pls_y) |>
  dplyr::select(-c(topo_x, topo_y))
topo <- rbind(IL_topo, IN_topo, MI_topo, MN_topo, WI_topo)

# Combine with taxon level data
taxon_combined <- taxon |>
  dplyr::left_join(y = clim, by = c('x', 'y')) |>
  dplyr::left_join(y = soil, by = c('x', 'y')) |>
  dplyr::left_join(y = topo, by = c('x', 'y'))

# Combine with ecosystem level data
ecosystem_combined <- ecosystem |>
  dplyr::left_join(y = clim, by = c('x', 'y')) |>
  dplyr::left_join(y = soil, by = c('x', 'y')) |>
  dplyr::left_join(y = topo, by = c('x', 'y'))

# Unique locations
locs <- taxon_combined |>
  dplyr::mutate(loc = paste0(x,'_',y)) |>
  dplyr::summarize(loc = unique(loc))

# Number of locations in each group
n_train <- round(nrow(locs) * 0.6)
n_test <- round(nrow(locs) * 0.2)
n_val <- nrow(locs) - n_train - n_test

# Randomly sample all locations for training set with size = n_train
train_ind <- sample(x = locs$loc, size = n_train, replace = FALSE)

# Format
locs <- locs$loc
# Remove locations put into the training set
locs <- locs[which(!(locs %in% train_ind))]

# Randomly sample remaining locations for testing set with size = n_test
test_ind <- sample(x = locs, size = n_test, replace = FALSE)

# Remove locations put into the training set
# Remaining locations are validation set
val_ind <- locs[which(!(locs %in% test_ind))]

# Use index vectors to filter taxon_combined and ecosystem_combined
# into each set
taxon_train <- taxon_combined |>
  dplyr::mutate(loc = paste0(x,'_',y)) |>
  dplyr::filter(loc %in% train_ind) |>
  dplyr::select(-loc)
taxon_test <- taxon_combined |>
  dplyr::mutate(loc = paste0(x,'_',y)) |>
  dplyr::filter(loc %in% test_ind) |>
  dplyr::select(-loc)
taxon_val <- taxon_combined |>
  dplyr::mutate(loc = paste0(x,'_',y)) |>
  dplyr::filter(loc %in% val_ind) |>
  dplyr::select(-loc)
ecosystem_train <- ecosystem_combined |>
  dplyr::mutate(loc = paste0(x,'_',y)) |>
  dplyr::filter(loc %in% train_ind) |>
  dplyr::select(-loc)
