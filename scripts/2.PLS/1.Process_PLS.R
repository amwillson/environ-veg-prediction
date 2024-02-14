## Formatting & preparing vegetation data

rm(list = ls())

# There are six files that we need to format separately:
# Indiana
# Illinois
# Minnesota
# Wisconsin
# Upper Michigan
# Lower Michigan

#### INDIANA ####
indiana_raw <- readr::read_csv(file = 'data/raw/pls/ndinpls_v2.1.csv')

# Species translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Cypress is both bald cypress and tamarack. Make bald cypress
translation <- translation[-1462,]

indiana <- indiana_raw |>  
# Select only necessary rows
  dplyr::select(x, y, # coordinates in meters in epsg:3175
                state, # of the PLS survey
                L1_tree1, # species name for first tree at corner
                L1_tree2, # tree 2
                L1_tree3, # tree 3
                L1_tree4) |> # tree 4
  # Pivot longer so each row corresponds to a tree
  tidyr::pivot_longer(cols = c(L1_tree1, L1_tree2, L1_tree3, L1_tree4),
                      names_to = 'tree', values_to = 'level1') |>
  # Remove empty rows. These are typically tree 2-4, when only one tree was recorded
  dplyr::filter(!is.na(level1)) |>
  # Process species names using translation table to level 3a
  dplyr::left_join(y = translation, by = 'level1') |>
  # remove level1 taxon name column because we can now focus on the level3a level
  dplyr::select(-level1) |>
  # rename because level3a is a weird way of saying the taxon-level
  dplyr::rename(species = level3a) |>
  # rename the taxa
  # we group some taxa here to be consistent with the ecology of the region
  # and to simplify the number of taxonomic categories
  dplyr::mutate(species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Black gum/sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Sycamore', 'sycamore', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Hackberry', 'hackberry', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Mulberry', 'mulberry', species),
                species = dplyr::if_else(species == 'Locust', 'locust', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Unknown tree', 'unknown_tree', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Buckeye', 'buckeye', species),
                species = dplyr::if_else(species == 'Willow', 'willow', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Chestnut', 'chestnut', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Bald cypress', 'cypress', species),
                species = dplyr::if_else(species == 'Alder', 'alder', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species)) |>
  # Remove level3a categories that are not trees
  dplyr::filter(species != 'No data',
                species != 'Water')

# Convert coordinates
# make a spatial object
indiana <- sf::st_as_sf(indiana, coords = c('x', 'y'))
# add the current projection
sf::st_crs(indiana) <- 'EPSG:3175'
# transform the projection
indiana <- sf::st_transform(indiana, crs = 'EPSG:4326')
# change back to regular dataframe
indiana <- sfheaders::sf_to_df(indiana, fill = TRUE)
# remove unnecessary columns
indiana <- dplyr::select(indiana, -c(sfg_id, point_id))

# Pivot wider and make biomes
# prairie if no tree
# savanna if oak and/or hickory and no other species
# forest if any other tree
indiana_ecosystem <- indiana |>
  # change the name of each tree to make it easier to  type the mutate command
  dplyr::mutate(tree = dplyr::if_else(tree == 'L1_tree1', 'one', tree),
                tree = dplyr::if_else(tree == 'L1_tree2', 'two', tree)) |>
  # pivot wider, so each tree 1-2 is a column. Now we can work across rows
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  # If the row has no trees = prairie, oak and/or hickory = savanna, else forest
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA), 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

# Plot at taxon-level
states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

indiana |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

indiana_ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(indiana, indiana_ecosystem, file = 'data/processed/PLS/indiana_format.RData')

#### ILLINOIS ####
rm(list = ls())

illinois_raw <- readr::read_csv('data/raw/pls/ndilpls_v2.1.csv')

# Species translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Take out cypress = tamarack column, as above
translation <- translation[-1462,]
# Make white = 'Unknown tree' because this is more conservative than "Oak'
translation <- translation[-1562,]

# Repeat steps from Indiana
illinois <- illinois_raw |>
  dplyr::select(x, y, # coordinates in meters in epsg:3175
                state, # of the PLS survey
                L1_tree1, # species name for first tree at corner
                L1_tree2, # tree 2
                L1_tree3, # tree 3
                L1_tree4) |> # tree 4
  # Pivot longer so each row corresponds to a tree
  tidyr::pivot_longer(cols = c(L1_tree1, L1_tree2, L1_tree3, L1_tree4),
                      names_to = 'tree', values_to = 'level1') |>
  # Remove empty rows. These are typically tree 2-4, when only one tree was recorded
  dplyr::filter(!is.na(level1)) |>
  # Process species names using a translation table to level 3a
  dplyr::left_join(y = translation, by = 'level1') |>
  dplyr::select(-level1) |>
  dplyr::rename(species = level3a) |>
  dplyr::mutate(species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Black gum/sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Hackberry', 'hackberry', species),
                species = dplyr::if_else(species == 'Mulberry', 'mulberry', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Locust', 'locust', species),
                species = dplyr::if_else(species == 'Unknown tree', 'unknown_tree', species),
                species = dplyr::if_else(species == 'Bald cypress', 'cypress', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Sycamore', 'sycamore', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Willow', 'willow', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Buckeye', 'buckeye', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species)) |>
  dplyr::filter(species != 'Water',
                species != 'No data')

# Convert coordinates
illinois <- sf::st_as_sf(illinois, coords = c('x', 'y'))
sf::st_crs(illinois) <- 'EPSG:3175'
illinois <- sf::st_transform(illinois, crs = 'EPSG:4326')
illinois <- sfheaders::sf_to_df(illinois, fill = TRUE)
illinois <- dplyr::select(illinois, -c(sfg_id, point_id))

# Pivot wider and make biomes
# prairie if no tree
# savanna if oak and/or hickory and no other species
# forest if any other tree
illinois_ecosystem <- illinois |>
  dplyr::mutate(tree = dplyr::if_else(tree == 'L1_tree1', 'one', tree),
                tree = dplyr::if_else(tree == 'L1_tree2', 'two', tree),
                tree = dplyr::if_else(tree == 'L1_tree3', 'three', tree),
                tree = dplyr::if_else(tree == 'L1_tree4', 'four', tree)) |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two) & is.na(three) & is.na(four), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA) &
                                             three %in% c('oak', 'hickory', NA) & four %in% c('oak', 'hickory', NA), 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

# Plot at taxon-level
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

illinois |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

illinois_ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(illinois, illinois_ecosystem, file = 'data/processed/PLS/illinois_format.RData')

#### WISCONSIN ####
rm(list = ls())

wisconsin_raw <- readr::read_csv('data/raw/pls/wi_pls_projected_v1.1.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Take out HM = 'Hemlock' in favor of HM = 'No tree' (home)
translation <- translation[-1488,]

# Repeat steps from Indiana
wisconsin <- wisconsin_raw |>
  dplyr::mutate(state = 'WI') |>
  dplyr::select(x_alb, y_alb,
                state,
                SP1,
                SP2,
                SP3,
                SP4) |>
  tidyr::pivot_longer(cols = c(SP1, SP2, SP3, SP4), 
                      names_to = 'tree', values_to = 'level1') |>
  dplyr::filter(!is.na(level1)) |>
  dplyr::left_join(translation, by = 'level1') |>
  dplyr::select(-level1) |>
  dplyr::rename(species = level3a) |>
  dplyr::mutate(species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Willow', 'willow', species),
                species = dplyr::if_else(species == 'Hackberry', 'hackberry', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Alder', 'alder', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Unknown tree', 'unknown_tree', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Sycamore', 'sycamore', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species)) |>
  dplyr::filter(species != 'Water',
                species != 'No data')

# Convert coordinates
wisconsin <- sf::st_as_sf(wisconsin, coords = c('x_alb', 'y_alb'))
sf::st_crs(wisconsin) <- 'EPSG:3175'
wisconsin <- sf::st_transform(wisconsin, crs = 'EPSG:4326')
wisconsin <- sfheaders::sf_to_df(wisconsin, fill = TRUE)
wisconsin <- dplyr::select(wisconsin, -c(sfg_id, point_id))

wisconsin_ecosystem <- wisconsin |>
  dplyr::mutate(tree = dplyr::if_else(tree == 'SP1', 'one', tree),
                tree = dplyr::if_else(tree == 'SP2', 'two', tree),
                tree = dplyr::if_else(tree == 'SP3', 'three', tree),
                tree = dplyr::if_else(tree == 'SP4', 'four', tree)) |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two) & is.na(three) & is.na(four), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA) &
                                             three %in% c('oak', 'hickory', NA) & four %in% c('oak', 'hickory', NA), 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

# Plot
states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

wisconsin |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

wisconsin_ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(wisconsin, wisconsin_ecosystem, file = 'data/processed/PLS/wisconsin_process.RData')

#### MINNESOTA ####
rm(list = ls())

minnesota_raw <- readr::read_csv('data/raw/pls/mn_pls_projected_v1.1.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Repeat steps from Indiana
minnesota <- minnesota_raw |>
  dplyr::mutate(state = 'MN') |>
  dplyr::select(x_alb, y_alb,
                state,
                SP1,
                SP2,
                SP3,
                SP4) |>
  tidyr::pivot_longer(cols = c(SP1, SP2, SP3, SP4),
                      names_to = 'tree', values_to = 'level1') |>
  dplyr::filter(!is.na(level1),
                level1 != '_') |>
  dplyr::left_join(translation, by = 'level1') |>
  dplyr::select(-level1) |>
  dplyr::rename(species = level3a) |>
  dplyr::mutate(species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Willow', 'willow', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Alder', 'alder', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Hackberry', 'hackberry', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Buckeye', 'buckeye', species),
                species = dplyr::if_else(species == 'Sycamore', 'sycamore', species)) |>
  dplyr::filter(species != 'Missing') |>
  # Remove some repeat rows.
  # We know these are repeats because we have labeled the trees 1-4
  # and there can only be these trees at a given corner
  dplyr::distinct()

# Convert coordinates
minnesota <- sf::st_as_sf(minnesota, coords = c('x_alb', 'y_alb'))
sf::st_crs(minnesota) <- 'EPSG:3175'
minnesota <- sf::st_transform(minnesota, crs = 'EPSG:4326')
minnesota <- sfheaders::sf_to_df(minnesota, fill = TRUE)
minnesota <- dplyr::select(minnesota, -c(sfg_id, point_id))

# Convert to ecosystem type
minnesota_ecosystem <- minnesota |>
  # Rename tree numbers
  dplyr::mutate(tree = dplyr::if_else(tree == 'SP1', 'one', tree),
                tree = dplyr::if_else(tree == 'SP2', 'two', tree),
                tree = dplyr::if_else(tree == 'SP3', 'three', tree),
                tree = dplyr::if_else(tree == 'SP4', 'four', tree)) |>
  # Pivot wider, values_fn allows things to be put in a list
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  # Unnest when things were put in a list
  # I have no idea why there are duplicates for some locations though
  tidyr::unnest(cols = everything()) |>
  # Make ecosystem level based on no tree, oak/hickory, anything else
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two) & is.na(three) & is.na(four), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA) &
                                             three %in% c('oak', 'hickory', NA) & four %in% c('oak', 'hickory', NA), 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

# Some corners were duplicated, but also contain different taxa.
# We'll remove the second of each of these columns
# It's a very small number of points and the differnece between
# taxa at these corners is relatively minimal
dupes <- minnesota_ecosystem |>
  # Find duplicate x y coordiantes
  janitor::get_dupes(x,y) |>
  # Make a row uniquely identifying the columns
  # use all taxa because the rows should have the same
  # x y coordinates and differ in their taxa
  dplyr::mutate(ind = paste0(x,y,one,two,three,four)) |>
  # record row number
  dplyr::mutate(row = dplyr::row_number()) |>
  # since there are 2 identical xy coordinate pairs for each
  # remaining duplicate, we can remove even columns
  # and keep odd columns
  dplyr::mutate(del = dplyr::if_else(row %% 2 == 0, 'yes', 'no')) |>
  dplyr::select(ind, del)

# Now we need to actually remove those rows from
# the ecosystem dataset
minnesota_ecosystem <- minnesota_ecosystem |>
  # make the same indexing column as in the df above
  dplyr::mutate(ind = paste0(x,y,one,two,three,four)) |>
  # join the dfs by the indexing column
  dplyr::left_join(y = dupes, by = 'ind') |>
  # remove any rows that were flagged as "remove"
  dplyr::filter(del %in% c('no', NA)) |>
  dplyr::select(-del, -ind)

# Plot
states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

minnesota |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

minnesota_ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(minnesota, minnesota_ecosystem, file = 'data/processed/PLS/minnesota_process.RData')

#### UPPER MICHIGAN ####
rm(list = ls())

upmichigan_raw <- readr::read_csv('data/raw/pls/northernmi_pls_projected_v1.3.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Repeat steps from Minnesota
# This includes filtering out repeat rows
upmichigan <- upmichigan_raw |>
  dplyr::mutate(state = 'UPMI') |>
  dplyr::select(x_alb, y_alb,
                state,
                SPP1,
                SPP2,
                SPP3,
                SPP4) |>
  tidyr::pivot_longer(cols = c(SPP1, SPP2, SPP3, SPP4),
                      names_to = 'tree', values_to = 'level1') |>
  dplyr::filter(!is.na(level1)) |>
  dplyr::left_join(translation, by = 'level1') |>
  dplyr::select(-level1) |>
  dplyr::rename(species = level3a) |>
  dplyr::mutate(species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Unknown tree', 'unknown_tree', species),
                species = dplyr::if_else(species == 'Willow', 'willow', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Alder', 'alder', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Sycamore', 'sycamore', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Hackberry', 'hackberry', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species)) |>
  dplyr::filter(species != 'Missing') |>
  dplyr::distinct()

# Convert coordinates
upmichigan <- sf::st_as_sf(upmichigan, coords = c('x_alb', 'y_alb'))
sf::st_crs(upmichigan) <- 'EPSG:3175'
upmichigan <- sf::st_transform(upmichigan, crs = 'EPSG:4326')
upmichigan <- sfheaders::sf_to_df(upmichigan, fill = TRUE)
upmichigan <- dplyr::select(upmichigan, -c(sfg_id, point_id))

upmichigan_ecosystem <- upmichigan |>
  dplyr::mutate(tree = dplyr::if_else(tree == 'SPP1', 'one', tree),
                tree = dplyr::if_else(tree == 'SPP2', 'two', tree)) |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species', 
                     values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA), 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

dupes <- upmichigan_ecosystem |>
  janitor::get_dupes(x,y) |>
  dplyr::mutate(ind = paste0(x,y,one,two)) |>
  dplyr::mutate(row = dplyr::row_number()) |>
  dplyr::mutate(del = dplyr::if_else(row %% 2 == 0, 'yes', 'no')) |>
  dplyr::select(ind, del)

upmichigan_ecosystem <- upmichigan_ecosystem |>
  dplyr::mutate(ind = paste0(x,y,one,two)) |>
  dplyr::left_join(y = dupes, by = 'ind') |>
  dplyr::filter(del %in% c('no', NA)) |>
  dplyr::select(-del,-ind)

# Plot
states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

upmichigan |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

upmichigan_ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(upmichigan, upmichigan_ecosystem, file = 'data/processed/PLS/upmichigan_process.RData')

#### LOWER MICHIGAN ####
rm(list = ls())

lowmichigan_raw <- readr::read_csv('data/raw/pls/southernmi_projected_v1.6.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Remove HM = 'No tree' in foavor of HM = 'Hemlock'
translation <- translation[-1645,]

# Repeat steps from Indiana
lowmichigan <- lowmichigan_raw |>
  dplyr::mutate(state = 'LOMI') |>
  dplyr::select(point_x, point_y,
                state,
                species1,
                species2,
                species3,
                species4) |>
  tidyr::pivot_longer(cols = c(species1, species2, species3, species4),
                      names_to = 'tree', values_to = 'level1') |>
  dplyr::filter(!is.na(level1)) |>
  dplyr::left_join(translation, by = 'level1') |>
  dplyr::select(-level1) |>
  dplyr::rename(species = level3a) |>
  dplyr::mutate(species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Sycamore', 'sycamore', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Unknown tree', 'unknown_tree', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Willow', 'willow', species),
                species = dplyr::if_else(species == 'Black gum/sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Alder', 'alder', species),
                species = dplyr::if_else(species == 'Hackberry', 'hackberry', species),
                species = dplyr::if_else(species == 'Mulberry', 'mulberry', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Buckeye', 'buckeye', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Chestnut', 'chestnut', species)) |>
  dplyr::filter(!is.na(species)) |>
  dplyr::distinct()

# Convert coordinates
lowmichigan <- sf::st_as_sf(lowmichigan, coords = c('point_x', 'point_y'))
sf::st_crs(lowmichigan) <- 'EPSG:3175'
lowmichigan <- sf::st_transform(lowmichigan, crs = 'EPSG:4326')
lowmichigan <- sfheaders::sf_to_df(lowmichigan, fill = TRUE)
lowmichigan <- dplyr::select(lowmichigan, -c(sfg_id, point_id))

lowmichigan_ecosystem <- lowmichigan |>
  dplyr::mutate(tree = dplyr::if_else(tree == 'species1', 'one', tree),
                tree = dplyr::if_else(tree == 'species2', 'two', tree)) |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA) , 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

# Plot
states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

lowmichigan |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

lowmichigan_ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(lowmichigan, lowmichigan_ecosystem, file = 'data/processed/PLS/lowmichigan_process.RData')

#### AGGREGATE ####
rm(list = ls())

load('data/processed/PLS/illinois_format.RData')
load('data/processed/PLS/indiana_format.RData')
load('data/processed/PLS/lowmichigan_process.RData')
load('data/processed/PLS/minnesota_process.RData')
load('data/processed/PLS/upmichigan_process.RData')
load('data/processed/PLS/wisconsin_process.RData')

# Taxon-level full dataset
taxon <- rbind(illinois, indiana, lowmichigan, minnesota, upmichigan, wisconsin)

# Take only ecosystem column for ecosystem level datasets
illinois_ecosystem <- dplyr::select(illinois_ecosystem, c(state, x, y, ecosystem))
indiana_ecosystem <- dplyr::select(indiana_ecosystem, c(state, x, y, ecosystem))
lowmichigan_ecosystem <- dplyr::select(lowmichigan_ecosystem, c(state, x, y, ecosystem))
minnesota_ecosystem <- dplyr::select(minnesota_ecosystem, c(state, x, y, ecosystem))
upmichigan_ecosystem <- dplyr::select(upmichigan_ecosystem, c(state, x, y, ecosystem))
wisconsin_ecosystem <- dplyr::select(wisconsin_ecosystem, c(state, x, y, ecosystem))

# Ecosystem-level full dataset
ecosystem <- rbind(illinois_ecosystem, indiana_ecosystem, lowmichigan_ecosystem,
                   minnesota_ecosystem, upmichigan_ecosystem, wisconsin_ecosystem)

# Plot
states <- sf::st_as_sf(maps::map('state', region = c('indiana', 'illinois',
                                                     'michigan',
                                                     'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

pal <- c("#0cc0aa", "#812050", "#39f27a", "#fe35d0", "#c0e15c", "#9620fc", "#459521", "#d0589f", "#1b511d", "#f1b3e3", "#154975")

taxon |>
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'Other hardwood', species),
                species = dplyr::if_else(species == 'hickory', 'Hickory', species),
                species = dplyr::if_else(species == 'ash', 'Ash', species),
                species = dplyr::if_else(species == 'ironwood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'basswood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'locust', 'Other hardwood', species),
                species = dplyr::if_else(species == 'beech', 'Beech', species),
                species = dplyr::if_else(species == 'maple', 'Maple', species),
                species = dplyr::if_else(species == 'birch', 'Birch', species),
                species = dplyr::if_else(species == 'mulberry', 'Other hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'Other hardwood', species),
                species = dplyr::if_else(species == 'no_tree', 'No tree', species),
                species = dplyr::if_else(species == 'buckeye', 'Other hardwood', species),
                species = dplyr::if_else(species == 'oak', 'Oak', species),
                species = dplyr::if_else(species == 'cedar_juniper', 'Other conifer', species),
                species = dplyr::if_else(species == 'other_hardwood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'Other hardwood', species),
                species = dplyr::if_else(species == 'pine', 'Pine', species),
                species = dplyr::if_else(species == 'chestnut', 'Other hardwood', species),
                species = dplyr::if_else(species == 'poplar_tulippoplar', 'Other hardwood', species),
                species = dplyr::if_else(species == 'cypress', 'Other conifer', species),
                species = dplyr::if_else(species == 'spruce', 'Other conifer', species),
                species = dplyr::if_else(species == 'dogwood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'Other hardwood', species),
                species = dplyr::if_else(species == 'elm', 'Other hardwood', species),
                species = dplyr::if_else(species == 'tamarack', 'Other conifer', species),
                species = dplyr::if_else(species == 'fir', 'Other conifer', species),
                species = dplyr::if_else(species == 'unknown_tree', 'Other hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'Other hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'Other hardwood', species),
                species = dplyr::if_else(species == 'hemlock', 'Hemlock', species),
                species = dplyr::if_else(species == 'willow', 'Other hardwood', species)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

# Faceted by taxon
taxon |>
  dplyr::mutate(species = dplyr::if_else(species == 'alder', 'Other hardwood', species),
                species = dplyr::if_else(species == 'hickory', 'Hickory', species),
                species = dplyr::if_else(species == 'ash', 'Ash', species),
                species = dplyr::if_else(species == 'ironwood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'basswood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'locust', 'Other hardwood', species),
                species = dplyr::if_else(species == 'beech', 'Beech', species),
                species = dplyr::if_else(species == 'maple', 'Maple', species),
                species = dplyr::if_else(species == 'birch', 'Birch', species),
                species = dplyr::if_else(species == 'mulberry', 'Other hardwood', species),
                species = dplyr::if_else(species == 'blackgum_sweetgum', 'Other hardwood', species),
                species = dplyr::if_else(species == 'no_tree', 'No tree', species),
                species = dplyr::if_else(species == 'buckeye', 'Other hardwood', species),
                species = dplyr::if_else(species == 'oak', 'Oak', species),
                species = dplyr::if_else(species == 'cedar_juniper', 'Other conifer', species),
                species = dplyr::if_else(species == 'other_hardwood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'cherry', 'Other hardwood', species),
                species = dplyr::if_else(species == 'pine', 'Pine', species),
                species = dplyr::if_else(species == 'chestnut', 'Other hardwood', species),
                species = dplyr::if_else(species == 'poplar_tulippoplar', 'Other hardwood', species),
                species = dplyr::if_else(species == 'cypress', 'Other conifer', species),
                species = dplyr::if_else(species == 'spruce', 'Other conifer', species),
                species = dplyr::if_else(species == 'dogwood', 'Other hardwood', species),
                species = dplyr::if_else(species == 'sycamore', 'Other hardwood', species),
                species = dplyr::if_else(species == 'elm', 'Other hardwood', species),
                species = dplyr::if_else(species == 'tamarack', 'Other conifer', species),
                species = dplyr::if_else(species == 'fir', 'Other conifer', species),
                species = dplyr::if_else(species == 'unknown_tree', 'Other hardwood', species),
                species = dplyr::if_else(species == 'hackberry', 'Other hardwood', species),
                species = dplyr::if_else(species == 'walnut', 'Other hardwood', species),
                species = dplyr::if_else(species == 'hemlock', 'Hemlock', species),
                species = dplyr::if_else(species == 'willow', 'Other hardwood', species)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = species), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~species) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

pal <- c('#25341e', '#f8da1c', '#94bd46')

ecosystem |>
  dplyr::mutate(ecosystem = dplyr::if_else(ecosystem == 'forest', 'Forest', ecosystem),
                ecosystem = dplyr::if_else(ecosystem == 'prairie', 'Prairie', ecosystem),
                ecosystem = dplyr::if_else(ecosystem == 'savanna', 'Savanna', ecosystem)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

# Separate biomes by facet
ecosystem |>
  dplyr::mutate(ecosystem = dplyr::if_else(ecosystem == 'forest', 'Forest', ecosystem),
                ecosystem = dplyr::if_else(ecosystem == 'prairie', 'Prairie', ecosystem),
                ecosystem = dplyr::if_else(ecosystem == 'savanna', 'Savanna', ecosystem)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), alpha = 0.7, shape = '.') +
  ggplot2::labs(color = 'Biome') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~ecosystem) +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326')

save(taxon, ecosystem, file = 'data/processed/PLS/total.RData')
