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

# Indiana PLS data have undergone extensive quality control already
# There are no duplicate rows and no partial duplicate rows that appear
# to truly be duplicate, and not just similar trees recorded at different locations
# Therefore, we skip steps involving removing duplicates

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
  dplyr::mutate(species = dplyr::if_else(species == 'Alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Black gum/sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Chestnut', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Unknown tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Willow', 'other_hardwood', species)) |>
  # Remove level3a categories that are not trees
  dplyr::filter(species != 'No data',
                species != 'Water',
                species != 'Bald cypress')

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

# Like Indiana, Illinois has undergone extensive quality control
# There are no complete duplicate rows and partial duplicates
# appear to represent real similarities in the trees recorded
# at different locations
# All points are kept

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
  dplyr::mutate(species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Black gum/sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Locust', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Unknown tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Willow', 'other_hardwood', species)) |>
  dplyr::filter(species != 'Water',
                species != 'No data',
                species != 'Bald cypress')

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

wisconsin_raw <- readr::read_csv('data/raw/pls/PLS_Wisconsin_trees_Level0_v1.0.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Take out HM = 'Hemlock' in favor of HM = 'No tree' (home)
translation <- translation[-1488,]

# Like Indiana and Illinois, Wisconsin has been through a extensive quality control
# There are no complete duplicates and the incomplete duplicates
# appear to be true similarities in the data

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
  dplyr::mutate(species = dplyr::if_else(species == 'Alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Unknown tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Willow', 'other_hardwood', species)) |>
  dplyr::filter(species != 'Water',
                species != 'No data',
                species != 'NA')

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

minnesota_raw <- readr::read_csv('data/raw/pls/PLS_Minnesota_trees_Level0_v1.0.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Replace "_" with "no tree" in the first species column
# We know that the corners (southwest corner of the state)
# that have NA in the first species column are prairie
# according to this paper: https://doi.org/10.1371/journal.pone.0151935
minnesota_raw <- minnesota_raw |>
  dplyr::mutate(SP1 = dplyr::if_else(SP1 == '_', 'no tree', SP1))

# In Minnesota, there are some data problems. There are duplicate
# and near-duplicate rows that we need to remove.
# We can see the locations of these duplicates:
all_duplicates <- minnesota_raw |>
  janitor::get_dupes()

duplicates_removed <- minnesota_raw |>
  dplyr::distinct()

duplicate_corners <- duplicates_removed |>
  janitor::get_dupes(x_alb, y_alb)

states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Shows that the rows that are completely the same
# occur along county lines
all_duplicates |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x_alb, y = y_alb), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Shows that rows that are partial duplicates, with
# different species at the same location are more infrequent
# and also along county lines
duplicate_corners |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x_alb, y = y_alb), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# First remove repeat rows
minnesota <- minnesota_raw |>
  # Complete repeats
  # These occur along county lines and occur when
  # a transcriber included both the east and west
  # or north and south county boundary
  # The procedure for other states has been to remove duplicates of
  # this type during data cleaning
  dplyr::distinct() # removes 3,511 points

# We are now left with two types of partial duplicates
# The first type is corners with the same coordinates
# but different species
# We completely remove these corners because we don't
# know which species are "correct"

# Find partial duplicates of type 1
partial_dupes_sameloc <- minnesota |>
  janitor::get_dupes(x_alb, y_alb) |>
  dplyr::mutate(coord = paste0(x_alb,y_alb))

# Remove these by filtering by the coordinate pairs
minnesota <- minnesota |>
  dplyr::mutate(coord = paste0(x_alb,y_alb)) |>
  dplyr::filter(!(coord %in% partial_dupes_sameloc$coord)) # removes 782 rows

# The second type is corners with the same tree information
# but slightly different coordinates
# These are likely due to an error in surveyor notes
# We completely remove these corners because we don't 
# know which location is "correct"

# Find partial duplicates of type 2
partial_dupes_samesp <- minnesota |>
  janitor::get_dupes(SP1, DIAM1, AZ1, DIST1,
                     SP2, DIAM2, AZ2, DIST2,
                     SP3, DIAM3, AZ3, DIST3,
                     SP4, DIAM4, AZ4, DIST4) |>
  dplyr::filter(AZ1 != '_') |>
  dplyr::mutate(coord = paste0(x_alb,y_alb))

dupe_count_3s <- partial_dupes_samesp |>
  # Filter for when there are three "duplicate" corners
  dplyr::filter(dupe_count == 3) |>
  # Make groups of three
  dplyr::mutate(groups = rep(1:(length(dupe_count)/3), each = 3)) |>
  # Take out the first 2 groups because after 
  # plotting it is clear that they are not close together
  dplyr::filter(groups %in% 3:6) |>
  dplyr::mutate(coord = paste0(x_alb,y_alb))

# All 2-point duplicates appear close together visually
dupe_count_2s <- partial_dupes_samesp |>
  dplyr::filter(dupe_count == 2) |>
  dplyr::mutate(groups = rep(1:(length(dupe_count)/2), each = 2)) |>
  dplyr::mutate(coord = paste0(x_alb,y_alb))

# Remove these by filtering by the coordinate pairs
minnesota <- minnesota |>
  dplyr::filter(!(coord %in% dupe_count_3s$coord)) |>
  dplyr::filter(!(coord %in% dupe_count_2s$coord)) |>
  dplyr::select(-coord)

# Now, repeat steps from Indiana
minnesota <- minnesota |>
  dplyr::mutate(state = 'MN') |>
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
  dplyr::mutate(species = dplyr::if_else(species == 'Alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Willow', 'other_hardwood', species)) |>
  dplyr::filter(species != 'Missing',
                !is.na(species))

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
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  # Make ecosystem level based on no tree, oak/hickory, anything else
  dplyr::mutate(ecosystem = dplyr::if_else(one == 'no_tree' & is.na(two) & is.na(three) & is.na(four), 'prairie', NA),
                ecosystem = dplyr::if_else(one %in% c('oak', 'hickory') & two %in% c('oak', 'hickory', NA) &
                                             three %in% c('oak', 'hickory', NA) & four %in% c('oak', 'hickory', NA), 'savanna', ecosystem),
                ecosystem = dplyr::if_else(is.na(ecosystem), 'forest', ecosystem))

# Plot
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

upmichigan_raw <- readr::read_csv('data/raw/pls/PLS_northernMichigan_trees_Level0_v1.0.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Repeat steps from Minnesota
# This includes filtering out
# duplicate and partial duplicate rows
upmichigan <- upmichigan_raw |>
  dplyr::distinct() # 0 rows removed

partial_dupes_sameloc <- upmichigan |>
  janitor::get_dupes(x_alb,y_alb) |>
  dplyr::mutate(coord = paste0(x_alb,y_alb))

upmichigan <- upmichigan |>
  dplyr::mutate(coord = paste0(x_alb,y_alb)) |>
  dplyr::filter(!(coord %in% partial_dupes_sameloc$coord))# 158 rows removed

partial_dupes_samesp <- upmichigan |>
  janitor::get_dupes(SPP1, DBH1, AZIMUTH, DIST1,
                     SPP2, DBH2, AZIMUTH2, DIST2,
                     SPP3, DBH3, AZIMUTH3, DIST3,
                     SPP4, DBH4, AZIMUTH4, DIST4) |>
  dplyr::filter(!is.na(SPP1)) |>
  dplyr::mutate(coord = paste0(x_alb,y_alb))

# It appears that the rows same species information at different locations
# represent true similar observations at multiple corners, so
# we keep all observations

upmichigan <- upmichigan |>
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
  dplyr::mutate(species = dplyr::if_else(species == 'Alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'No tree', 'no_tree', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Unknown tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Willow', 'other_hardwood', species)) |>
  dplyr::filter(species != 'Missing',
                species != 'NA')

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

lowmichigan_raw <- readr::read_csv('data/raw/pls/PLS_southernMichigan_trees_Level0_v1.0.csv')

# Load taxon translation table
translation <- readr::read_csv('data/raw/pls/level0_to_level3a_v1/level0_to_level3a_v1.2.csv')

# Take only columns we need and change column names to match state data
translation <- translation |>
  dplyr::select(level1, level3a)

# Take out repeat columns
translation <- unique(translation)

# Remove HM = 'No tree' in foavor of HM = 'Hemlock'
translation <- translation[-1645,]

# Check for any duplicates, as in minnesota and upper michigan
lowmichigan <- lowmichigan_raw |>
  dplyr::distinct()# 0 rows removed

partial_dupes_sameloc <- lowmichigan |>
  janitor::get_dupes(point_x, point_y) |>
  dplyr::mutate(coord = paste0(point_x, point_y))

lowmichigan <- lowmichigan |>
  dplyr::mutate(coord = paste0(point_x, point_y)) |>
  dplyr::filter(!(coord %in% partial_dupes_sameloc$coord)) # 212 rows removed

partial_dupes_samesp <- lowmichigan |>
  janitor::get_dupes(species1, diam1, q1, az1, az1_360, dist1,
                     species2, diam2, q2, az2, az2_360, dist2,
                     species3, diam3, q3, az3, az3_360, dist3,
                     species4, diam4, q4, az4, az4_360, dist4) |>
  dplyr::mutate(coord = paste0(point_x,point_y))

lowmichigan <- lowmichigan |>
  dplyr::filter(!(coord %in% partial_dupes_samesp$coord)) |> # 52 rows removed
  dplyr::select(-coord)

# Repeat steps from Indiana
lowmichigan <- lowmichigan |>
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
  dplyr::mutate(species = dplyr::if_else(species == 'Alder', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Ash', 'ash', species),
                species = dplyr::if_else(species == 'Basswood', 'basswood', species),
                species = dplyr::if_else(species == 'Beech', 'beech', species),
                species = dplyr::if_else(species == 'Birch', 'birch', species),
                species = dplyr::if_else(species == 'Black gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Black gum/sweet gum', 'blackgum_sweetgum', species),
                species = dplyr::if_else(species == 'Buckeye', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Cedar/juniper', 'cedar_juniper', species),
                species = dplyr::if_else(species == 'Cherry', 'cherry', species),
                species = dplyr::if_else(species == 'Chestnut', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Dogwood', 'dogwood', species),
                species = dplyr::if_else(species == 'Elm', 'elm', species),
                species = dplyr::if_else(species == 'Fir', 'fir', species),
                species = dplyr::if_else(species == 'Hackberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Hemlock', 'hemlock', species),
                species = dplyr::if_else(species == 'Hickory', 'hickory', species),
                species = dplyr::if_else(species == 'Ironwood', 'ironwood', species),
                species = dplyr::if_else(species == 'Maple', 'maple', species),
                species = dplyr::if_else(species == 'Mulberry', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Oak', 'oak', species),
                species = dplyr::if_else(species == 'Other hardwood', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Pine', 'pine', species),
                species = dplyr::if_else(species == 'Poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Poplar/tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Spruce', 'spruce', species),
                species = dplyr::if_else(species == 'Sycamore', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Tamarack', 'tamarack', species),
                species = dplyr::if_else(species == 'Tulip poplar', 'poplar_tulippoplar', species),
                species = dplyr::if_else(species == 'Unknown tree', 'other_hardwood', species),
                species = dplyr::if_else(species == 'Walnut', 'walnut', species),
                species = dplyr::if_else(species == 'Willow', 'other_hardwood', species)) |>
  dplyr::filter(!is.na(species))

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
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 14),
                legend.title = ggplot2::element_text(size = 16))

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
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 16),
                 legend.text = ggplot2::element_text(size = 14))

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
