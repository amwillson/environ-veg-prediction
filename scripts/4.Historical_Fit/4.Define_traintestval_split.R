## Defining training, testing, and validation datasets

rm(list = ls())

# Load total PLS dataset
load('data/processed/PLS/total_matched.RData')

# All grid cells across domain
grid <- unique(ecosystem_matched$grid_id)

# Grid cells in the training dataset
train_grid <- sample(x = grid, size = length(grid) * 0.6, replace = FALSE)

# Remove grid cells in the training dataset from the vector of grid cells
grid_left <- grid[!grid %in% train_grid]

# Grid cells in the testing dataset
test_grid <- sample(x = grid_left, size = length(grid_left) * 0.5, replace = FALSE)

# Grid cells in the validation dataset are everything that's left
val_grid <- grid_left[!grid_left %in% test_grid]

# Map of states
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Check that the locations of the grid cells seem randomly distributed
# over space across the three datasets
ecosystem_matched |>
  dplyr::filter(grid_id %in% train_grid) |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(x = median(x),
                   y = median(y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

ecosystem_matched |>
  dplyr::filter(grid_id %in% test_grid) |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(x = median(x),
                   y = median(y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

ecosystem_matched |>
  dplyr::filter(grid_id %in% val_grid) |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(x = median(x),
                   y = median(y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Check that there is no overlap
any(train_grid %in% test_grid)
any(train_grid %in% val_grid)
any(test_grid %in% val_grid)

# Only training grid cells
train_point <- ecosystem_matched |>
  dplyr::filter(grid_id %in% train_grid)
# Points in the training dataset
train_point <- unique(train_point$ecosystem_id)

# Only testing grid cells
test_point <- ecosystem_matched |>
  dplyr::filter(grid_id %in% test_grid)
# Points in the testing dataset
test_point <- unique(test_point$ecosystem_id)

# Only validation grid cells
val_point <- ecosystem_matched |>
  dplyr::filter(grid_id %in% val_grid)
# Points in the validation dataset
val_point <- unique(val_point$ecosystem_id)

# Plot again. Should be random but more filled in
ecosystem_matched |>
  dplyr::filter(ecosystem_id  %in% train_point) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

ecosystem_matched |>
  dplyr::filter(ecosystem_id %in% test_point) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

ecosystem_matched |>
  dplyr::filter(ecosystem_id %in% val_point) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Check to make sure there's no overlap
any(train_point %in% test_point)
any(train_point %in% val_point)
any(test_point %in% val_point)

# Save
save(train_grid, test_grid, val_grid,
     train_point, test_point, val_point,
     file = 'data/processed/train_test_val_split.RData')
