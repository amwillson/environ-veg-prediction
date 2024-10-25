## Defining training, testing, and validation datasets

rm(list = ls())

# Load total PLS dataset
load('data/processed/grid_joined.RData')

# All grid cells across domain
gridcells <- unique(grid$grid_id)

# Grid cells in the training dataset
train_gridcells <- sample(x = gridcells, size = length(gridcells) * 0.6, replace = FALSE)

# Remove grid cells in the training dataset from the vector of grid cells
gridcells_left <- gridcells[!gridcells %in% train_gridcells]

# Grid cells in the testing dataset
test_gridcells <- sample(x = gridcells_left, size = length(gridcells_left) * 0.5, replace = FALSE)

# Grid cells in the validation dataset are everything that's left
val_gridcells <- gridcells_left[!gridcells_left %in% test_gridcells]

# Map of states
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Check that the locations of the grid cells seem randomly distributed
# over space across the three datasets
grid |>
  dplyr::filter(grid_id %in% train_gridcells) |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(x = median(x),
                   y = median(y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

grid |>
  dplyr::filter(grid_id %in% test_gridcells) |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(x = median(x),
                   y = median(y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

grid |>
  dplyr::filter(grid_id %in% val_gridcells) |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(x = median(x),
                   y = median(y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Check that there is no overlap
any(train_gridcells %in% test_gridcells)
any(train_gridcells %in% val_gridcells)
any(test_gridcells %in% val_gridcells)

# Save
save(train_gridcells, test_gridcells, val_gridcells,
     file = 'data/processed/train_test_val_split.RData')
