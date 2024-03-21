## Separate training, testing, and validation sets

rm(list = ls())

# Load splits
load('data/processed/train_test_val_split.RData')

# Load combined data
load('data/processed/grid_joined.RData')
load('data/processed/point_joined.RData')

# Split ecosystem-level (wide) point-level data
ecosystem_point_train <- ecosystem_point |>
  dplyr::filter(grid_id %in% train_gridcells)
ecosystem_point_test <- ecosystem_point |>
  dplyr::filter(grid_id %in% test_gridcells)
ecosystem_point_val <- ecosystem_point |>
  dplyr::filter(grid_id %in% val_gridcells)

# Check that every row is included
nrow(ecosystem_point_train) + nrow(ecosystem_point_test) + nrow(ecosystem_point_val) == nrow(ecosystem_point)
any(ecosystem_point_train$grid_id %in% ecosystem_point_test$grid_id)
any(ecosystem_point_train$grid_id %in% ecosystem_point_val$grid_id)
any(ecosystem_point_test$grid_id %in% ecosystem_point_val$grid_id)
any(ecosystem_point_train$ecosystem_id %in% ecosystem_point_test$ecosystem_id)
any(ecosystem_point_train$ecosystem_id %in% ecosystem_point_val$ecosystem_id)
any(ecosystem_point_test$ecosystem_id %in% ecosystem_point_val$ecosystem_id)

# Save
save(ecosystem_point_train, file = 'data/processed/training/ecosystem_point_train.RData')
save(ecosystem_point_test, file = 'data/processed/testing/ecosystem_point_test.RData')
save(ecosystem_point_val, file = 'data/processed/validation/ecosystem_point_val.RData')

# Split tree-level (long) point-level data
point_train <- point |>
  dplyr::filter(grid_id %in% train_gridcells)
point_test <- point |>
  dplyr::filter(grid_id %in% test_gridcells)
point_val <- point |>
  dplyr::filter(grid_id %in% val_gridcells)

# Check that every row is included
nrow(point_train) + nrow(point_test) + nrow(point_val) == nrow(point)
any(point_train$grid_id %in% point_test$grid_id)
any(point_train$grid_id %in% point_val$grid_id)
any(point_test$grid_id %in% point_val$grid_id)
any(point_train$ecosystem_id %in% point_test$ecosystem_id)
any(point_train$ecosystem_id %in% point_val$ecosystem_id)
any(point_test$ecosystem_id %in% point_val$ecosystem_id)

# Save
save(point_train, file = 'data/processed/training/point_train.RData')
save(point_test, file = 'data/processed/testing/point_test.RData')
save(point_val, file = 'data/processed/validation/point_val.RData')

# Split gridded data
grid_train <- grid |>
  dplyr::filter(grid_id %in% train_gridcells)
grid_test <- grid |>
  dplyr::filter(grid_id %in% test_gridcells)
grid_val <- grid |>
  dplyr::filter(grid_id %in% val_gridcells)

# Check that every row is included
nrow(grid_train) + nrow(grid_test) + nrow(grid_val) == nrow(grid)
any(grid_train$grid_id %in% grid_test$grid_id)
any(grid_train$grid_id %in% grid_val$grid_id )
any(grid_test$grid_id %in% grid_val$grid_id)

# Save
save(grid_train, file = 'data/processed/training/grid_train.RData')
save(grid_test, file = 'data/processed/testing/grid_test.RData')
save(grid_val, file = 'data/processed/validation/grid_val.RData')