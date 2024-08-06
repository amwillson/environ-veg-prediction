## Random forest of point-level historical data

rm(list = ls())

# Load training dataset
load('data/processed/training/point_train.RData')

# Remove columns we don't want in random forest
point_train <- point_train |>
  dplyr::select(-x, -y, -grid_id, -ecosystem_id, -state, -tree) |>
  dplyr::mutate(species = as.factor(species),
                Floodplain = as.factor(Floodplain))

# Create model with default paramters
control <- caret::trainControl(method = "repeatedcv", 
                               number = 10, 
                               repeats = 3)
metric <- "Accuracy"
mtry <- sqrt(ncol(point_train)-1)
tunegrid <- expand.grid(.mtry=mtry)

rf_default <- caret::train(species ~ ., 
                           data = point_train, 
                           method = "rf", 
                           metric = metric, 
                           tuneGrid = tunegrid, 
                           trControl = control,
                           do.trace = TRUE,
                           importance = TRUE)

print(rf_default)

saveRDS(rf_default, file = 'out/taxon_point_rf_default.RDS')

# Random Search
metric <- "Accuracy"

control <- caret::trainControl(method = "repeatedcv", 
                               number = 10, 
                               repeats = 3, 
                               search = "random")
rf_random <- caret::train(species ~ ., 
                          data = point_train, 
                          method = "rf", 
                          metric = metric, 
                          tuneLength = 15, 
                          trControl = control)
print(rf_random)
plot(rf_random)