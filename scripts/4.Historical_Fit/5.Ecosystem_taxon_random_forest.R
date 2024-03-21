rm(list = ls())

# Load training dataset
load('data/processed/training/ecosystem_point_train.RData')

# Remove columns we don't want in random forest
ecosystem_point_train <- ecosystem_point_train |>
  dplyr::select(-x, -y, -grid_id, -ecosystem_id, -state, -one, -two, -three, -four) |>
  dplyr::mutate(ecosystem = as.factor(ecosystem),
                Floodplain = as.factor(Floodplain))

# Create model with default paramters
control <- caret::trainControl(method = "repeatedcv", 
                               number = 10, 
                               repeats = 3)
metric <- "Accuracy"
mtry <- sqrt(ncol(ecosystem_point_train)-1)
tunegrid <- expand.grid(.mtry=mtry)

rf_default <- caret::train(ecosystem ~ ., 
                           data = ecosystem_point_train, 
                           method = "rf", 
                           metric = metric, 
                           tuneGrid = tunegrid, 
                           trControl = control,
                           do.trace = TRUE)

print(rf_default)

saveRDS(rf_default, file = 'out/ecosystem_point_rf_default.RDS')