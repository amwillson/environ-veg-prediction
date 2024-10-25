## Test random forest

rm(list = ls())

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select relevant columns
rf_data <- pls_in |>
  dplyr::select(total_density, # response variable
                clay, sand, silt, caco3, awc, flood, # edaphic variables
                ppt_mean, tmean_mean, ppt_sd, ppt_cv, 
                tmean_sd, tmean_cv, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::distinct()

# Random forest
rf_out <- randomForest::randomForest(formula = total_density ~ .,
                                     data = rf_data)
imp <- rf_out$importance
imp <- as.data.frame(imp)
imp |>
  tibble::rownames_to_column(var = 'covar') |>
  dplyr::arrange(dplyr::desc(IncNodePurity))

# Random forest using multivariate random forest package
rf_out2 <- MultivariateRandomForest::build_forest_predict()