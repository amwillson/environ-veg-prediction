## STEP 4-5

## Predict HISTORICAL TOTAL STEM DENSITY
## from HISTORICAL fitted random forests

## 1. Load models
## 2. Load data
## 3. Predict

rm(list = ls())

#### 1. Load models ####

# Load model with all covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/density/allcovar.RData')
# Load model with climate covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/density/climcovar.RData')
# Load model with four covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/density/redcovar.RData')
# Load model with all covariates plus coordinates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/density/xycovar.RData')

#### 2. Load data ####

# Load out-of-sample data
load('data/processed/PLS/xydata_out.RData')

#### 3. Predict ####

# Make prediction with model 1
pred_rf1 <- randomForestSRC::predict.rfsrc(object = density_rf_H_allcovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf1 <- pred_rf1$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf1 <- cbind(pls_oos, pred_out_rf1)

# Remove vector
rm(pred_out_rf1)

# Save
save(pred_historical_rf1,
     file = 'out/rf/H/density/predicted_historical_rf1.RData')

# Make prediction with model 2
pred_rf2 <- randomForestSRC::predict.rfsrc(object = density_rf_H_climcovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf2 <- pred_rf2$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf2 <- cbind(pls_oos, pred_out_rf2)

# Remove vector
rm(pred_out_rf2)

# Save
save(pred_historical_rf2,
     file = 'out/rf/H/density/predicted_historical_rf2.RData')

# Make predictions with model 3
pred_rf3 <- randomForestSRC::predict.rfsrc(object = density_rf_H_redcovar,
                                          newdata = pls_oos)

# Extract predictions
pred_out_rf3 <- pred_rf3$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf3 <- cbind(pls_oos, pred_out_rf3)

# Remove vector
rm(pred_out_rf3)

# Save
save(pred_historical_rf3,
     file = 'out/rf/H/density/predicted_historical_rf3.RData')

# Make predictions with model 4
pred_rf4 <- randomForestSRC::predict.rfsrc(object = density_rf_H_xycovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf4 <- pred_rf4$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf4 <- cbind(pls_oos, pred_out_rf4)

# Remove vector
rm(pred_out_rf4)

# Save
save(pred_historical_rf4,
     file = 'out/rf/H/density/predicted_historical_rf4.RData')
