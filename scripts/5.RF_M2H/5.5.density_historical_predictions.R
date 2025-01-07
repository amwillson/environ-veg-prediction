#### STEP 5-5

## Predict HISTORICAL TOTAL STEM DENSITY
## from MODERN fitted random forests

## NOTE that models were saved on an external hard drive in
## steps 5-1 through 5-4. They must be loaded from the external
## hard drive or wherever you saved the models here. Change
## the file paths accordingly

## 1. Load models
## 2. Load data
## 3. Predict

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/allcovar.RData
## Fitted random forest using all climate and soil covariates
## Used to make predictions from the main model with climate and soil
## covariates
## From 5.1.fit_density_allcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/climcovar.RData
## Fitted random forest using only climate covariates
## Used to make predictions from the alternate model with only climate
## covariates
## From 5.2.fit_density_climcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/redcovar.RData
## Fitted random forest using only the reduced set of covariates
## Used to make predictions from the alternate model with a reduced set
## of covariates
## From 5.3.fit_density_redcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/M/density/xycovar.RData
## Fitted random forest using all the soil and climate covariates plus
## the latitude and longtiude of the grid cell
## Used to make predictions from the alternate model including grid
## cell coordinates and environmental covariates
## From 5.4.fit_density_xycovar.R

## Input: data/processed/PLS/xydata_out.RData
## Dataframe containing the out-of-sample historical (PLS)
## vegetation, soil, and climate data
## The covariates are used to make predictions of the out-of-sample
## historical vegetation data
## From 1.3.Split_data.R

## Output: out/rf/M/density/predicted_historical_rf1.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the main model with climate and soil covariates
## Used in 5.7.density_figures.R

## Output: out/rf/M/density/predicted_historical_rf2.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the alternate model with only climate covariates
## Used in 5.7.density_figures.R

## Output: out/rf/M/density/predicted_historical_rf3.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the alternate model with only the reduced set of covariates
## Used in 5.7.density_figures.R

## Output: out/rf/M/density/predicted_historical_rf4.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical total stem density from the out-of-sample 
## grid cells using the alternate model with the soil and climate 
## covariates as well as the latitude and the longitude of the grid cell
## Used in 5.7.density_figures.R

rm(list = ls())

#### 1. Load models ####

# Load model with all covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/M/density/allcovar.RData')
# Load model with climate covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/M/density/climcovar.RData')
# Load model with four covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/M/density/redcovar.RData')
# Load model with all covariates plus coordinates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/M/density/xycovar.RData')

#### 2. Load data ####

# Load out-of-sample data
load('data/processed/PLS/xydata_out.RData')

#### 3. Predict ####

# Make prediction with model 1
pred_rf1 <- randomForestSRC::predict.rfsrc(object = density_rf_M_allcovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf1 <- pred_rf1$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf1 <- cbind(pls_oos, pred_out_rf1)

# Remove vector
rm(pred_out_rf1)

# Save
save(pred_historical_rf1,
     file = 'out/rf/M/density/predicted_historical_rf1.RData')

# Make prediction with model 2
pred_rf2 <- randomForestSRC::predict.rfsrc(object = density_rf_M_climcovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf2 <- pred_rf2$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf2 <- cbind(pls_oos, pred_out_rf2)

# Remove vector
rm(pred_out_rf2)

# Save
save(pred_historical_rf2,
     file = 'out/rf/M/density/predicted_historical_rf2.RData')

# Make predictions with model 3
pred_rf3 <- randomForestSRC::predict.rfsrc(object = density_rf_M_redcovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf3 <- pred_rf3$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf3 <- cbind(pls_oos, pred_out_rf3)

# Remove vector
rm(pred_out_rf3)

# Save
save(pred_historical_rf3,
     file = 'out/rf/M/density/predicted_historical_rf3.RData')

# Make predictions with model 4
pred_rf4 <- randomForestSRC::predict.rfsrc(object = density_rf_M_xycovar,
                                           newdata = pls_oos)

# Extract predictions
pred_out_rf4 <- pred_rf4$predicted

# Bind predictions and out-of-sample data for analysis
pred_historical_rf4 <- cbind(pls_oos, pred_out_rf4)

# Remove vector
rm(pred_out_rf4)

# Save
save(pred_historical_rf4,
     file = 'out/rf/M/density/predicted_historical_rf4.RData')
