#### STEP 6-12

## Predict HISTORICAL RELATIVE ABUNDANCE
## from HISTORICAL fitted GLMs

## NOTE that models were saved on an external hard drive in
## steps 6-8 through 6-11. They must be loaded from the external
## hard drive or wherever you saved the models here. Change
## the file paths accordingly

## 1. Load models
## 2. Load data
## 3. Predict
## 4. Check sum-to-one

## Input: /Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/allcovar.RData
## Fitted GJAM using all climate and soil covariates
## Used to make predictions from the main model with climate and soil
## covariates
## From 6.8.fit_abundance_allcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/climcovar.RData
## Fitted GJAM using only climate covariates
## Used to make predictions from the alternate model with only climate
## covariates
## From 6.9.fit_abundance_climcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/redcovar.RData
## Fitted GJAM using only the reduced set of covariates
## Used to make predictions from the alternate model with a reduced set
## of covariates
## From 6.10.fit_abundance_redcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/xycovar.RData
## Fitted GJAM using all the soil and climate covariates plus
## the latitude and longtiude of the grid cell
## Used to make predictions from the alternate model including grid
## cell coordinates and environmental covariates
## From 6.11.fit_abundance_xycovar.R

## Input: data/processed/PLS/xydata_out.RData
## Dataframe containing the out-of-sample historical (PLS)
## vegetation, soil, and climate data
## The covariates are used to make predictions of the out-of-sample
## historical vegetation data
## From 1.3.Split_data.R

## Output: out/gjam/H/abundance/predicted_historical_gjam1.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical relative abundances from the out-of-sample 
## grid cells using the main model with climate and soil covariates
## Used in 6.14.abundance_figures.R

## Output: out/gjam/H/abundance/predicted_historical_gjam2.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical relative abundances from the out-of-sample 
## grid cells using the alternate model with only climate covariates
## Used in 6.14.abundance_figures.R

## Output: out/gjam/H/abundance/predicted_historical_gjam3.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical relative abundances from the out-of-sample 
## grid cells using the alternate model with only the reduced set of covariates
## Used in 6.14.abundance_figures.R

## Output: out/gjam/H/abundance/predicted_historical_gjam4.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted historical relative abundances from the out-of-sample 
## grid cells using the alternate model with the soil and climate 
## covariates as well as the latitude and the longitude of the grid cell
## Used in 6.14.abundance_figures.R

rm(list = ls())

#### 1. Load models ####

# Load model with all covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/allcovar.RData')
# Load model with climate covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/climcovar.RData')
# Load model with reduced covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/redcovar.RData')
# Load model with all covariates plus coordinates
load('/Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/xycovar.RData')

#### 2. Load data ####

# Load out-of-sample data
load('data/processed/PLS/xydata_out.RData')

# Rename columns
pls_oos <- pls_oos |>
  dplyr::rename(oh = `Other hardwood`,
                gum = `Black gum/sweet gum`,
                cedar = `Cedar/juniper`,
                poplar = `Poplar/tulip poplar`,
                pptsum = ppt_sum,
                tmean = tmean_mean,
                pptsd = ppt_sd,
                pptcv = ppt_cv,
                tmeansd = tmean_sd,
                tmeancv = tmean_cv)

# Extract xdata
new_xdata <- pls_oos |>
  dplyr::select(x, y,
                clay:vpdmax)

#### 3. Predict ####

## Note: I debugged this for hours. You have to include
## more columns than you need in your xdata here
## or else there is an error
## The correct columns ARE chosen. You can check for yourself
## in the gjam source code: line 54 of the .getStandX function

# Make data list for prediction
new_datalist <- list(xdata = new_xdata,
                     nsim = 10000)

# Make prediction with model 1
pred_gjam1 <- gjam::gjamPredict(output = abund_gjam_H_allcovar,
                                newdata = new_datalist)

# Extract predictions
pred_out_gjam1 <- pred_gjam1$sdList$yMu

# Add pred to column names
colnames(pred_out_gjam1) <- paste(colnames(pred_out_gjam1), 'pred1', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_historical_gjam1 <- cbind(pls_oos, pred_out_gjam1)

# Remove vector
rm(pred_out_gjam1)

# Save
save(pred_historical_gjam1,
     file = 'out/gjam/H/abundance/predicted_historical_gjam1.RData')

# Make prediction with model 2
pred_gjam2 <- gjam::gjamPredict(output = abund_gjam_H_climcovar,
                                newdata = new_datalist)

# Extract predictions
pred_out_gjam2 <- pred_gjam2$sdList$yMu

# Add pred to column names
colnames(pred_out_gjam2) <- paste(colnames(pred_out_gjam2), 'pred2', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_historical_gjam2 <- cbind(pls_oos, pred_out_gjam2)

# Remove vector
rm(pred_out_gjam2)

# Save
save(pred_historical_gjam2,
     file = 'out/gjam/H/abundance/predicted_historical_gjam2.RData')

# Make prediction with model 3
pred_gjam3 <- gjam::gjamPredict(output = abund_gjam_H_redcovar,
                                newdata = new_datalist)

# Extract predictions
pred_out_gjam3 <- pred_gjam3$sdList$yMu

# Add pred to column names
colnames(pred_out_gjam3) <- paste(colnames(pred_out_gjam3), 'pred3', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_historical_gjam3 <- cbind(pls_oos, pred_out_gjam3)

# Remove vector
rm(pred_out_gjam3)

# Save
save(pred_historical_gjam3,
     file = 'out/gjam/H/abundance/predicted_historical_gjam3.RData')

# Make prediction with model 4
pred_gjam4 <- gjam::gjamPredict(output = abund_gjam_H_xycovar,
                                newdata = new_datalist)

# Extract predictions
pred_out_gjam4 <- pred_gjam4$sdList$yMu

# Add pred to column names
colnames(pred_out_gjam4) <- paste(colnames(pred_out_gjam4), 'pred4', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_historical_gjam4 <- cbind(pls_oos, pred_out_gjam4)

# Remove vector
rm(pred_out_gjam4)

# Save
save(pred_historical_gjam4,
     file = 'out/gjam/H/abundance/predicted_historical_gjam4.RData')

#### 4. Check sum-to-one ####

## Make sum column for each model
pred_historical_gjam1 <- pred_historical_gjam1 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred1:poplar_pred1)))
range(pred_historical_gjam1$total) # should be very close to 1

pred_historical_gjam2 <- pred_historical_gjam2 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred2:poplar_pred2)))
range(pred_historical_gjam2$total) # should be very close to 1

pred_historical_gjam3 <- pred_historical_gjam3 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred3:poplar_pred3)))
range(pred_historical_gjam3$total) # should be very close to 1

pred_historical_gjam4 <- pred_historical_gjam4 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred4:poplar_pred4)))
range(pred_historical_gjam4$total) # should be very close to 1
