#### STEP 4-13

## Predict MODERN RELATIVE ABUNDANCE
## from HISTORICAL fitted random forests

## NOTE that models were saved on an external hard drive in
## steps 4-8 through 4-11. They must be loaded from the external
## hard drive or wherever you saved the models here. Change
## the file paths accordingly

## 1. Load models
## 2. Load data
## 3. Predict
## 4. Check sum-to-one

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData
## Fitted random forest using all climate and soil covariates
## Used to make predictions from the main model with climate and soil
## covariates
## From 4.8.fit_abundance_allcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/climcovar.RData
## Fitted random forest using only climate covariates
## Used to make predictions from the alternate model with only climate
## covariates
## From 4.9.fit_abundance_climcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/redcovar.RData
## Fitted random forest using only the reduced set of covariates
## Used to make predictions from the alternate model with a reduced set
## of covariates
## From 4.10.fit_abundance_redcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/xycovar.RData
## Fitted random forest using all the soil and climate covariates plus
## the latitude and longtiude of the grid cell
## Used to make predictions from the alternate model including grid
## cell coordinates and environmental covariates
## From 4.11.fit_abundance_xycovar.R

## Input: data/processed/FIA/xydata_out.RData
## Dataframe containing the out-of-sample modern (FIA)
## vegetation, soil, and climate data
## The covariates are used to make predictions of the out-of-sample
## modern vegetation data
## From 2.4.Split_data.R

## Input: data/processed/PLS/xydata_out.RData
## Dataframe containing the out-of-sample historical (PLS)
## vegetation, soil, and climate data
## Used to make sure the FIA columns are in the same order
## I'm pretty sure this isn't necessary though
## From 1.3.Split_data.R

## Output: out/rf/H/abundance/predicted_modern_rf1.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern relative abundances from the out-of-sample 
## grid cells using the main model with climate and soil covariates
## Used in 4.14.abundance_figures.R

## Output: out/rf/H/abundance/predicted_modern_rf2.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern relative abundances from the out-of-sample 
## grid cells using the alternate model with only climate covariates
## Used in 4.14.abundance_figures.R

## Output: out/rf/H/abundance/predicted_modern_rf3.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern relative abundances from the out-of-sample 
## grid cells using the alternate model with only the reduced set of covariates
## Used in 4.14.abundance_figures.R

## Output: out/rf/H/abundance/predicted_modern_rf4.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern relative abundances from the out-of-sample 
## grid cells using the alternate model with the soil and climate 
## covariates as well as the latitude and the longitude of the grid cell
## Used in 4.14.abundance_figures.R

rm(list = ls())

#### 1. Load models ####

# Load model with all covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/allcovar.RData')
# Load model with climate covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/climcovar.RData')
# Load model with four covariates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/redcovar.RData')
# Load model with all covariates plus coordinates
load('/Volumes/FileBackup/SDM_bigdata/out/rf/H/abundance/xycovar.RData')

#### 2. Load data ####

# Load out-of-sample data
load('data/processed/FIA/xydata_out.RData')

# Convert to regular dataframe
fia_oos <- as.data.frame(fia_oos)

# Load PLS out-of-sample data
load('data/processed/PLS/xydata_out.RData')

# Format columns
pls_oos <- pls_oos |>
  dplyr::rename(oh = `Other hardwood`, # rename so no special characters
                gum = `Black gum/sweet gum`,
                cedar = `Cedar/juniper`,
                poplar = `Poplar/tulip poplar`)

# Re-format FIA OOS
fia_oos <- fia_oos |>
  dplyr::rename(oh = `Other hardwood`, # rename so no special charaters
                gum = `Black gum/sweet gum`,
                cedar = `Cedar/juniper`,
                poplar = `Poplar/tulip poplar`,
                total_density = total_stem_density) |>
  dplyr::select(colnames(pls_oos))

#### 3. Predict ####

# Make predictions with model 1
pred_rf1 <- randomForestSRC::predict.rfsrc(object = abund_rf_H_allcovar,
                                           newdata = fia_oos)

# Extract predictions
pred_out_rf1 <- randomForestSRC::get.mv.predicted(pred_rf1)

# Add pred to column names
colnames(pred_out_rf1) <- paste(colnames(pred_out_rf1), 'pred1', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_modern_rf1 <- cbind(fia_oos, pred_out_rf1)

# Remove vector
rm(pred_out_rf1)

# Save
save(pred_modern_rf1,
     file = 'out/rf/H/abundance/predicted_modern_rf1.RData')

# Make predictions with model 2
pred_rf2 <- randomForestSRC::predict.rfsrc(object = abund_rf_H_climcovar,
                                           newdata = fia_oos)

# Extract predictions
pred_out_rf2 <- randomForestSRC::get.mv.predicted(pred_rf2)

# Add pred to column names
colnames(pred_out_rf2) <- paste(colnames(pred_out_rf2), 'pred2', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_modern_rf2 <- cbind(fia_oos, pred_out_rf2)

# Remove vector
rm(pred_out_rf2)

# Save
save(pred_modern_rf2,
     file = 'out/rf/H/abundance/predicted_modern_rf2.RData')

# Make predictions with model 3
pred_rf3 <- randomForestSRC::predict.rfsrc(object = abund_rf_H_redcovar,
                                           newdata = fia_oos)

# Extract predictions
pred_out_rf3 <- randomForestSRC::get.mv.predicted(pred_rf3)

# Add pred to column names
colnames(pred_out_rf3) <- paste(colnames(pred_out_rf3), 'pred3', sep = '_')

# Bind predictions and out-of-sample data for analyis
pred_modern_rf3 <- cbind(fia_oos, pred_out_rf3)

# Remove vector
rm(pred_out_rf3)

# Save
save(pred_modern_rf3,
     file = 'out/rf/H/abundance/predicted_modern_rf3.RData')

# Make predictions with model 4
pred_rf4 <- randomForestSRC::predict.rfsrc(object = abund_rf_H_xycovar,
                                           newdata = fia_oos)

# Extract predictions
pred_out_rf4 <- randomForestSRC::get.mv.predicted(pred_rf4)

# Add pred to column names
colnames(pred_out_rf4) <- paste(colnames(pred_out_rf4), 'pred4', sep = '_')

# Bind predictions and out-of-sample data for analysis
pred_modern_rf4 <- cbind(fia_oos, pred_out_rf4)

# Remove vector
rm(pred_out_rf4)

# Save
save(pred_modern_rf4,
     file = 'out/rf/H/abundance/predicted_modern_rf4.RData')

#### 4. Check sum-to-one ####

## Make sum column for each model
pred_modern_rf1 <- pred_modern_rf1 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred1:poplar_pred1)))
unique(pred_modern_rf1$total) # should be all 1s. Unique values caused by very small deviations from 1

pred_modern_rf2 <- pred_modern_rf2 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred2:poplar_pred2)))
unique(pred_modern_rf2$total) # should be all 1s

pred_modern_rf3 <- pred_modern_rf3 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred3:poplar_pred3)))
unique(pred_modern_rf3$total) # should be all 1s

pred_modern_rf4 <- pred_modern_rf4 |>
  dplyr::mutate(total = rowSums(dplyr::across(Ash_pred4:poplar_pred4)))
unique(pred_modern_rf4$total) # should be all 1s
