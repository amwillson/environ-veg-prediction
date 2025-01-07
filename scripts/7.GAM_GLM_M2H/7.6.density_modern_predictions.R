#### STEP 7-6

## Predict MODERN TOTAL STEM DENSITY
## from MODERN fitted GAMs

## NOTE that models were saved on an external hard drive in
## steps 7-1 through 7-4. They must be loaded from the external
## hard drive or wherever you saved the models here. Change
## the file paths accordingly

## 1. Load models
## 2. Load data
## 3. Predict

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData
## Fitted GAM using all climate and soil covariates
## Used to make predictions from the main model with climate and soil
## covariates
## From 7.1.fit_density_allcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData
## Fitted GAM using only climate covariates
## Used to make predictions from the alternate model with only climate
## covariates
## From 7.2.fit_density_climcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar.RData
## Fitted GAM using only the reduced set of covariates
## Used to make predictions from the alternate model with a reduced set
## of covariates
## From 7.3.fit_density_redcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar.RData
## Fitted GAM using all the soil and climate covariates plus
## the latitude and longtiude of the grid cell
## Used to make predictions from the alternate model including grid
## cell coordinates and environmental covariates
## From 7.4.fit_density_xycovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar_4k.RData
## Fitted GAM using all climate and soil covariates fit with lower maximum
## basis dimensionality
## Used to make predictions from the main model with climate and soil
## covariates but reducing overfitting
## From 7.1.fit_density_allcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar_4k.RData
## Fitted GAM using only climate covariates fit with lower maximum
## basis dimensionality
## Used to make predictions from the alternate model with only climate
## covariates but reducing overfitting
## From 7.2.fit_density_climcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar_4k.RData
## Fitted GAM using only the reduced set of covariates fit with lower
## maximum basis dimensionality
## Used to make predictions from the alternate model with a reduced set
## of covariates but reducing overfitting
## From 7.3.fit_density_redcovar.R

## Input: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar_4k.RData
## Fitted GAM using all the soil and climate covariates plus
## the latitude and longitude of the grid cell with lower maximum
## basis dimensionality
## Used to make predictions from the alternate model including grid
## cell coordinates and environmental covariates
## From 7.4.fit_density_xycovar.R

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

## Output: out/gam/M/density/predicted_modern_gam1.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample 
## grid cells using the main model with climate and soil covariates
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam2.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample 
## grid cells using the alternate model with only climate covariates
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam3.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample 
## grid cells using the alternate model with only the reduced set of covariates
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam4.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with the soil and climate
## covariates as well as the latitude and the longitude of the grid cell
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam1_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the main model with climate and soil covariates
## with lower maximum basis dimensionality
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam2_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with only climate covariates
## with lower maximum basis dimensionality
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam3_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with only the reduced set of covariates
## with lower maximum basis dimensionality
## Used in 7.7.density_figures.R

## Output: out/gam/M/density/predicted_modern_gam4_4k.RData
## Dataframe of observed vegetation, soil, and climate data and
## predicted modern total stem density from the out-of-sample
## grid cells using the alternate model with the soil and climate
## covariates as well as the latitude and the longitude of the grid cell
## Used in 7.7.density_figures.R

rm(list = ls())

#### 1. Load models ####

# Load model with all covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData')
# Load model with climate covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData')
# Load model with reduced covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar.RData')
# Load model with all covariates plus coordinates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar.RData')
# Load model with all covariates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar_4k.RData')
# Load model with climate covariates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar_4k.RData')
# Load model with reduced covariates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/redcovar_4k.RData')
# Load model with all covariates plus coordinates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/xycovar_4k.RData')

#### 2. Load data ####

# Load out-of-sample data
load('data/processed/FIA/xydata_out.RData')

# Convert to regular dataframe
fia_oos <- as.data.frame(fia_oos)

# Load PLS out-of-sample data
load('data/processed/PLS/xydata_out.RData')

# Re-format FIA OOS
fia_oos <- fia_oos |>
  dplyr::rename(total_density = total_stem_density) |>
  dplyr::select(colnames(pls_oos)) |>
  dplyr::rename(x_coord = x,
                y_coord = y)

#### 3. Predict ####

# Make predictions with model 1
pred_modern_gam1 <- mvgam::predictions(model = density_gam_M_allcovar,
                                       newdata = fia_oos)

# Extract predictions
# This is done because this dataframe is really lareg
# because of attributes not in the columns themselves?
pred <- dplyr::select(pred_modern_gam1, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam1',
                    'conf.low_gam1',
                    'conf.high_gam1')

# Add to original dataframe
pred_modern_gam1 <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam1,
     file = 'out/gam/M/density/predicted_modern_gam1.RData')

# Make predictions with model 2
pred_modern_gam2 <- mvgam::predictions(model = density_gam_M_climcovar,
                                       newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam2, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam2',
                    'conf.low_gam2',
                    'conf.high_gam2')

# Add to original dataframe
pred_modern_gam2 <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam2,
     file = 'out/gam/M/density/predicted_modern_gam2.RData')

# Make predictions with model 3
pred_modern_gam3 <- mvgam::predictions(model = density_gam_M_redcovar,
                                       newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam3, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam3',
                    'conf.low_gam3',
                    'conf.high_gam3')

# Add to original dataframe
pred_modern_gam3 <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam3,
     file = 'out/gam/M/density/predicted_modern_gam3.RData')

# Make predictions with model 4
pred_modern_gam4 <- mvgam::predictions(model = density_gam_M_xycovar,
                                       newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam4, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam4',
                    'conf.low_gam4',
                    'conf.high_gam4')

# Add to original dataframe
pred_modern_gam4 <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam4,
     file = 'out/gam/M/density/predicted_modern_gam4.RData')

# Make predictions with model 1, lower basis dimension
pred_modern_gam1_4k <- mvgam::predictions(model = density_gam_M_allcovar_4k,
                                          newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam1_4k, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam1_4k',
                    'conf.low_gam1_4k',
                    'conf.high_gam1_4k')

# Add to original dataframe
pred_modern_gam1_4k <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam1_4k,
     file = 'out/gam/M/density/predicted_modern_gam1_4k.RData')

# Make predictions with model 2, lower basis dimension
pred_modern_gam2_4k <- mvgam::predictions(model = density_gam_M_climcovar_4k,
                                          newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam2_4k, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam2_4k',
                    'conf.low_gam2_4k',
                    'conf.high_gam2_4k')

# Add to original dataframe
pred_modern_gam2_4k <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam2_4k,
     file = 'out/gam/M/density/predicted_modern_gam2_4k.RData')

# Make predictions with model 3, lower basis dimension
pred_modern_gam3_4k <- mvgam::predictions(model = density_gam_M_redcovar_4k,
                                          newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam3_4k, estimate:conf.high)

# Rename columns to be spcific to the model
colnames(pred) <- c('estimate_gam3_4k',
                    'conf.low_gam3_4k',
                    'conf.high_gam3_4k')

# Add to original dataframe
pred_modern_gam3_4k <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam3_4k,
     file = 'out/gam/M/density/predicted_modern_gam3_4k.RData')

# Make predictions with model 4, lower basis dimension
pred_modern_gam4_4k <- mvgam::predictions(model = density_gam_M_xycovar_4k,
                                          newdata = fia_oos)

# Extract predictions
pred <- dplyr::select(pred_modern_gam4_4k, estimate:conf.high)

# Rename columns to be specific to the model
colnames(pred) <- c('estimate_gam4_4k',
                    'conf.low_gam4_4k',
                    'conf.high_gam4_4k')

# Add to original dataframe
pred_modern_gam4_4k <- cbind(pred, fia_oos)

# Save
save(pred_modern_gam4_4k,
     file = 'out/gam/M/density/predicted_modern_gam4_4k.RData')
