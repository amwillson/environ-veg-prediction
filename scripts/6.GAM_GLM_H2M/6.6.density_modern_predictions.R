#### STEP 6-6

## Predict MODERN TOTAL STEM DENSITY
## from HISTORICAL fitted GAMs

## Note that we are not making predictions from the model with
## coordinates because it does not converge

## 1. Load models
## 2. Load data
## 3. Predict

rm(list = ls())

#### 1. Load models ####

# Load model with all covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/allcovar.RData')
# Load model with climate covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/climcovar.RData')
# Load model with reduced covariates
load('/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData')
# Load model with all covariates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/allcovar_4k.RData')
# Load model with climate covariates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/climcovar_4k.RData')
# Load model with reduced covariates with lower basis dimension
load('/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar_4k.RData')

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
  dplyr::select(colnames(pls_oos))

#### 3. Predict ####

# Make predictions with model 1
pred_modern_gam1 <- mvgam::predictions(model = density_gam_H_allcovar,
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
     file = 'out/gam/H/density/predicted_modern_gam1.RData')

# Make predictions with model 2
pred_modern_gam2 <- mvgam::predictions(model = density_gam_H_climcovar,
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
     file = 'out/gam/H/density/predicted_modern_gam2.RData')

# Make predictions with model 3
pred_modern_gam3 <- mvgam::predictions(model = density_gam_H_redcovar,
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
     file = 'out/gam/H/density/predicted_modern_gam3.RData')

# Make predictions with model 1, lower basis dimension
pred_modern_gam1_4k <- mvgam::predictions(model = density_gam_H_allcovar_4k,
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
     file = 'out/gam/H/density/predicted_modern_gam1_4k.RData')

# Make predictions with model 2, lower basis dimension
pred_modern_gam2_4k <- mvgam::predictions(model = density_gam_H_climcovar_4k,
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
     file = 'out/gam/H/density/predicted_modern_gam2_4k.RData')

# Make predictions with model 3, lower basis dimension
pred_modern_gam3_4k <- mvgam::predictions(model = density_gam_H_redcovar_4k,
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
     file = 'out/gam/H/density/predicted_modern_gam3_4k.RData')
