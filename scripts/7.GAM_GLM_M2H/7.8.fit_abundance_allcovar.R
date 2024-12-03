#### STEP 7-8

## Multivariate GLM fit to MODERN
## RELATIVE ABUNDANCE and CLIMATE and SOIL covariates

## Here, we use GJAM (generalized joint attribute modeling)
## to fit linear relationships between environmental covariates
## and taxon relative abundances, while accounting for the
## covariance between relative abundances
## This is effectively a GLM SDM, but allowing for the multivariate
## response variable

## NOTE that the GJAM is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to your file structure

## 1. Load data
## 2. Fit GJAM
## 3. Variable importance
## 4. Coefficient estimates

## Input: data/processed/FIA/xydata_in.RData
## Dataframe of in-sample grid cells with modern (FIA) era
## vegetation, soil, and climate data

## Output: /Volumes/FileBackup/SDM_bigdata/out/gjam/M/abundance/allcovar.RData
## Fitted GJAM object saved to external hard drive
## Used in 7.12.abundance_historical_predictions.R,
## 7.13.abundance_modern_predictions.R

## Coefficient estimates figure also saved to figures/ directory

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/FIA/xydata_in.RData')

# Select response variable columns
ydata <- fia_in |>
  dplyr::ungroup() |>
  dplyr::select(Ash, Basswood, Beech,
                Birch, Cherry, Dogwood,
                Elm, Fir, Hemlock,
                Hickory, Ironwood, Maple,
                Oak, Pine, Spruce,
                Tamarack, Walnut,
                `Other hardwood`,
                `Black gum/sweet gum`,
                `Cedar/juniper`,
                `Poplar/tulip poplar`) |>
  # Rename columns so there are no special characters
  dplyr::rename(oh = `Other hardwood`,
                gum = `Black gum/sweet gum`,
                cedar = `Cedar/juniper`,
                poplar = `Poplar/tulip poplar`)

# Select covariate columns
xdata <- fia_in |>
  dplyr::ungroup() |>
  dplyr::select(clay, sand, silt, caco3, awc, flood, # edaphic variables
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  # Rename columns so there are no special characters
  dplyr::rename(pptsum = ppt_sum,
                tmean = tmean_mean,
                pptcv = ppt_cv,
                tmeansd = tmean_sd)

#### 2. Fit GJAM ####

## Fit model with all covariates, even though they are correlated
## Precedent for this in Charney et al. 2021

# Define variables for GJAM
niter <- 10000 # Number of iterations
nburn <- 2000 # Number of burn-in iterations
typeNames <- 'FC' # using proportional data in the response

# Model list
ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)

# Run model
abund_gjam_M_allcovar <- gjam::gjam(formula = ~ clay + sand + silt + caco3 + awc + flood +
                                      pptsum + tmean + pptcv +
                                      tmeansd + tmin + tmax + vpdmax, # formula
                                    xdata = xdata, ydata = ydata, # data
                                    modelList = ml) # model parameters 

# Simple plots
gjam::gjamPlot(abund_gjam_M_allcovar)

# Save
save(abund_gjam_M_allcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gjam/M/abundance/allcovar.RData')

#### 3. Variable sensitivity ####

## This shows the joint sensitivity of all
## taxon relative abundances to each environmental
## covariate

## This gives me an idea of what covariates could
## be used in a reduced model

# Extract sensitivity samples
sens <- abund_gjam_M_allcovar$chains$fSensGibbs

# Remove burnin
sens <- sens[-c(1:2000),]

# Make dataframe
sens <- as.data.frame(sens)

# Format longer
sens |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'covariate',
                      values_to = 'sensitivity') |>
  # Change covariate names
  dplyr::mutate(covariate = dplyr::if_else(covariate == 'clay', 'Soil % clay', covariate),
                covariate = dplyr::if_else(covariate == 'sand', 'Soil % sand', covariate),
                covariate = dplyr::if_else(covariate == 'silt', 'Soil % silt', covariate),
                covariate = dplyr::if_else(covariate == 'caco3', 'Soil calcium carbonate\nconcentration (%)', covariate),
                covariate = dplyr::if_else(covariate == 'awc', 'Soil available water\ncontent (cm/cm)', covariate),
                covariate = dplyr::if_else(covariate == 'flood', 'Fraction of grid cell\nin floodplain', covariate),
                covariate = dplyr::if_else(covariate == 'pptsum', 'Total annual precipitation (mm/year)', covariate),
                covariate = dplyr::if_else(covariate == 'tmean', 'Mean annual temperature (°C)', covariate),
                covariate = dplyr::if_else(covariate == 'pptcv', 'Precipitation seasonality (CV)', covariate),
                covariate = dplyr::if_else(covariate == 'tmeansd', 'Temperature seasonality (SD)', covariate),
                covariate = dplyr::if_else(covariate == 'tmin', 'Minimum annual temperature (°C)', covariate),
                covariate = dplyr::if_else(covariate == 'tmax', 'Maximum annual temperature (°C)', covariate),
                covariate = dplyr::if_else(covariate == 'vpdmax', 'Maximum annual vapor pressure\ndeficit (hPa)', covariate)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = reorder(covariate, sensitivity), y = sensitivity)) +
  ggplot2::coord_flip() +
  ggplot2::ylab('Sensitivity') + ggplot2::xlab('') +
  ggplot2::theme_minimal()

## This figure indicates that the temperature variables are
## the most important, followed by VPD and temperature seasonality
## The most important soil variable is soil % clay

# Look at the correlation between environmental variables
# Removing ones that were not important according to above
# sensitivity figure
corrs <- cor(dplyr::select(xdata, -flood, -caco3, -clay,
                           -pptsum, -vpdmax, -awc,
                           -sand))

corrplot::corrplot(corr = corrs, method = 'circle',
                   type = 'lower', diag = FALSE)

# How about tmin, pptcv, silt
corrs2 <- cor(dplyr::select(xdata, tmin, silt, pptcv))
corrs2
# These are < 0.7 so can be used

#### 4. Coefficient estimates ####

## Note that the coefficient estimates are likely to be unreliable
## due to strong covariance among predictor variables
## The purpose is just to document the trends the model is quantifying
## that propagate into the predictions, as with the RFs and GAMs

# Extract standardized coefficient samples
bFacGibbs <- abund_gjam_M_allcovar$chains$bFacGibbs

# Remove burnin
bFacGibbs <- bFacGibbs[-c(1:2000),]

# Convert to dataframe
bFacGibbs <- as.data.frame(bFacGibbs)

# Format
bFacGibbs_long <- bFacGibbs |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'taxon_covariate',
                      values_to = 'estimate') |>
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = taxon_covariate),
                covariate = sub(pattern = '.*_', replacement = '', x = taxon_covariate))

# Plot
bFacGibbs_long |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'cedar', 'Cedar/juniper', taxon),
                taxon = dplyr::if_else(taxon == 'gum', 'Black gum/sweet gum', taxon),
                taxon = dplyr::if_else(taxon == 'oh', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'poplar', 'Poplar/tulip poplar', taxon),
                covariate = dplyr::if_else(covariate == 'clay', 'Soil % clay', covariate),
                covariate = dplyr::if_else(covariate == 'sand', 'Soil % sand', covariate),
                covariate = dplyr::if_else(covariate == 'silt', 'Soil % silt', covariate),
                covariate = dplyr::if_else(covariate == 'caco3', 'Soil calcium carbonate\nconcentration (%)', covariate),
                covariate = dplyr::if_else(covariate == 'awc', 'Soil available water content (cm/cm)', covariate),
                covariate = dplyr::if_else(covariate == 'flood', 'Fraction of grid cell in a floodplain', covariate),
                covariate = dplyr::if_else(covariate == 'pptsum', 'Total annual precipitation (mm/year)', covariate),
                covariate = dplyr::if_else(covariate == 'tmean', 'Mean annual temperature (°C)', covariate),
                covariate = dplyr::if_else(covariate == 'pptcv', 'Precipitation seasonality (CV)', covariate),
                covariate = dplyr::if_else(covariate == 'tmeansd', 'Temperature seasonality (SD)', covariate),
                covariate = dplyr::if_else(covariate == 'tmin', 'Minimum annual temperature (°C)', covariate),
                covariate = dplyr::if_else(covariate == 'tmax', 'Maximum annual temperature (°C)', covariate),
                covariate = dplyr::if_else(covariate == 'vpdmax', 'Maximum annual vapor\npressure deficit (hPa)', covariate)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = taxon, y = estimate, 
                                    color = taxon, fill = taxon)) +
  ggplot2::facet_wrap(~covariate) +
  ggplot2::scale_color_discrete(name = 'Taxon') +
  ggplot2::scale_fill_discrete(name = 'Taxon') +
  ggplot2::xlab('') + ggplot2::ylab('Standardized coefficient estimate') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gjam/M/abundance/fit/all_covar_covariates.png',
                height = 20, width = 30, units = 'cm')
