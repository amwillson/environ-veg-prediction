#### STEP 6-10

## Multivariate GLM fit to HISTORICAL
## RELATIVE ABUNDANCE and REDUCED covariates

## Here, we use GJAM (generalized joint attribute modeling)
## to fit linear relationships between environmental covariates
## and taxon relative abundances, while accounting for the
## covariance between relative abundances
## This is effectively a GLM SDM, but allowing for the multivariate
## response variable

## Reduced covariates refers to using a subset of all the covariates
## based on joint sensitivity of response variables to each covariate analyzed in step 6-8:
## maximum annual temperature,
## temperature seasonality,
## soil % clay

## NOTE that the GJAM is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to you file structure

## 1. Load data
## 2. Fit GJAM
## 3. Coefficient estimates

## Input: data/processed/PLS/xydata_in.RData
## Dataframe of in-sample grid cells with historical (PLS) era
## vegetation, soil, and climate data

## Output: /Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/redcovar.RData
## Fitted GJAM object saved to external hard drive
## Used in 6.12.abundance_historical_predictions.R,
## 6.13.abundance_modern_predictions.R

## Figure of coefficient estimates also saved to figures/ directory
## Note that these coefficient estimates are likely more accurate
## than in the other models, which have trade-offs between correlated
## covariates

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select response variable columns
ydata <- pls_in |>
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
xdata <- pls_in |>
  dplyr::select(clay, # edaphic variable
                tmax, tmean_sd) |> # climatic variables
  # Rename columns so there are no special characters
  dplyr::rename(tmeansd = tmean_sd)

#### 2. Fit GJAM ####

## Fit model with reduced covariates

# Define variables for GJAM
niter <- 10000 # Number of iterations
nburn <- 2000 # Number of burn-in iterations
typeNames <- 'FC' # using proportional data in the response

# Model list
ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)

# Run model
abund_gjam_H_redcovar <- gjam::gjam(formula = ~ clay + 
                                      tmax + tmeansd, # formula
                                    xdata = xdata, ydata = ydata, # data
                                    modelList = ml) # model parameters

# Simple plots
gjam::gjamPlot(abund_gjam_H_redcovar)

# Save
save(abund_gjam_H_redcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gjam/H/abundance/redcovar.RData')

#### 3. Coefficient estimates ####

## These coefficient estimates are more likely to represent
## real environment-vegetation relationships than in 
## steps 6-8 and 6-9 because of the limited correlation between
## environmental covariates in this model

# Extract standardized coefficient samples
bFacGibbs <- abund_gjam_H_redcovar$chains$bFacGibbs

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
                covariate = dplyr::if_else(covariate == 'tmeansd', 'Temperature seasonality (SD)', covariate),
                covariate = dplyr::if_else(covariate == 'tmax', 'Maximum annual temperature (°C)', covariate)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = taxon, y = estimate, 
                                    color = taxon, fill = taxon)) +
  ggplot2::facet_wrap(~covariate, nrow = 2) +
  ggplot2::scale_color_discrete(name = 'Taxon') +
  ggplot2::scale_fill_discrete(name = 'Taxon') +
  ggplot2::xlab('') + ggplot2::ylab('Standardized coefficient estimate') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gjam/H/abundance/fit/red_covar_covariates.png',
                height = 20, width = 25, units = 'cm')
