#### STEP 6-3

## Univariate GAM fit to HISTORICAL
## TOTAL STEM DENSITY and REDUCED covariates

## Reduced covariates refers to using a subset of all the covariates 
## that are relatively uncorrelated. All minimally correlated covariate
## combinations were tried and the best model according to leave-one-out
## cross validation was chosen and saved

## NOTE that the GAM is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to your file structure

## 1. Load data
## 2. Fit GAMs
## 3. Save final model
## 4. Fit GAMs -- lower basis dimension
## 5. Save final model with lower basis dimension
## 6. Partial effects plots

## Input: data/processed/PLS/xydata_in.RData
## Dataframe of in-sample grid cells with historical (PLS) era
## vegetation, soil, and climate data

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData
## Fitted GAM object saved to external hard drive
## Used in 6.5.density_historical_predictions.R,
## 6.6.density_modern_predictions.R

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar_4k.RData
## Fitted GAM object with lower maximum basis dimensionality
## to reduce overfitting. Saved to external hard drive
## Used in 6.5.density_historical_predictions.R,
## 6.6.density_modern_predictions.R

## Figures of partial effects plots also saved to figures/ directory

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select relevant columns
gam_data <- pls_in |>
  dplyr::select(total_density, # response variable
                clay, sand, silt, caco3, awc, flood, # edaphic variables
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::distinct()

#### 2. Fit GAMs ####

## Fitting multiple GAMs with default basis dimension
## Any GAM with relatively uncorrelated climate parameters will be tried
## Of course, soil texture variables are highly correlated
## Only clay will be used here because sand and silt are usually not
## significant based on 1.fit_density_allcovar.R

## Precedent for using the best model with minimally
## correlated variables: Charney et al. 2021

## Option 1: clay, caco3, awc, flood, ppt_sum, ppt_cv
option1 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(ppt_sum) +
                          s(ppt_cv),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS warning
                        family = mvgam::lognormal()) # Takes about 9 minutes

# Check summary
summary(option1)

# Option 2: clay, caco3, awc, flood, tmean_mean, ppt_cv
option2 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(tmean_mean) +
                          s(ppt_cv),
                        data = gam_data,
                        burnin = 1500,
                        samples = 1000, # increasing samples because of low ESS warning
                        family = mvgam::lognormal()) # Takes about 14 minutes

# Check summary
summary(option2)

# Option 3: clay, caco3, awc, flood, tmean_sd
option3 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(tmean_sd),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 9 minutes

# Check summary
summary(option3)

# Option 4: clay, caco3, awc, flood, tmin
option4 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(tmin),
                        data = gam_data,
                        family = mvgam::lognormal()) # Takes about 4 minutes

# Check summary
summary(option4)

# Option 5: clay, caco3, awc, flood, tmax, ppt_cv
option5 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(tmax) +
                          s(ppt_cv),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 9.5 minutes

# Check summary
summary(option5)

# Option 6: clay, caco3, awc, flood, tmax, tmean_sd
option6 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(tmax) +
                          s(tmean_sd),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 9.5 minutes

# Check summary
summary(option6)

# Option 7: clay, caco3, awc, flood, vpdmax, ppt_cv
option7 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(vpdmax) +
                          s(ppt_cv),
                        data = gam_data,
                        burnin = 1500,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 10.5 minutes

# Check summary
summary(option7)

# Option 8: clay, caco3, awc, flood, vpdmax, tmean_sd
option8 <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(caco3) +
                          s(awc) +
                          s(flood) +
                          s(vpdmax) +
                          s(tmean_sd),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 9 minutes

# Check summary
summary(option8)

# Compare model performance using leave-one-out cross validation
# The top model will be the one associated with the top row of the output,
# This will have the highest ELPD and the lowest SE
# The ELPD and SE columns are DIFFERENCE in ELPD and SE, so they are
# relative to the other models
mvgam::loo_compare(option1, option2,
                   option3, option4,
                   option5, option6,
                   option7, option8)

## Based on this analysis, option2 is the best

# Check whether removing the less important soil variables is better
# Option 2b: clay, tmean_mean, ppt_cv
option2b <- mvgam::mvgam(formula = total_density ~
                          s(clay) +
                          s(tmean_mean) + 
                          s(ppt_cv),
                        data = gam_data,
                        family = mvgam::lognormal()) # Takes about 3 minutes

# Check summary
summary(option2b)

# Compare option2 and option2b for which one is the best final model
mvgam::loo_compare(option2, option2b)

## Based on this, option 2 is still best, indicating that the uncorrelated
## but relatively unimportant soil variables are still contributing to better
## predictive performance

#### 3. Save final model ####

# Rename to match naming convention
density_gam_H_redcovar <- option2

# Save
save(density_gam_H_redcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData')

#### 4. Fit GAMs -- lower basis dimension ####

## Refitting all the models with maximum of 4 dimensions to basis

## Option 1: clay, caco3, awc, flood, ppt_sum, ppt_cv
option1_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(ppt_sum, k = 5) +
                          s(ppt_cv, k = 5),
                        data = gam_data,
                        family = mvgam::lognormal()) # Takes about 1 minute

# Check summary
summary(option1_4k)

# Option 2: clay, caco3, awc, flood, tmean_mean, ppt_cv
option2_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(tmean_mean, k = 5) +
                          s(ppt_cv, k = 5),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS warning
                        family = mvgam::lognormal()) # Takes about 5 minutes

# Check summary
summary(option2_4k)

# Option 3: clay, caco3, awc, flood, tmean_sd
option3_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(tmean_sd, k = 5),
                        data = gam_data,
                        family = mvgam::lognormal()) # Takes about 1.5 minutes

# Check summary
summary(option3_4k)

# Option 4: clay, caco3, awc, flood, tmin
option4_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(tmin, k = 5),
                        data = gam_data,
                        family = mvgam::lognormal()) # Takes about 1.5 minutes

# Check summary
summary(option4_4k)

# Option 5: clay, caco3, awc, flood, tmax, ppt_cv
option5_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(tmax, k = 5) +
                          s(ppt_cv, k = 5),
                        data = gam_data,
                        burnin = 1500,
                        samples = 1500, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 5 minutes

# Check summary
summary(option5_4k)

# Option 6: clay, caco3, awc, flood, tmax, tmean_sd
option6_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(tmax, k = 5) +
                          s(tmean_sd, k = 5),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 4 minutes

# Check summary
summary(option6_4k)

# Option 7: clay, caco3, awc, flood, vpdmax, ppt_cv
option7_4k <- mvgam::mvgam(formula = total_density ~
                          s(clay, k = 5) +
                          s(caco3, k = 5) +
                          s(awc, k = 5) +
                          s(flood, k = 5) +
                          s(vpdmax, k = 5) +
                          s(ppt_cv, k = 5),
                        data = gam_data,
                        burnin = 1000,
                        samples = 1000, # increasing samples because of low ESS
                        family = mvgam::lognormal()) # Takes about 5 minutes

# Check summary
summary(option7_4k)

## Note: option8 persistently leads to divergent transitions. This is likely
## because the covariates are too correlated. I am dropping this option

# Compare model performance using leave-one-out cross validation
# The top model will be the one associated with the top row of the output,
# This will have the highest ELPD and the lowest SE
# The ELPD and SE columns are DIFFERENCE in ELPD and SE, so they are
# relative to the other models
mvgam::loo_compare(option1_4k, option2_4k,
                   option3_4k, option4_4k,
                   option5_4k, option6_4k,
                   option7_4k)

## Based on this analysis, option5 is the best

# Check whether removing the less important soil variables is better
# Option 5b with 4 dimensional basis: clay, tmax, ppt_cv
option5b_4k <- mvgam::mvgam(formula = total_density ~
                           s(clay, k = 5) +
                           s(tmax, k = 5) + 
                           s(ppt_cv, k = 5),
                         data = gam_data,
                         family = mvgam::lognormal()) # Takes about 2 minutes

# Check summary
summary(option5b_4k)

# Compare option5_4k and option5b_4k for which one is the best final model
mvgam::loo_compare(option5_4k, option5b_4k)

## Based on this, option 5 is still best, indicating that the uncorrelated
## but relatively unimportant soil variables are still contributing to better
## predictive performance

#### 5. Save final model with lower basis dimension ####

# Rename to match naming convention
density_gam_H_redcovar_4k <- option5_4k

# Save
save(density_gam_H_redcovar_4k,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar_4k.RData')

#### 6. Partial effects plots ####

### Default basis dimension ###

# Get partial effects plots
partials <- mvgam::drawDotmvgam(density_gam_H_redcovar)

# Extract data
clay_data <- partials[[1]]$data
caco3_data <- partials[[2]]$data
awc_data <- partials[[3]]$data
flood_data <- partials[[4]]$data
tmean_data <- partials[[5]]$data
ppt_cv_data <- partials[[6]]$data

# Find minimum range of effect size
range(c(clay_data$.estimate - clay_data$.se,
        clay_data$.estimate + clay_data$.se,
        caco3_data$.estimate - caco3_data$.se,
        caco3_data$.estimate + caco3_data$.se,
        awc_data$.estimate - awc_data$.se,
        awc_data$.estimate + awc_data$.se,
        flood_data$.estimate - flood_data$.se,
        flood_data$.estimate + flood_data$.se,
        tmean_data$.estimate - tmean_data$.se,
        tmean_data$.estimate + tmean_data$.se,
        ppt_cv_data$.estimate - ppt_cv_data$.se,
        ppt_cv_data$.estimate + ppt_cv_data$.se))

# Soil % clay
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$clay,
                                    ymin = clay_data$.estimate - clay_data$.se,
                                    ymax = clay_data$.estimate + clay_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-5.2, 2.93)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

# Calcium carbonate concentration
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$caco3, y = caco3_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$caco3, y = caco3_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$caco3,
                                    ymin = caco3_data$.estimate - caco3_data$.se,
                                    ymax = caco3_data$.estimate + caco3_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-5.2, 2.93)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_caco3.png',
                height = 8, width = 9.5, units = 'cm')

# Available water content
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$awc, y = awc_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$awc, y = awc_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$awc,
                                    ymin = awc_data$.estimate - awc_data$.se,
                                    ymax = awc_data$.estimate + awc_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-5.2, 2.93)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_awc.png',
                height = 8, width = 9.5, units = 'cm')

# Fraction of grid cell in floodplain
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$flood, y = flood_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$flood, y = flood_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$flood,
                                    ymin = flood_data$.estimate - flood_data$.se,
                                    ymax = flood_data$.estimate + flood_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-5.2, 2.93)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

# Mean annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$tmean_mean, y = tmean_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$tmean_mean, y = tmean_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$tmean_mean,
                                    ymin = tmean_data$.estimate - tmean_data$.se,
                                    ymax = tmean_data$.estimate + tmean_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-5.2, 2.93)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

# Precipitation seasonality
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$ppt_cv, y = ppt_cv_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$ppt_cv, y = ppt_cv_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$ppt_cv,
                                    ymin = ppt_cv_data$.estimate - ppt_cv_data$.se,
                                    ymax = ppt_cv_data$.estimate + ppt_cv_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (CV)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-5.2, 2.93)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

### 4 dimensional basis ###

# Get partial effects plots
partials <- mvgam::drawDotmvgam(density_gam_H_redcovar_4k)

# Extract data
clay_data <- partials[[1]]$data
caco3_data <- partials[[2]]$data
awc_data <- partials[[3]]$data
flood_data <- partials[[4]]$data
tmax_data <- partials[[5]]$data
ppt_cv_data <- partials[[6]]$data

# Find maximum range of effect size
range(c(clay_data$.estimate - clay_data$.se,
        clay_data$.estimate + clay_data$.se,
        caco3_data$.estimate - caco3_data$.se,
        caco3_data$.estimate + caco3_data$.se,
        awc_data$.estimate - awc_data$.se,
        awc_data$.estimate + awc_data$.se,
        flood_data$.estimate - flood_data$.se,
        flood_data$.estimate + flood_data$.se,
        tmax_data$.estimate - tmax_data$.se,
        tmax_data$.estimate + tmax_data$.se,
        ppt_cv_data$.estimate - ppt_cv_data$.se,
        ppt_cv_data$.estimate + ppt_cv_data$.se))

# Soil % clay
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$clay,
                                    ymin = clay_data$.estimate - clay_data$.se,
                                    ymax = clay_data$.estimate + clay_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-3.31, 2.67)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_4k_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

# Calcium carbonate concentration
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$caco3, y = caco3_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$caco3, y = caco3_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$caco3,
                                    ymin = caco3_data$.estimate - caco3_data$.se,
                                    ymax = caco3_data$.estimate + caco3_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-3.31, 2.67)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_4k_partial_caco3.png',
                height = 8, width = 9.5, units = 'cm')

# Available water content
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$awc, y = awc_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$awc, y = awc_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$awc,
                                    ymin = awc_data$.estimate - awc_data$.se,
                                    ymax = awc_data$.estimate + awc_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-3.31, 2.67)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_4k_partial_awc.png',
                height = 8, width = 9.5, units = 'cm')

# Fraction of grid cell in floodplain
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$flood, y = flood_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$flood, y = flood_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$flood,
                                    ymin = flood_data$.estimate - flood_data$.se,
                                    ymax = flood_data$.estimate + flood_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-3.31, 2.67)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_4k_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

# Maximum annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$tmax, y = tmax_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$tmax, y = tmax_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$tmax,
                                    ymin = tmax_data$.estimate - tmax_data$.se,
                                    ymax = tmax_data$.estimate + tmax_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-3.31, 2.67)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_4k_partial_tmax.png',
                height = 8, width = 9.5, units = 'cm')

# Precipitation seasonality
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$ppt_cv, y = ppt_cv_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$ppt_cv, y = ppt_cv_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$ppt_cv,
                                    ymin = ppt_cv_data$.estimate - ppt_cv_data$.se,
                                    ymax = ppt_cv_data$.estimate + ppt_cv_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (CV)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-3.31, 2.67)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_4k_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')
