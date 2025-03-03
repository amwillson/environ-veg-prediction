#### STEP 7-1

## Univariate GAM fit to MODERN
## TOTAL STEM DENSITY and CLIMATE and SOIL covariates

## NOTE that the GAM is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to your file structure

## 1. Load data
## 2. Plot GAM smooths
## 3. Fit GAM
## 4. Fit GAM -- lower basis dimension
## 5. Partial effects plots

## Input: data/processed/FIA/xydata_in.RData
## Dataframe of in-sample grid cells with modern (FIA) era
## vegetation, soil, and climate data
## From 2.4.Split_data.R

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData
## Fitted GAM object saved to external hard drive
## Used in 7.5.density_historical_predictions.R,
## 7.6.density_modern_predictions.R

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar_4k.RData
## Fitted GAM object with lower maximum basis dimensionality
## to reduce overfitting. Saved to external hard drive
## Used in 7.5.density_historical_predictions.R,
## 7.6.density_modern_predictions.R

## Figures of partial effects plots also saved to figures/ directory

rm(list = ls())

#### 1. Load data ####

# Load FIA data
load('data/processed/FIA/xydata_in.RData')

# Select relevant columns
gam_data <- fia_in |>
  dplyr::ungroup() |>
  dplyr::rename(total_density = total_stem_density) |>
  dplyr::select(total_density, # response variable
                clay, sand, silt, caco3, awc, flood, # edaphic variables
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::distinct()

#### 2. Plot GAM smooths  ####

## Plotting the relationship between each covariate
## and the response with GAM smooths to see the trends

# Soil % clay
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = clay, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Soil % clay') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Soil % sand
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = sand, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Soil % sand') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Soil % silt
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = silt, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Soil % silt') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Soil calcium carbonate concentration
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = caco3, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Available water content
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = awc, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Soil available water content (cm/cm)') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Fraction of grid cell in floodplain
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = flood, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Fraction of grid cell in a floodplain') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Total annual precipitation
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_sum, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = ppt_sum, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Total annual precipitation (mm/year)') + 
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Mean annual temperature
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_mean, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = tmean_mean, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Mean annual temperature (°C)') +
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Precipitation seasonality
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = ppt_cv, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Precipitation seasonality (CV)') +
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Temperature seasonality
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = tmean_sd, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Temperature seasonality (SD, °C)') +
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Minimum annual temperature
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = tmin, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Minimum annual temperature (°C)') +
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Maximum annual temperature
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = tmax, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Maximum annual temperature (°C)') +
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

# Maximum VPD
gam_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpdmax, y = total_density)) +
  ggplot2::geom_smooth(ggplot2::aes(x = vpdmax, y = total_density),
                       method = 'gam') +
  ggplot2::xlab('Maximum vapor pressure deficit (hPa)') +
  ggplot2::ylab('Total stem density (stems/ha)') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

#### 3. Fit GAM ####

## Fit model with all covariates, even though they are correlated
## Precedent for this in Charney et al. 2021

# Fit GAM
density_gam_M_allcovar <- mvgam::mvgam(formula = total_density ~
                                         s(clay) +
                                         s(sand) +
                                         s(silt) +
                                         s(caco3) +
                                         s(awc) +
                                         s(flood) +
                                         s(ppt_sum) +
                                         s(tmean_mean) +
                                         s(ppt_cv) +
                                         s(tmean_sd) +
                                         s(tmin) +
                                         s(tmax) +
                                         s(vpdmax),
                                       data = gam_data,
                                       burnin = 1000,
                                       samples = 1000, # increasing samples because of ESS warning
                                       family = mvgam::lognormal()) # Takes about 12 minutes

# Check summary
summary(density_gam_M_allcovar)

# Save
# Change directory according to your file structure
save(density_gam_M_allcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar.RData')

#### 4. Fit GAM -- fewer basis functions ####

# Fit GAM
# Note that setting k = 5 leads to 4 smooths per parameter
density_gam_M_allcovar_4k <- mvgam::mvgam(formula = total_density ~
                                            s(clay, k = 5) +
                                            s(sand, k = 5) +
                                            s(silt, k = 5) +
                                            s(caco3, k = 5) +
                                            s(awc, k = 5) +
                                            s(flood, k = 5) +
                                            s(ppt_sum, k = 5) +
                                            s(tmean_mean, k = 5) +
                                            s(ppt_cv, k = 5) +
                                            s(tmean_sd, k = 5) +
                                            s(tmin, k = 5) +
                                            s(tmax, k = 5) +
                                            s(vpdmax, k = 5),
                                          data = gam_data,
                                          burnin = 1000,
                                          samples = 1000, # increasing samples because of ESS warning
                                          family = mvgam::lognormal()) # Takes about 2 minutes

# Check summary
summary(density_gam_M_allcovar_4k)

# Save
# Change directory according to your file structure
save(density_gam_M_allcovar_4k,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/allcovar_4k.RData')

#### 5. Partial effects plots ####

### Default basis dimension ###

# Get partial effects plots
partials <- mvgam::drawDotmvgam(density_gam_M_allcovar)

# Extract data
clay_data <- partials[[1]]$data
sand_data <- partials[[2]]$data
silt_data <- partials[[3]]$data
caco3_data <- partials[[4]]$data
awc_data <- partials[[5]]$data
flood_data <- partials[[6]]$data
ppt_data <- partials[[7]]$data
tmean_data <- partials[[8]]$data
ppt_cv_data <- partials[[9]]$data
tmean_sd_data <- partials[[10]]$data
tmin_data <- partials[[11]]$data
tmax_data <- partials[[12]]$data
vpd_data <- partials[[13]]$data

# Find maximum range of effect size
range(c(clay_data$.estimate - clay_data$.se,
        clay_data$.estimate + clay_data$.se,
        sand_data$.estimate - sand_data$.se,
        sand_data$.estimate + sand_data$.se,
        silt_data$.estimate - silt_data$.se,
        silt_data$.estimate + silt_data$.se,
        caco3_data$.estimate - caco3_data$.se,
        caco3_data$.estimate + caco3_data$.se,
        awc_data$.estimate - awc_data$.se,
        awc_data$.estimate + awc_data$.se,
        flood_data$.estimate - flood_data$.se,
        flood_data$.estimate + flood_data$.se,
        ppt_data$.estimate - ppt_data$.se,
        ppt_data$.estimate + ppt_data$.se,
        tmean_data$.estimate - tmean_data$.se,
        tmean_data$.estimate + tmean_data$.se,
        ppt_cv_data$.estimate - ppt_cv_data$.se,
        ppt_cv_data$.estimate + ppt_cv_data$.se,
        tmean_sd_data$.estimate - tmean_sd_data$.se,
        tmean_sd_data$.estimate + tmean_sd_data$.se,
        tmin_data$.estimate - tmin_data$.se,
        tmin_data$.estimate + tmin_data$.se,
        tmax_data$.estimate - tmax_data$.se,
        tmax_data$.estimate + tmax_data$.se,
        vpd_data$.estimate - vpd_data$.se,
        vpd_data$.estimate + vpd_data$.se))

range(clay_data$.estimate, sand_data$.estimate, silt_data$.estimate,
      caco3_data$.estimate, awc_data$.estimate, flood_data$.estimate,
      ppt_data$.estimate, tmean_data$.estimate, ppt_cv_data$.estimate,
      tmean_sd_data$.estimate, tmin_data$.estimate, tmax_data$.estimate,
      vpd_data$.estimate)

# Soil % clay
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$clay,
                                    ymin = clay_data$.estimate - clay_data$.se,
                                    ymax = clay_data$.estimate + clay_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

# Soil % sand
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$sand, y = sand_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$sand, y = sand_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$sand,
                                    ymin = sand_data$.estimate - sand_data$.se,
                                    ymax = sand_data$.estimate + sand_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_sand.png',
                height = 8, width = 9.5, units = 'cm')

# Soil % silt
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$silt, y = silt_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$silt, y = silt_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$silt,
                                    ymin = silt_data$.estimate - silt_data$.se,
                                    ymax = silt_data$.estimate + silt_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_silt.png',
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
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_caco3.png',
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
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_awc.png',
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
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

# Total annual precipitation
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$ppt_sum, y = ppt_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$ppt_sum, y = ppt_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$ppt_sum,
                                    ymin = ppt_data$.estimate - ppt_data$.se,
                                    ymax = ppt_data$.estimate + ppt_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_ppt_sum.png',
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
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_tmean_mean.png',
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
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

# Temperature seasonality
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$tmean_sd, y = tmean_sd_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$tmean_sd, y = tmean_sd_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$tmean_sd,
                                    ymin = tmean_sd_data$.estimate - tmean_sd_data$.se,
                                    ymax = tmean_sd_data$.estimate + tmean_sd_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (SD (°C))') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

# Minimum annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$tmin, y = tmin_data$.estimate)) +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$tmin, y = tmin_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$tmin,
                                    ymin = tmin_data$.estimate - tmin_data$.se,
                                    ymax = tmin_data$.estimate + tmin_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_tmin.png',
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
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_tmax.png',
                height = 8, width = 9.5, units = 'cm')

# Maximum VPD
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$vpdmax, y = vpd_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$vpdmax, y = vpd_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$vpdmax,
                                    ymin = vpd_data$.estimate - vpd_data$.se,
                                    ymax = vpd_data$.estimate + vpd_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.55, 0.83)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### 4 dimensional basis function ###

# Get partial effects plots
partials <- mvgam::drawDotmvgam(density_gam_M_allcovar_4k)

# Extract data
clay_data <- partials[[1]]$data
sand_data <- partials[[2]]$data
silt_data <- partials[[3]]$data
caco3_data <- partials[[4]]$data
awc_data <- partials[[5]]$data
flood_data <- partials[[6]]$data
ppt_data <- partials[[7]]$data
tmean_data <- partials[[8]]$data
ppt_cv_data <- partials[[9]]$data
tmean_sd_data <- partials[[10]]$data
tmin_data <- partials[[11]]$data
tmax_data <- partials[[12]]$data
vpd_data <- partials[[13]]$data

# Find maximum range of effect size
range(c(clay_data$.estimate - clay_data$.se,
        clay_data$.estimate + clay_data$.se,
        sand_data$.estimate - sand_data$.se,
        sand_data$.estimate + sand_data$.se,
        silt_data$.estimate - silt_data$.se,
        silt_data$.estimate + silt_data$.se,
        caco3_data$.estimate - caco3_data$.se,
        caco3_data$.estimate + caco3_data$.se,
        awc_data$.estimate - awc_data$.se,
        awc_data$.estimate + awc_data$.se,
        flood_data$.estimate - flood_data$.se,
        flood_data$.estimate + flood_data$.se,
        ppt_data$.estimate - ppt_data$.se,
        ppt_data$.estimate + ppt_data$.se,
        tmean_data$.estimate - tmean_data$.se,
        tmean_data$.estimate + tmean_data$.se,
        ppt_cv_data$.estimate - ppt_cv_data$.se,
        ppt_cv_data$.estimate + ppt_cv_data$.se,
        tmean_sd_data$.estimate - tmean_sd_data$.se,
        tmean_sd_data$.estimate + tmean_sd_data$.se,
        tmin_data$.estimate - tmin_data$.se,
        tmin_data$.estimate + tmin_data$.se,
        tmax_data$.estimate - tmax_data$.se,
        tmax_data$.estimate + tmax_data$.se,
        vpd_data$.estimate - vpd_data$.se,
        vpd_data$.estimate + vpd_data$.se))

range(clay_data$.estimate, sand_data$.estimate, silt_data$.estimate,
      caco3_data$.estimate, awc_data$.estimate, flood_data$.estimate,
      ppt_data$.estimate, tmean_data$.estimate, ppt_cv_data$.estimate,
      tmean_sd_data$.estimate, tmin_data$.estimate, tmax_data$.estimate,
      vpd_data$.estimate)

# Soil % clay
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$clay, y = clay_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$clay,
                                    ymin = clay_data$.estimate - clay_data$.se,
                                    ymax = clay_data$.estimate + clay_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

# Soil % sand
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$sand, y = sand_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$sand, y = sand_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$sand,
                                    ymin = sand_data$.estimate - sand_data$.se,
                                    ymax = sand_data$.estimate + sand_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_sand.png',
                height = 8, width = 9.5, units = 'cm')

# Soil % silt
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$silt, y = silt_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$silt, y = silt_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$silt,
                                    ymin = silt_data$.estimate - silt_data$.se,
                                    ymax = silt_data$.estimate + silt_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_silt.png',
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
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_caco3.png',
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
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_awc.png',
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
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

# Total annual precipitation
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$ppt_sum, y = ppt_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$ppt_sum, y = ppt_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$ppt_sum,
                                    ymin = ppt_data$.estimate - ppt_data$.se,
                                    ymax = ppt_data$.estimate + ppt_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_ppt_sum.png',
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
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_tmean_mean.png',
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
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

# Temperature seasonality
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$tmean_sd, y = tmean_sd_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$tmean_sd, y = tmean_sd_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$tmean_sd,
                                    ymin = tmean_sd_data$.estimate - tmean_sd_data$.se,
                                    ymax = tmean_sd_data$.estimate + tmean_sd_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (SD (°C))') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

# Minimum annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$tmin, y = tmin_data$.estimate)) +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$tmin, y = tmin_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$tmin,
                                    ymin = tmin_data$.estimate - tmin_data$.se,
                                    ymax = tmin_data$.estimate + tmin_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_4k_covar_partial_tmin.png',
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
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_tmax.png',
                height = 8, width = 9.5, units = 'cm')

# Maximum VPD
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$vpdmax, y = vpd_data$.estimate)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$vpdmax, y = vpd_data$.estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$vpdmax,
                                    ymin = vpd_data$.estimate - vpd_data$.se,
                                    ymax = vpd_data$.estimate + vpd_data$.se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-1.31, 0.89)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/all_covar_4k_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')
