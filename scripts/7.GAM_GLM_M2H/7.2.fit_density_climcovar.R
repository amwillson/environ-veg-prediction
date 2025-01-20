#### STEP 7-2

## Univariate GAM fit to MODERN
## TOTAL STEM DENSITY and CLIMATE covariates only

## NOTE that the random forest model is saved to an external hard
## drive. The object isn't THAT big, so it can be saved locally,
## but I elected to save it externally. The directory should be
## saved according to your file structure

## 1. Load data
## 2. Fit GAM
## 3. Fit GAM -- lower basis dimension
## 4. Partial effects plots

## Input: data/processed/FIA/xydata_in.RData
## Dataframe of in-sample grid cells with modern (FIA) era
## vegetation, soil, and climate data
## From 2.4.Split_data.R

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData
## Fitted GAM object saved to external hard drive
## Used in 7.5.density_historical_predictions.R,
## 7.6.density_modern_predictions.R

## Output: /Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar_4k.RData
## Fitted GAM object with lower maximum basis dimensionality to 
## reduce overfitting. Saved to external hard drive
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
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::distinct()

#### 2. Fit GAM ####

## Fit model with climate covariates

# Fit GAM
density_gam_M_climcovar <- mvgam::mvgam(formula = total_density ~
                                          s(ppt_sum) +
                                          s(tmean_mean) +
                                          s(ppt_cv) +
                                          s(tmean_sd) +
                                          s(tmin) +
                                          s(tmax) +
                                          s(vpdmax),
                                        data = gam_data,
                                        burnin = 1000,
                                        samples = 1000, # increasing samples because of low ESS warning
                                        family = mvgam::lognormal()) # Takes about 2 minutes

# Check summary
summary(density_gam_M_climcovar)

# Save
# Change directory according to your file structure
save(density_gam_M_climcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar.RData')

#### 3. Fit GAM -- fewer dimensions in basis function ####

# Fit GAM
# Note that setting k = 5 leads to 4 smooths per parameter
density_gam_M_climcovar_4k <- mvgam::mvgam(formula = total_density ~
                                             s(ppt_sum, k = 5) +
                                             s(tmean_mean, k = 5) +
                                             s(ppt_cv, k = 5) +
                                             s(tmean_sd, k = 5) +
                                             s(tmin, k = 5) +
                                             s(tmax, k = 5) +
                                             s(vpdmax, k = 5),
                                           data = gam_data,
                                           burnin = 1000,
                                           samples = 1000,
                                           family = mvgam::lognormal()) # Takes about 2 minutes

# Check summary
summary(density_gam_M_climcovar_4k)

# Save
# Change directory according to your file structure
save(density_gam_M_climcovar_4k,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/M/density/climcovar_4k.RData')

#### 4. Partial effects plots ####

### Default dimension of basis ###

# Get partial effects plots
partials <- mvgam::drawDotmvgam(density_gam_M_climcovar)

# Extract data
ppt_data <- partials[[1]]$data
tmean_data <- partials[[2]]$data
ppt_cv_data <- partials[[3]]$data
tmean_sd_data <- partials[[4]]$data
tmin_data <- partials[[5]]$data
tmax_data <- partials[[6]]$data
vpd_data <- partials[[7]]$data

# Find maximum range of effect size
range(c(ppt_data$.estimate - ppt_data$.se,
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

range(ppt_data$.estimate, tmean_data$.estimate, ppt_cv_data$.estimate,
      tmean_sd_data$.estimate, tmin_data$.estimate, tmax_data$.estimate,
      vpd_data$.estimate)

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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_ppt_sum.png',
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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_tmean_mean.png',
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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_ppt_cv.png',
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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_tmean_sd.png',
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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_tmin.png',
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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_tmax.png',
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
  ggplot2::ylim(c(-1.98, 0.84)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')

### 4 dimensional basis ###

# Get partial effects plots
partials <- mvgam::drawDotmvgam(density_gam_M_climcovar_4k)

# Extract data
ppt_data <- partials[[1]]$data
tmean_data <- partials[[2]]$data
ppt_cv_data <- partials[[3]]$data
tmean_sd_data <- partials[[4]]$data
tmin_data <- partials[[5]]$data
tmax_data <- partials[[6]]$data
vpd_data <- partials[[7]]$data

# Find maximum range of effect size
range(c(ppt_data$.estimate - ppt_data$.se,
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

range(ppt_data$.estimate, tmean_data$.estimate, ppt_cv_data$.estimate,
      tmean_sd_data$.estimate, tmin_data$.estimate, tmax_data$.estimate,
      vpd_data$.estimate)

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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_partial_ppt_sum.png',
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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_partial_tmean_mean.png',
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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_partial_ppt_cv.png',
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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_partial_tmean_sd.png',
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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_covar_partial_tmin.png',
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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_partial_tmax.png',
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
  ggplot2::ylim(c(-1.89, 0.86)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/M/density/fit/clim_covar_4k_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')
