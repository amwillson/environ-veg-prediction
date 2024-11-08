## STEP 6-2

## Univariate GAM fit to HISTORICAL
## TOTAL STEM DENSITY and CLIMATE covariates only

## 1. Load data
## 2. Fit GAM
## 3. Partial effects plots

rm(list = ls())

#### 1. Load data ####

# Load PLS data
load('data/processed/PLS/xydata_in.RData')

# Select relevant columns
gam_data <- pls_in |>
  dplyr::select(total_density, # response variable
                ppt_sum, tmean_mean, ppt_cv,
                tmean_sd, tmin, tmax, vpdmax) |> # climatic variables
  dplyr::distinct()

#### 2. Fit GAM ####

# Fit GAM
startTime <- Sys.time()
density_gam_H_climcovar <- mgcv::gam(formula = total_density ~
                                       s(ppt_sum, k = 60) +
                                       s(tmean_mean, k = 60) +
                                       s(ppt_cv, k = 60) +
                                       s(tmean_sd, k = 60) +
                                       s(tmin, k = 60) +
                                       s(tmax, k = 60) +
                                       s(vpdmax, k = 60), # formula with smooths for each covariate
                                     data = gam_data,
                                     family = gaussian(link = 'log'))
endTime = Sys.time()
endTime - startTime # Takes about 12 minutes

# Check summary
summary(density_gam_H_climcovar)

# Check if k is high enough
mgcv::gam.check(density_gam_H_climcovar)

# Save
save(density_gam_H_climcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/climcovar.RData')

#### 3. Partial effects plots ####

# Get partial effects plots
partials <- mgcv::plot.gam(density_gam_H_climcovar)

# Extract data
ppt_data <- partials[[1]]
tmean_data <- partials[[2]]
ppt_cv_data <- partials[[3]]
tmean_sd_data <- partials[[4]]
tmin_data <- partials[[5]]
tmax_data <- partials[[6]]
vpd_data <- partials[[7]]

# Find maximum range of effect size
range(c(ppt_data$fit - ppt_data$se,
        ppt_data$fit + ppt_data$se,
        tmean_data$fit - tmean_data$se,
        tmean_data$fit + tmean_data$se,
        ppt_cv_data$fit - ppt_cv_data$se,
        ppt_cv_data$fit + ppt_cv_data$se,
        tmean_sd_data$fit - tmean_sd_data$se,
        tmean_sd_data$fit + tmean_sd_data$se,
        tmin_data$fit - tmin_data$se,
        tmin_data$fit + tmin_data$se,
        tmax_data$fit - tmax_data$se,
        tmax_data$fit + tmax_data$se,
        vpd_data$fit - vpd_data$se,
        vpd_data$fit + vpd_data$se))

range(ppt_data$fit, tmean_data$fit, ppt_cv_data$fit,
      tmean_sd_data$fit, tmin_data$fit, tmax_data$fit,
      vpd_data$fit)

# Total annual precipitation
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_data$x, y = ppt_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_data$x, y = ppt_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_data$x,
                                    ymin = ppt_data$fit - ppt_data$se,
                                    ymax = ppt_data$fit + ppt_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Total annual precipitation (mm/year)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-13.1, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_ppt_sum.png',
                height = 8, width = 9.5, units = 'cm')

# Mean annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_data$x, y = tmean_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_data$x, y = tmean_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_data$x,
                                    ymin = tmean_data$fit - tmean_data$se,
                                    ymax = tmean_data$fit + tmean_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Mean annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7.3, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_tmean_mean.png',
                height = 8, width = 9.5, units = 'cm')

# Precipitation seasonality
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ppt_cv_data$x, y = ppt_cv_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = ppt_cv_data$x, y = ppt_cv_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = ppt_cv_data$x,
                                    ymin = ppt_cv_data$fit - ppt_cv_data$se,
                                    ymax = ppt_cv_data$fit + ppt_cv_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Precipitation seasonality (CV)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7.3, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')

# Temperature seasonality
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmean_sd_data$x, y = tmean_sd_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = tmean_sd_data$x, y = tmean_sd_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmean_sd_data$x,
                                    ymin = tmean_sd_data$fit - tmean_sd_data$se,
                                    ymax = tmean_sd_data$fit + tmean_sd_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Temperature seasonality (SD (°C))') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7.3, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_tmean_sd.png',
                height = 8, width = 9.5, units = 'cm')

# Minimum annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x, y = tmin_data$fit)) +
  ggplot2::geom_point(ggplot2::aes(x = tmin_data$x, y = tmin_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmin_data$x,
                                    ymin = tmin_data$fit - tmin_data$se,
                                    ymax = tmin_data$fit + tmin_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Minimum annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7.3, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_tmin.png',
                height = 8, width = 9.5, units = 'cm')

# Maximum annual temperature
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = tmax_data$x, y = tmax_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = tmax_data$x, y = tmax_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = tmax_data$x,
                                    ymin = tmax_data$fit - tmax_data$se,
                                    ymax = tmax_data$fit + tmax_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual temperature (°C)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7.3, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_tmax.png',
                height = 8, width = 9.5, units = 'cm')

# Maximum VPD
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = vpd_data$x, y = vpd_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = vpd_data$x, y = vpd_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = vpd_data$x,
                                    ymin = vpd_data$fit - vpd_data$se,
                                    ymax = vpd_data$fit + vpd_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Maximum annual vapor pressure deficit (hPa)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7.3, 5.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/clim_covar_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')
