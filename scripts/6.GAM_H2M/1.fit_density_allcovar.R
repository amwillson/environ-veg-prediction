## STEP 6-1

## Univariate GAM fit to HISTORICAL
## TOTAL STEM DENSITY and CLIMATE and SOIL covariates

## 1. Load data
## 2. Plot GAM smooths
## 3. Fit GAM
## 4. Partial effects plots

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
startTime <- Sys.time()
density_gam_H_allcovar <- mgcv::gam(formula = total_density ~
                                      s(clay, k = 70) +
                                      s(sand, k = 70) +
                                      s(silt, k = 70) +
                                      s(caco3, k = 70) +
                                      s(awc, k = 70) +
                                      s(flood, k = 70) +
                                      s(ppt_sum, k = 70) +
                                      s(tmean_mean, k = 70) +
                                      s(ppt_cv, k = 70) +
                                      s(tmean_sd, k = 70) +
                                      s(tmin, k = 70) +
                                      s(tmax, k = 70) +
                                      s(vpdmax, k = 70), # formula with smooths for each covariate
                                    data = gam_data,
                                    family = gaussian(link = 'log'))
endTime <- Sys.time()
endTime - startTime # Takes about 1.8 hours

# Check summary
summary(density_gam_H_allcovar)

# Check if k is high enough
mgcv::gam.check(density_gam_H_allcovar)

## Running gam.check and increasing k by 10 was done iteratively
## until none of the k index significantly < 1
## Increasing all is fine because k will shrink -> 1 for
## linear functions anyway

# Save
save(density_gam_H_allcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/allcovar.RData')

#### 4. Partial effects plots ####

# Get partial effects plots
partials <- mgcv::plot.gam(density_gam_H_allcovar)

# Extract data
clay_data <- partials[[1]]
sand_data <- partials[[2]]
silt_data <- partials[[3]]
caco3_data <- partials[[4]]
awc_data <- partials[[5]]
flood_data <- partials[[6]]
ppt_data <- partials[[7]]
tmean_data <- partials[[8]]
ppt_cv_data <- partials[[9]]
tmean_sd_data <- partials[[10]]
tmin_data <- partials[[11]]
tmax_data <- partials[[12]]
vpd_data <- partials[[13]]

# Find maximum range of effect size
range(c(clay_data$fit - clay_data$se,
        clay_data$fit + clay_data$se,
        sand_data$fit - sand_data$se,
        sand_data$fit + sand_data$se,
        silt_data$fit - silt_data$se,
        silt_data$fit + silt_data$se,
        caco3_data$fit - caco3_data$se,
        caco3_data$fit + caco3_data$se,
        awc_data$fit - awc_data$se,
        awc_data$fit + awc_data$se,
        flood_data$fit - flood_data$se,
        flood_data$fit + flood_data$se,
        ppt_data$fit - ppt_data$se,
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

range(clay_data$fit, sand_data$fit, silt_data$fit,
      caco3_data$fit, awc_data$fit, flood_data$fit,
      ppt_data$fit, tmean_data$fit, ppt_cv_data$fit,
      tmean_sd_data$fit, tmin_data$fit, tmax_data$fit,
      vpd_data$fit)

# Soil % clay
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clay_data$x, y = clay_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = clay_data$x, y = clay_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = clay_data$x,
                                    ymin = clay_data$fit - clay_data$se,
                                    ymax = clay_data$fit + clay_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % clay') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-19, 17)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_clay.png',
                height = 8, width = 9.5, units = 'cm')

# Soil % sand
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x, y = sand_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x, y = sand_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x,
                                    ymin = sand_data$fit - sand_data$se,
                                    ymax = sand_data$fit + sand_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_sand.png',
                height = 8, width = 9.5, units = 'cm')

# Soil % silt
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = silt_data$x, y = silt_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = silt_data$x, y = silt_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = silt_data$x,
                                    ymin = silt_data$fit - silt_data$se,
                                    ymax = silt_data$fit + silt_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % silt') + ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_silt.png',
                height = 8, width = 9.5, units = 'cm')

# Calcium carbonate concentration
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = caco3_data$x, y = caco3_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = caco3_data$x, y = caco3_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = caco3_data$x,
                                    ymin = caco3_data$fit - caco3_data$se,
                                    ymax = caco3_data$fit + caco3_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil calcium carbonate concentration (%)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_caco3.png',
                height = 8, width = 9.5, units = 'cm')

# Available water content
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = awc_data$x, y = awc_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = awc_data$x, y = awc_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = awc_data$x,
                                   ymin = awc_data$fit - awc_data$se,
                                   ymax = awc_data$fit + awc_data$se),
                      alpha = 0.2) +
  ggplot2::xlab('Soil available water content (cm/cm)') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_awc.png',
                height = 8, width = 9.5, units = 'cm')

# Fraction of grid cell in floodplain
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = flood_data$x, y = flood_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = flood_data$x, y = flood_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = flood_data$x,
                                    ymin = flood_data$fit - flood_data$se,
                                    ymax = flood_data$fit + flood_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Fraction of grid cell in a floodplain') +
  ggplot2::ylab('Effect') +
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_flood.png',
                height = 8, width = 9.5, units = 'cm')

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
  ggplot2::ylim(c(-14, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_ppt_sum.png',
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
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_tmean_mean.png',
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
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_ppt_cv.png',
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
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_tmean_sd.png',
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
  ggplot2::ylim(c(-7, 5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_tmin.png',
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
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_tmax.png',
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
  ggplot2::ylim(c(-7, 4.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/all_covar_partial_vpdmax.png',
                height = 8, width = 9.5, units = 'cm')