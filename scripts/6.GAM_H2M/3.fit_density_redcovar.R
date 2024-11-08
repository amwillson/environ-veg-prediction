## STEP 6-3

## Univariate GAM fit to HISTORICAL
## TOTAL STEM DENSITY and REDUCED covariates

## Reduced covariates referes to using a subset
## of all the covariates using AIC and ensuring
## that covariates are uncorrelated

## 1. Load data
## 2. Covariate correlations
## 3. Fit models
## 4. Compare R2 and AIC
## 5. Save final model
## 6. Partial effects plots

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

#### 2. Covariate correlations ####

# Select only covariate columns
covs <- dplyr::select(gam_data, -total_density)

# Correlations
cov_cors <- cor(covs)

# Keep only correlations > |0.7|
cov_cors[abs(cov_cors) < 0.7] <- NA

# Plot
corrplot::corrplot(cov_cors,
                   type = 'lower',
                   diag = FALSE,
                   na.label = ' ')

#### 3. Fit models ####

# Fit models with shrinkage smoothers with
# only minimally correlated covariates

option1 <- mgcv::gam(formula = total_density ~
                       s(clay, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(ppt_sum, k = 60, bs = 'ts') +
                       s(ppt_cv, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option2 <- mgcv::gam(formula = total_density ~
                       s(sand, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(ppt_sum, k = 60, bs = 'ts') +
                       s(ppt_cv, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option3 <- mgcv::gam(formula = total_density ~
                       s(clay, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmean_mean, k = 60, bs = 'ts') +
                       s(ppt_cv, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option4 <- mgcv::gam(formula = total_density ~
                       s(sand, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmean_mean, k = 60, bs = 'ts') +
                       s(ppt_cv, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option5 <- mgcv::gam(formula = total_density ~
                       s(clay, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmean_sd, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option6 <- mgcv::gam(formula = total_density ~
                       s(sand, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmean_sd, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option7 <- mgcv::gam(formula = total_density ~
                       s(clay, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmin, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option8 <- mgcv::gam(formula = total_density ~
                       s(sand, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmin, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option9 <- mgcv::gam(formula = total_density ~
                       s(clay, k = 60, bs = 'ts') +
                       s(caco3, k = 60, bs = 'ts') +
                       s(awc, k = 60, bs = 'ts') +
                       s(flood, k = 60, bs = 'ts') +
                       s(tmax, k = 60, bs = 'ts') +
                       s(ppt_cv, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

option10 <- mgcv::gam(formula = total_density ~
                        s(sand, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(tmax, k = 60, bs = 'ts') +
                        s(ppt_cv, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

option11 <- mgcv::gam(formula = total_density ~
                        s(clay, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(tmax, k = 60, bs = 'ts') +
                        s(tmean_sd, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

option12 <- mgcv::gam(formula = total_density ~
                        s(sand, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(tmax, k = 60, bs = 'ts') +
                        s(tmean_sd, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

option13 <- mgcv::gam(formula = total_density ~
                        s(clay, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(vpdmax, k = 60, bs = 'ts') +
                        s(ppt_cv, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

option14 <- mgcv::gam(formula = total_density ~
                        s(sand, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(vpdmax, k = 60, bs = 'ts') +
                        s(ppt_cv, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

option15 <- mgcv::gam(formula = total_density ~
                        s(clay, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(vpdmax, k = 60, bs = 'ts') +
                        s(tmean_sd, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

option16 <- mgcv::gam(formula = total_density ~
                        s(sand, k = 60, bs = 'ts') +
                        s(caco3, k = 60, bs = 'ts') +
                        s(awc, k = 60, bs = 'ts') +
                        s(flood, k = 60, bs = 'ts') +
                        s(vpdmax, k = 60, bs = 'ts') +
                        s(tmean_sd, k = 60, bs = 'ts'),
                      data = gam_data,
                      family = gaussian(link = 'log'))

#### 4. Compare R2 & AIC ####

# Which model has highest R2
which.max(c(summary(option1)$r.sq,
            summary(option2)$r.sq,
            summary(option3)$r.sq,
            summary(option4)$r.sq,
            summary(option5)$r.sq,
            summary(option6)$r.sq,
            summary(option7)$r.sq,
            summary(option8)$r.sq,
            summary(option9)$r.sq,
            summary(option10)$r.sq,
            summary(option11)$r.sq,
            summary(option12)$r.sq,
            summary(option13)$r.sq,
            summary(option14)$r.sq,
            summary(option16)$r.sq))

# Which model has lowest AIC
which.min(c(AIC(option1),
            AIC(option2),
            AIC(option3),
            AIC(option4),
            AIC(option5),
            AIC(option6),
            AIC(option7),
            AIC(option8),
            AIC(option9),
            AIC(option10),
            AIC(option11),
            AIC(option12),
            AIC(option13),
            AIC(option14),
            AIC(option15),
            AIC(option16)))

## Option 2 is best

# Check whether removing the extraneous soil variables is better
option2b <- mgcv::gam(formula = total_density ~
                       s(sand, k = 60, bs = 'ts') +
                       s(ppt_sum, k = 60, bs = 'ts') +
                       s(ppt_cv, k = 60, bs = 'ts'),
                     data = gam_data,
                     family = gaussian(link = 'log'))

summary(option2)$r.sq
summary(option2b)$r.sq

AIC(option2)
AIC(option2b)

## Option 2 is still best

##### 5. Save final model ####

# Rename to match naming convention
density_gam_H_redcovar <- option2

# Save
save(density_gam_H_redcovar,
     file = '/Volumes/FileBackup/SDM_bigdata/out/gam/H/density/redcovar.RData')

#### 6. Partial effects plots ####

# Get partial effects plots
partials <- mgcv::plot.gam(density_gam_H_redcovar)

# Extract data
sand_data <- partials[[1]]
caco3_data <- partials[[2]]
awc_data <- partials[[3]]
flood_data <- partials[[4]]
ppt_data <- partials[[5]]
ppt_cv_data <- partials[[6]]

# Find maximum range of effect size
range(c(sand_data$fit - sand_data$se,
        sand_data$fit + sand_data$se,
        caco3_data$fit - caco3_data$se,
        caco3_data$fit + caco3_data$se,
        awc_data$fit - awc_data$se,
        awc_data$fit + awc_data$se,
        flood_data$fit - flood_data$se,
        flood_data$fit + flood_data$se,
        ppt_data$fit - ppt_data$se,
        ppt_data$fit + ppt_data$se,
        ppt_cv_data$fit - ppt_cv_data$se,
        ppt_cv_data$fit + ppt_cv_data$se))

range(sand_data$fit, caco3_data$fit, awc_data$fit, 
      flood_data$fit, ppt_data$fit, ppt_cv_data$fit)

# Soil % sand
ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = sand_data$x, y = sand_data$fit)) +
  ggplot2::geom_line(ggplot2::aes(x = sand_data$x, y = sand_data$fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = sand_data$x,
                                    ymin = sand_data$fit - sand_data$se,
                                    ymax = sand_data$fit + sand_data$se),
                       alpha = 0.2) +
  ggplot2::xlab('Soil % sand') + ggplot2::ylab('Effect') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_sand.png',
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
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_caco3.png',
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
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_awc.png',
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
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_flood.png',
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
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_ppt_sum.png',
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
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/gam/H/density/fit/red_covar_partial_ppt_cv.png',
                height = 8, width = 9.5, units = 'cm')
