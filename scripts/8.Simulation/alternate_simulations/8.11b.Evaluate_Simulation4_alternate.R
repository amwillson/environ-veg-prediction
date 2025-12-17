#### Analyzing Simulation 4- alternate
#### Fitting models and predicting stem density
#### Assumptions for Simulation 4 are as follows:
#### 1. Environment does not change over time
#### 2. Ecosystem does change over time, independently of environment
#### 3. Initial stem density is explained by environment
#### 4. Alternate has fewer environmental variables

rm(list = ls())

# Load output from Simulation 4
load('out/simulation/sim4_alt.RData')

# Number of locations
nloc <- 100
# Number of time steps
ntime <- 150

# First three time steps
first_steps <- 1:3
# Last three time steps
last_steps <- (ntime-2):ntime

# Set seed
set.seed(12)

#### 1. Data preparation ####

# Choose out of sample rows randomly
oos_rows <- sample(x = 1:nloc,
                   size = round(nloc * 0.3),
                   replace = FALSE)

# Take in-sample cells from first time period
simulation_in1 <- simulations |>
  dplyr::filter(time %in% first_steps) |>
  dplyr::arrange(space) |>
  dplyr::filter(!(space %in% oos_rows))

# Out-of-sample from first time period
simulation_oos1 <- simulations |>
  dplyr::filter(time %in% first_steps) |>
  dplyr::arrange(space) |>
  dplyr::filter(space %in% oos_rows)

# Out-of-sample from final time period
simulation_ooslast <- simulations |>
  dplyr::filter(time %in% last_steps) |>
  dplyr::arrange(space) |>
  dplyr::filter(space %in% oos_rows)

#### 2. Fit linear model ####

# Fit generalized linear model
fit_glm <- glm(formula = density ~ var1 + var2 + var3,
               data = simulation_in1,
               family = gaussian(link = 'identity'))

# Look at summary
summary(fit_glm)

#### 3. Fit random forest ####

# Tune hyperparameters
tune_rf <- randomForestSRC::tune(formula = density ~
                                   var1 + var2 + var3,
                                 ntreeTry = 500,
                                 data = simulation_in1)

# Look at optimal hyperparameter values
tune_rf$optimal

# Fit random forest
fit_rf <- randomForestSRC::rfsrc(formula = density ~
                                   var1 + var2 + var3,
                                 ntree = 1000,
                                 nodesize = tune_rf$optimal[1],
                                 mtry = tune_rf$optimal[2],
                                 data = simulation_in1)

# Look at summary
fit_rf

#### 4. Fit generalized additive model ####

# Fit GAM
fit_gam <- mvgam::mvgam(formula = density ~
                          s(var1, k = 3) + # reducing k because of convergence issues
                          s(var2, k = 3) + # ""
                          s(var3, k = 3),  # ""
                        data = dplyr::select(simulation_in1, -time),
                        burnin = 10000,
                        samples = 10000,
                        family = gaussian())

# Look at summary
summary(fit_gam)

#### 5. Make predictions ####

## Predict out-of-sample locations from first time step

# GLM
pred_1_glm <- predict(object = fit_glm,
                      newdata = dplyr::select(simulation_oos1,
                                              var1:var3),
                      type = 'response')

# RF
pred_1_rf <- randomForestSRC::predict.rfsrc(object = fit_rf,
                                            newdata = dplyr::select(simulation_oos1,
                                                                    var1:var3))

# GAM
pred_1_gam <- mvgam::predictions(model = fit_gam,
                                 newdata = dplyr::select(simulation_oos1,
                                                         var1:var3))

## Predict out-of-sample locations from final time step

# GLM
pred_final_glm <- predict(object = fit_glm,
                          newdata = dplyr::select(simulation_ooslast,
                                                  var1:var3),
                          type = 'response')

# RF
pred_final_rf <- randomForestSRC::predict.rfsrc(object = fit_rf,
                                                newdata = dplyr::select(simulation_ooslast,
                                                                        var1:var3))

# GAM
pred_final_gam <- mvgam::predictions(model = fit_gam,
                                     newdata = dplyr::select(simulation_ooslast,
                                                             var1:var3))

#### 6. Evaluate predictions ####

## Plots of prediction accuracy from first time step

# GLM
p_glm_1 <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = simulation_oos1$density,
                                   y = pred_1_glm)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = simulation_oos1$density,
                                    y = pred_1_glm),
                       method = 'lm', se = FALSE) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('First time step',
                   subtitle = 'Generalized linear model') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5))

# RF
p_rf_1 <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = simulation_oos1$density,
                                   y = pred_1_rf$predicted)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = simulation_oos1$density,
                                    y = pred_1_rf$predicted),
                       method = 'lm', se = FALSE) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('First time step',
                   subtitle = 'Random forest') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5))

# GAM
p_gam_1 <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = simulation_oos1$density,
                                   y = pred_1_gam$estimate)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = simulation_oos1$density,
                                    y = pred_1_gam$estimate),
                       method = 'lm', se = FALSE) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('First time step',
                   subtitle = 'Generalized additive model') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5))

# Plot together
cowplot::plot_grid(p_glm_1, p_rf_1, p_gam_1,
                   nrow = 1)

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/simulations/sim_4_alt_pred_1.png')

## Plots of prediction accuracy from final time step

# GLM
p_glm_final <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = simulation_ooslast$density,
                                   y = pred_final_glm)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = simulation_ooslast$density,
                                    y = pred_final_glm),
                       method = 'lm', se = FALSE) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Final time step',
                   subtitle = 'Generalized linear model') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5))

# RF
p_rf_final <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = simulation_ooslast$density,
                                   y = pred_final_rf$predicted)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = simulation_ooslast$density,
                                    y = pred_final_rf$predicted),
                       method = 'lm', se = FALSE) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Final time step',
                   subtitle = 'Random forest') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5))

# GAM
p_gam_final <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = simulation_ooslast$density,
                                   y = pred_final_gam$estimate)) +
  ggplot2::geom_abline() +
  ggplot2::geom_smooth(ggplot2::aes(x = simulation_ooslast$density,
                                    y = pred_final_gam$estimate),
                       method = 'lm', se = FALSE) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Final time step',
                   subtitle = 'Generalized additive model') +
  tune::coord_obs_pred() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5))

# Plot together
cowplot::plot_grid(p_glm_final, p_rf_final, p_gam_final,
                   nrow = 1)

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/simulations/sim_4_alt_pred_final.png')

## Calculate correlations between observed and predicted stem density

## First time step

# GLM
corr_1_lm <- cor(simulation_oos1$density,
                 pred_1_glm)
# RF
corr_1_rf <- cor(simulation_oos1$density,
                 pred_1_rf$predicted)
# GAM
corr_1_gam <- cor(simulation_oos1$density,
                  pred_1_gam$estimate)

## Final time step

# GLM
corr_final_lm <- cor(simulation_ooslast$density,
                     pred_final_glm)
# RF
corr_final_rf <- cor(simulation_ooslast$density,
                     pred_final_rf$predicted)
# GAM
corr_final_gam <- cor(simulation_ooslast$density,
                      pred_final_gam$estimate)

# Make columns for table
model <- c('GLM', 'RF', 'GAM')
first <- c(corr_1_lm, corr_1_rf, corr_1_gam)
final <- c(corr_final_lm, corr_final_rf,  corr_final_gam)

# Combine columns
corrs <- as.data.frame(cbind(model,
                             first,
                             final))

# Add column names
colnames(corrs) <- c('Model', 'First step', 'Final step')

# Formatting
corrs$`First step` <- as.numeric(corrs$`First step`)
corrs$`Final step` <- as.numeric(corrs$`Final step`)

# Make table
kableExtra::kable(corrs, digits = 3)
