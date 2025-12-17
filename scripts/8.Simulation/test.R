#### Simulation 6- alternate
#### Initial stem density is explained by initial environmental
#### conditions, in addition to ecosystem state
#### Environment change influences change in ecosystem state
#### Fewer environmental variables

rm(list = ls())

# Number of locations in 1D space
nloc <- 100
# Number of time steps
ntime <- 150

# Set seed for generating initial conditions
set.seed(1)

#### 1. Define ecosystem initial conditions ####

# Create 0s and 1s defining ecosystem state along left-right axis
binary_response <- c(rep(0, times = nloc/2),
                     rep(1, times = nloc/2))

# Plot to show initial state distribution
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = factor(binary_response)),
                     color = 'black') +
  ggplot2::scale_fill_manual(limits = factor(c(0, 1)),
                             values = c('#94bd46', '#1a5200'),
                             name = 'State',
                             labels = c('Savanna', 'Forest')) +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90))

#### 2. Define initial stem density ####

# Generate continuous respones variable (stem density)
# from ecosystem state
cont_response <- c(runif(n = nloc/2, min = 1, max = 47),
                   rnorm(n = nloc/2, mean = 250, sd = 75))

# Plot initial stem density
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = cont_response),
                     color = 'black') +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Greens',
                                direction = 1) +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90))

# Density plot showing bimodality
ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = cont_response)) +
  ggplot2::xlab('Stem density') +
  ggplot2::theme_minimal()

#### 3. Define environmental conditions ####

# This includes the same gradients describing the ecosystem
## state transition in Simulation 1
## but also includes environmental variables correlated
## with stem density in the first time period
## This is the same as Simulation 2

# Variable correlated with stem density
base_var1 <- scales::rescale(cont_response, to = c(0, 1))

# Add a small amount of random noise
var1 <- base_var1 + rnorm(n = length(base_var1),
                          mean = 0,
                          sd = 0.1)

# Variables with more stark change between locations of
# initial ecosystem states
base_var2 <- c(rep(0, times = nloc/2),
               rep(1, times = nloc/2))

# Add random noise
var2 <- base_var2 + rnorm(n = length(base_var2),
                          mean = 0,
                          sd = 0.1)

## Plot initial values

# Variable 1
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = var1),
                     color = 'black') +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 1') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Blues') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 plot.title = ggplot2::element_text(size = 14, hjust = 0.5))

# Variable 2
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = var2),
                     color = 'black') +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 2') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Greys') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 plot.title = ggplot2::element_text(size = 14, hjust = 0.5))

#### 4. Define relationship between environment and ecosystem ####
## I define the relationship in two steps: one for
## ecosystem state and one for stem density within the forest

## The first part is equivalent to ecosystem state prediction
## in Simulation 5

# Fit "true" relationship between the binary response
# variable and the environment
# Assuming here that only variables 1 and 4 matter
#true_rel_bin <- glm(factor(binary_response) ~ var1 + var3,
#                    family = 'binomial')
#true_rel_bin <- logistf::logistf(binary_response ~ var1 + var3,
#                                 control = logistf::logistf.control(maxit = 1000))

# Define true betas for binary response
# vars 1 & 5 are the estimates from the fitted model
# var 2-4 are 0
#true_beta_bin <- unname(c(true_rel_bin$coefficients[1], # intercept
#                          true_rel_bin$coefficient[2], # variable 1
#                          0, # variables 2-3
#                          true_rel_bin$coefficients[3])) # variable 4

# Subset only forest and only savanna stem density cells
#only_sav <- cont_response[1:(nloc/2)]
#only_for <- cont_response[(nloc/2+1):nloc]

# Fit "true" relationship between environment and
# the continuous response variable
#true_rel_cont_sav <- lm(only_sav ~ var2[1:(nloc/2)])
#true_rel_cont_for <- lm(only_for ~ var2[(nloc/2+1):nloc])

# Define true betas for continuous response
# var 2 is the estimate from the model
# vars 1 and 3-5 are 0
#true_beta_cont_sav <- unname(c(true_rel_cont_sav$coefficients[1], # intercept
#                               0, # variable 1
#                               true_rel_cont_sav$coefficients[2], # variable 2
#                               0)) # variables 3-5
#true_beta_cont_for <- unname(c(true_rel_cont_for$coefficients[1], # intercept
#                               0, # variable 1
#                               true_rel_cont_for$coefficients[2], # variable 2
#                               0)) # variables 3-5

true_rel <- lm(cont_response ~ var1 + var2)
true_beta <- unname(true_rel$coefficients)

#### 5. Process evolution ####

# Initialize matrices
sim_binary <- matrix(, nrow = ntime,
                     ncol = nloc)
sim_binary[1,] <- binary_response

sim_cont <- matrix(, nrow = ntime,
                   ncol = nloc)
sim_cont[1,] <- cont_response

x1_mat <- x2_mat <- #x3_mat <-
  matrix(, nrow = ntime,
         ncol = nloc)
x1_mat[1,] <- var1
x2_mat[1,] <- var2
#x3_mat[1,] <- var3

# Set seed again
set.seed(1)

# Loop over each row (time step) after initial conditions
for(i in 2:nrow(sim_binary)){
  # Loop over each column (location)
  for(j in 1:ncol(sim_binary)){
    #### Environmental change
    
    # Increasing pattern for variables 1 and 5
    x1_mat[i,j] <- x1_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    x2_mat[i,j] <- x2_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    
    # Random change for variables 2 and 3
    #x2_mat[i,j] <- x2_mat[i-1,j] + rnorm(n = 1,
    #                                     mean = 0,
    #                                     sd = 0.01)
    
    #### Dependent change in ecosystem state
    
    # Previous ecosystem state
    prev <- sim_binary[i-1,j]
    
    # Ecosystem state is a function of the environment
    # Predicted based on fitted coefficients
    curr_density <- true_beta[1] +
                           true_beta[2] * x1_mat[i,j] +
                           true_beta[3] * x2_mat[i,j]
    
    ## Change in stem density based on ecosystem state
    ## and environmental conditions
    
    # If the state is savanna, use savanna relationship
    #if(curr == 0) curr_density <-
    #  true_beta_cont_sav[1] +
    #  true_beta_cont_sav[2] * x1_mat[i,j] +
    #  true_beta_cont_sav[3] * x2_mat[i,j] +
    #  true_beta_cont_sav[4] * x3_mat[i,j]
    
    # If the state is forest, use forest relationship
    #if(curr == 1) curr_density <-
    #  true_beta_cont_for[1] +
    #  true_beta_cont_for[2] * x1_mat[i,j] +
    #  true_beta_cont_for[3] * x2_mat[i,j] +
    #  true_beta_cont_for[4] * x3_mat[i,j]
    
    # Adjustments to ensure that we maintain bimodality
    #if(curr == 0 & curr_density > 47) curr_density <- 47
    #if(curr == 0 & curr_density < 1) curr_density <- 1
    #if(curr == 1 & curr_density < 150) curr_density <- 150
    
    # Update matrices
    #sim_binary[i,j] <- curr
    sim_cont[i,j] <- curr_density
  }
}

#### 6. Format & visualize ecosystem state ####

# Melt matrix
sim_binary_df <- reshape2::melt(sim_binary)

# Add column names
colnames(sim_binary_df) <- c('time', 'space', 'value')

# Plot change over time
sim_binary_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = factor(value)),
                     color = 'black') +
  ggplot2::scale_fill_manual(limits = factor(c(0, 1)),
                             values = c('#94bd46', '#1a5200'),
                             name = 'State',
                             labels = c('Savanna', 'Forest')) +
  ggplot2::scale_y_reverse() +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 axis.text.y = ggplot2::element_text(size = 10))

nforest <- length(which(dplyr::filter(sim_binary_df, time == ntime)$value == 1)) / nloc * 100
nsavanna <- 100 - nforest
paste0(nforest, '% forest and ', nsavanna, '% savanna')

#### 7. Format & visualize stem density ####

# Melt matrix
sim_cont_df <- reshape2::melt(sim_cont)

# Add column names
colnames(sim_cont_df) <- c('time', 'space', 'value')

# Plot stem density
sim_cont_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = value),
                     color = 'black') +
  ggplot2::scale_y_reverse() +
  ggplot2::scale_fill_distiller(name = 'stems/ha',
                                palette = 'Greens',
                                direction = 1) +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 axis.text.y = ggplot2::element_text(size = 10))

#### 8. Format & visualize environmental variables ####

# Melt matrices
x1_df <- reshape2::melt(x1_mat)
x2_df <- reshape2::melt(x2_mat)
#x3_df <- reshape2::melt(x3_mat)

# Add column names
colnames(x1_df) <- c('time', 'space', 'var1')
colnames(x2_df) <- c('time', 'space', 'var2')
#colnames(x3_df) <- c('time', 'space', 'var3')

## Plot predictor variables over time

# Variable 1
x1_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = var1),
                     color = 'black') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Blues') +
  ggplot2::scale_y_reverse() +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 1') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 axis.text.y = ggplot2::element_text(size = 10))

# Variable 2
x2_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = var2),
                     color = 'black') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Greys') +
  ggplot2::scale_y_reverse() +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 2') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 axis.text.y = ggplot2::element_text(size = 10))

# Variable 3
x3_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = var3),
                     color = 'black') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Purples') +
  ggplot2::scale_y_reverse() +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 3') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 axis.text.y = ggplot2::element_text(size = 10))

#### 9. Save ####

# Combine
simulations <- sim_cont_df |>
  dplyr::rename(density = value) |>
  dplyr::full_join(y = x1_df,
                   by = c('time', 'space')) |>
  dplyr::full_join(y = x2_df,
                   by = c('time', 'space')) #|>
  dplyr::full_join(y = x3_df,
                   by = c('time', 'space'))

# Save
save(simulations,
     file = 'out/simulation/sim6_alt2.RData')
