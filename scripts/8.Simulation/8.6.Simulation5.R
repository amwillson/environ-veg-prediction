#### Simulation 5
#### Initial stem density randomly drawn within each ecosystem state
#### Environment change influences change in ecosystem state

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

# Generate continuous response variable (stem density)
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

## This includes the same gradients describing the ecosystem
## state transition as in Simulation 1

# Gradient from 0 to 1 representing smooth left-right gradient
base_var <- seq(from = 0, to = 1, length.out = nloc)

# Add a small amount of random noise
var1 <- base_var + rnorm(n = length(base_var),
                         mean = 0,
                         sd = 0.15)
var2 <- rev(base_var) + rnorm(n = length(base_var),
                              mean = 0,
                              sd = 0.25)
var3 <- exp(base_var) + rnorm(n = length(base_var),
                              mean = 0,
                              sd = 0.25)

# Variables with more stark change between locations of
# initial ecosystem states
base_var2 <- c(rep(0, times = nloc/2),
               rep(1, times = nloc/2))

# Add random noise
var4 <- base_var2 + rnorm(n = length(base_var2),
                          mean = 0,
                          sd = 0.25)
var5 <- rev(base_var2) + rnorm(n = length(base_var2),
                               mean = 0,
                               sd = 0.5)

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

# Variable 3
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = var3),
                     color = 'black') +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 3') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Purples') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 plot.title = ggplot2::element_text(size = 14, hjust = 0.5))

# Variable 4
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = var4),
                     color = 'black') +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 4') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Reds') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 plot.title = ggplot2::element_text(size = 14, hjust = 0.5))

# Variable 5
ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(1:nloc),
                                  y = 1,
                                  fill = var5),
                     color = 'black') +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 5') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Oranges') +
  ggplot2::theme_void() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 plot.title = ggplot2::element_text(size = 14, hjust = 0.5))

#### 4. Define relationship between environment and ecosystem ####
## I use a subset of the variables we chose, with their
## random variation, to fit the "true" relationship between
## ecosystem state and the environment.
## I fit between ecosystem state and environment here
## because we are not trying to capture within-state variation
## in stem density in this simulation.
## I include the random noise when fitting the relationship
## or else you end up with singularity in the relationship

# Fit "true" relationship between the binary response varaible
# and the environment
# Assuming here that only variables 1 and 5 matter
true_rel <- glm(factor(binary_response) ~ var1 + var5,
                family = 'binomial')

# Define true betas
# var 1 and 5 are the estimates from the fitted
# relationship
# var 2, 3, 4 are 0
true_beta <- unname(c(true_rel$coefficients[1], # intercept
                      true_rel$coefficients[2], # variable 1
                      0, 0, 0, # variables 2-4
                      true_rel$coefficients[3])) # variable 5

#### 5. Process evolution ####

# Initialize matrices
sim_binary <- matrix(, nrow = ntime,
                     ncol = nloc)
sim_binary[1,] <- binary_response

sim_cont <- matrix(, nrow = ntime,
                   ncol = nloc)
sim_cont[1,] <- cont_response

x1_mat <- x2_mat <- x3_mat <- x4_mat <- x5_mat <-
  matrix(, nrow = ntime,
         ncol = nloc)
x1_mat[1,] <- var1
x2_mat[1,] <- var2
x3_mat[1,] <- var3
x4_mat[1,] <- var4
x5_mat[1,] <- var5

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
    x5_mat[i,j] <- x5_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    
    # Random change for variables 2 and 3
    x2_mat[i,j] <- x2_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0,
                                         sd = 0.01)
    x3_mat[i,j] <- x3_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0,
                                         sd = 0.02)
    
    # No change in variable 4
    x4_mat[i,j] <- x4_mat[i-1,j]
    
    #### Dependent change in ecosystem state
    
    # Previous ecosystem state
    prev <- sim_binary[i-1,j]
    
    # Ecosystem state is a function of the environment
    # Predicted based on fitted coefficients
    curr <- round(plogis(true_beta[1] +
                           true_beta[2] * x1_mat[i,j] +
                           true_beta[3] * x2_mat[i,j] +
                           true_beta[4] * x3_mat[i,j] +
                           true_beta[5] * x4_mat[i,j] +
                           true_beta[6] * x5_mat[i,j]))
    
    ## Change in stem density based on ecosystem state
    
    # Previous stem density
    prev_density <- sim_cont[i-1,j]
    
    # If the state is the same, then randomly vary density
    if(prev == curr) curr_density <- rnorm(n = 1,
                                           mean = prev_density,
                                           sd = 5)
    
    # Adjustments to ensure that we maintain bimodality
    if(curr == 0 & curr_density > 47) curr_density <- 47
    if(curr == 0 & curr_density < 1) curr_density <- 1
    if(curr == 1 & curr_density < 150) curr_density <- 150
    
    # If there was a state shift, create a new stem density
    if(prev != curr){
      # If the state is savanna, draw from savanna distribution
      if(curr == 0) curr_density < runif(n = 1,
                                         min = 1,
                                         max = 47)
      # If the state is forest, draw from forest distribution
      if(curr == 1) curr_density <- rnorm(n = 1,
                                          mean = 250,
                                          sd = 75)
    }
    
    # Update matrices
    sim_binary[i,j] <- curr
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
x3_df <- reshape2::melt(x3_mat)
x4_df <- reshape2::melt(x4_mat)
x5_df <- reshape2::melt(x5_mat)

# Add column names
colnames(x1_df) <- c('time', 'space', 'var1')
colnames(x2_df) <- c('time', 'space', 'var2')
colnames(x3_df) <- c('time', 'space', 'var3')
colnames(x4_df) <- c('time', 'space', 'var4')
colnames(x5_df) <- c('time', 'space', 'var5')

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

# Variable 4
x4_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = var4),
                     color = 'black') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Reds') +
  ggplot2::scale_y_reverse() +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 4') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(angle = 90),
                 axis.text.y = ggplot2::element_text(size = 10))

# Variable 5
x5_df |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = factor(space),
                                  y = time,
                                  fill = var5),
                     color = 'black') +
  ggplot2::scale_fill_distiller(name = '',
                                palette = 'Oranges') +
  ggplot2::scale_y_reverse() +
  ggplot2::xlab('Space') + ggplot2::ylab('Time') +
  ggplot2::ggtitle('Variable 5') +
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
                   by = c('time', 'space')) |>
  dplyr::full_join(y = x3_df,
                   by = c('time', 'space')) |>
  dplyr::full_join(y = x4_df,
                   by = c('time', 'space')) |>
  dplyr::full_join(y = x5_df,
                   by = c('time', 'space'))

# Save
save(simulations,
     file = 'out/simulation/sim5.RData')
