#### Simulation 1
#### Initial stem density randomly drawn within each ecosystem state
#### Environmental & ecosystem change independent of each other

rm(list = ls())

# Number of locations in 1D space
nloc <- 100
# Number of time steps
ntime <- 150

# Load ecosystem state over time
load('out/simulation/state_change_ind.RData')

# Set seed for generating initial conditions
set.seed(1)

#### 1. Define initial stem density ####

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

#### 2. Define initial environmental conditions ####

## This is the simplest possible case where environmental conditions
## simply show gradients from left to right

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

#### 3. Stem density evolution ####

# Initialize matrices
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

# Loop over each row (time step) after initial conditions
for(i in 2:ntime){
  # Loop over each column (location)
  for(j in 1:nloc){
    # Extract previous & current ecosystem state
    prev <- sim_binary[i-1,j]
    curr <- sim_binary[i,j]
    
    # Previous stem density
    prev_density <- sim_cont[i-1,j]
    
    # If the state is the same, then randomly vary density
    if(prev == curr) curr_density <- rnorm(n = 1,
                                           mean = prev_density,
                                           sd = 5)
    
    # Adjustments to ensure we maintain bimodality
    if(curr == 0 & curr_density > 47) curr_density <- 47
    if(curr == 0 & curr_density < 1) curr_density <- 1
    if(curr == 1 & curr_density < 150) curr_density <- 150
    
    # If there was a state shift, create a new stem density
    if(prev != curr){
      # If the state is savanna, draw from savanna distribution
      if(curr == 0) curr_density <- runif(n = 1,
                                          min = 1,
                                          max = 47)
      # If the state is forest, draw from forest distribution
      if(curr == 1) curr_density <- rnorm(n = 1,
                                          mean = 250,
                                          sd = 75)
    }
    
    # Update matrix
    sim_cont[i,j] <- curr_density
  }
}

#### 4. Format & visualize stem density ####

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

#### 5. Environment evolution ####

## This is done in a separate step to reinforce the fact
## that the environment in no way actually explains change
## in the ecosystem or stem density

# Loop over each row (time step) after initial conditions
for(i in 2:ntime){
  # Loop over each column (location)
  for(j in 1:nloc){
    # Increasing pattern for variables 1 and 4
    # Very small changes or else you get really
    # large changes over 150 time steps
    # (temporal change far exceeds spatial change)
    x1_mat[i,j] <- x1_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    x4_mat[i,j] <- x4_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    # Random change for variables 2 and 3
    x2_mat[i,j] <- x2_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0,
                                         sd = 0.01)
    x3_mat[i,j] <- x3_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0,
                                         sd = 0.02)
    
    # No change in variable 5
    x5_mat[i,j] <- x5_mat[i-1,j]
  }
}

#### 6. Format & visualize environmental variables ####

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

#### 7. Save ####

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
     file = 'out/simulation/sim1.RData')
