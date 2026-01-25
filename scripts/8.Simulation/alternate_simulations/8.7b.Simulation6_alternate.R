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

# Gradient from 0 to 1 representing smooth left-right gradient
base_var <- seq(from = 0, to = 1, length.out = nloc)

# Add a small amount of random noise
var1 <- base_var + rnorm(n = length(base_var),
                         mean = 0,
                         sd = 0.1)

# Variable correlated with stem density
base_var2 <- scales::rescale(cont_response, to = c(0, 1))

# Add a small amount of random noise
var2 <- base_var2 + rnorm(n = length(base_var),
                          mean = 0,
                          sd = 0.1)

# Variables with more stark change between locations of
# initial ecosystem states
base_var3 <- c(rep(0, times = nloc/2),
               rep(1, times = nloc/2))

# Add random noise
var3 <- base_var3 + rnorm(n = length(base_var),
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

#### 4. Define relationship between environment and ecosystem ####

## The relationship is fit between the stem density and the
## environmental conditions
## I'm only fitting the "true" relationship with variables 2 and 3
## Basically variable 1 is being used as a "red herring"
true_rel <- lm(cont_response ~ var2 + var3)

# Extract true beta coefficients
true_beta <- unname(true_rel$coefficients)

#### 5. Process evolution ####

# Initialize matrices
sim_binary <- matrix(, nrow = ntime,
                     ncol = nloc)
sim_binary[1,] <- binary_response

sim_cont <- matrix(, nrow = ntime,
                   ncol = nloc)
sim_cont[1,] <- cont_response

x1_mat <- x2_mat <- x3_mat <-
  matrix(, nrow = ntime,
         ncol = nloc)
x1_mat[1,] <- var1
x2_mat[1,] <- var2
x3_mat[1,] <- var3

# Set seed again
set.seed(1)

# Loop over each row (time step) after initial conditions
for(i in 2:nrow(sim_binary)){
  # Loop over each column (location)
  for(j in 1:ncol(sim_binary)){
    #### Environmental change
    
    # Increasing pattern for variables 1 and 3
    x1_mat[i,j] <- x1_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    x3_mat[i,j] <- x3_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0.003,
                                         sd = 0.01)
    
    # Less change for variable 2
    x2_mat[i,j] <- x2_mat[i-1,j] + rnorm(n = 1,
                                         mean = 0,
                                         sd = 0.01)
    
    # Combine variables for prediction
    xs <- as.data.frame(cbind(x2_mat[i,j], x3_mat[i,j]))
    colnames(xs) <- c('var2', 'var3')
    
    #### Dependent change in stem density
    
    curr_density <- unname(predict(object = true_rel,
                                   newdata = xs)) +
      rnorm(n = 1, mean = 0, sd = 1)
      
    # Adjustments to maintain realistic stem densities
    if(curr_density < 1) curr_density <- 1
    #if(curr_density < 150 & curr_density > 100) curr_density <- 150
    
    # Assign ecosystem state
    if(curr_density < 50) curr <- 0
    if(curr_density >= 50) curr <- 1
    
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

# Add column names
colnames(x1_df) <- c('time', 'space', 'var1')
colnames(x2_df) <- c('time', 'space', 'var2')
colnames(x3_df) <- c('time', 'space', 'var3')

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
                   by = c('time', 'space')) |>
  dplyr::full_join(y = x3_df,
                   by = c('time', 'space'))

# Save
save(simulations,
     file = 'out/simulation/sim6_alt.RData')
