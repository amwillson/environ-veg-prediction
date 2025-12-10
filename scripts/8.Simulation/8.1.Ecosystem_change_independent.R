#### STEP 1: Basic ecosystem state evolution

## Change in ecosystem state is not depedent on the environment
## Compatbile with all simulations except those where
## environment drives ecosystem state directly

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

#### 2. Process evolution ####

# Initialize matrix
sim_binary <- matrix(, nrow = ntime,
                     ncol = nloc)
sim_binary[1,] <- binary_response

# Reset seed
set.seed(21)

# Loop over each row (time step) after initial conditions
for(i in 2:ntime){
  # Loop over each column (location)
  for(j in 1:nloc){
    # Take adjacent locations from the previous time step,
    # accounting for edges at j = 1 and j = nloc
    if(j == 1) prev <- sim_binary[i-1,c(j,j+1)]
    if(!(j %in% c(1, nloc))) prev <- sim_binary[i-1,c(j-1,j,j+1)]
    if(j == nloc) prev <- sim_binary[i-1,c(j-1,j)]
    
    # Determine current ecosystem state
    if(length(unique(prev)) == 1){
      # If all adjacent cells are 0, then new step is 0
      if(unique(prev) == 0) curr <- 0
      # If all adjacent cells are 1, then new step is 1
      if(unique(prev) == 1) curr <- 1
    }
    
    # If adjacent cells are a mix then the new step is a 50/50 chance
    # of staying the same state vs switching
    if(length(unique(prev)) > 1) curr <- sample(x = c(0, 1), size = 1)
    
    # Update the matrix
    sim_binary[i,j] <- curr
  }
}

#### 3. Formatting & visualization ####

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

#### 4. Save ####

# Save to use for changing stem density over time
save(sim_binary, file = 'out/simulation/state_change_ind.RData')
