## Exploring FIA data

rm(list = ls())

# Load plot-level data
IL_plot <- readr::read_csv('data/raw/FIA/IL_PLOT.csv')
IN_plot <- readr::read_csv('data/raw/FIA/IN_PLOT.csv')
MI_plot <- readr::read_csv('data/raw/FIA/MI_PLOT.csv')
MN_plot <- readr::read_csv('data/raw/FIA/MN_PLOT.csv')
WI_plot <- readr::read_csv('data/raw/FIA/WI_PLOT.csv')

# Combine states
plot <- rbind(IL_plot, IN_plot, MI_plot, MN_plot, WI_plot)

# Clean up data frame
plot <- plot |>
  dplyr::select(CN, # unique sequence number for each plot record
                INVYR, # inventory year
                STATECD, # state code
                PLOT, # unique identifier for each plot
                MEASYEAR, # measurement year (can be different from inventory year?)
                LAT, LON, # NAD 83- within ~ +/- 1 mile of actual plot
                QA_STATUS # make sure not > 1
                )

# Add coordinates
plot <- sf::st_as_sf(plot, coords = c('LON', 'LAT'))

# Add current projection
sf::st_crs(plot) <- 'EPSG:4269'

# Reproject to EPSG 4326
plot <- sf::st_transform(plot, crs = 'EPSG:4326')

# Convert back to regular data frame
plot <- sfheaders::sf_to_df(plot, fill = TRUE)

# Remove unnecessary columns
plot <- dplyr::select(plot, -c(sfg_id, point_id))

# Plot of states
states <- sf::st_as_sf(maps::map('state', region = c('indiana', 'illinois', 'michigan',
                                                     'minnesota', 'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Plot approximate locations of plots
plot |>
  dplyr::group_by(PLOT) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()
