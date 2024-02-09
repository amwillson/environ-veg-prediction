# Aggregate spatially
## Instead of doing what you're doing, try just making
## an array that's evenly spaced at 8 km intervals across
## the entire data extent (dimensions from bounding box)

## Then match the df_all coords to the evenly spaced coords
## need to figure out what to do with edges. probably cut-off for
## how close a point should be to be considered
## then to a NON-weighted average over all the points to aggregate
rm(list = ls())

# Loisad data for one experiment
load('data/raw/gssurgo_average_030_3000m.RData')

# Delete data for individual states
rm(df_IL, df_IN, df_MI, df_MN, df_WI)

# Remove outliers
df_all <- df_all |>
  dplyr::filter(claytotal_r >= 0 & claytotal_r <= 100 &
                  sandtotal_r >= 0 & sandtotal_r <= 100 &
                  silttotal_r >= 0 & silttotal_r <= 100)

# Make into sf object
df_all <- sf::st_as_sf(df_all, coords = c('x', 'y'))

# Add coordinate system
sf::st_crs(df_all) <- 'EPSG:4326'

# Check coordinate system -- should say EPSG 4326
sf::st_crs(df_all)

# Check by plotting
# Map of study area
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))

states <- sf::st_transform(states, crs = 'EPSG:4326')

# Simple plot of sand over the study area
ggplot2::ggplot(states) +
  ggplot2::geom_sf(data = df_all, ggplot2::aes(color = sandtotal_r)) +
  ggplot2::geom_sf(color = 'black', fill = NA, linewidth = 1.1)

# Convert from latitude/longitude to easting/northing
df_all1 <- sf::st_transform(df_all, crs = 'EPSG:26916')

# Convert the "states" data frame to the same coordinate system
states1 <- sf::st_transform(states, crs = 'EPSG:26916')

# Check plot again
ggplot2::ggplot() +
  ggplot2::geom_sf(data = df_all, ggplot2::aes(color = sandtotal_r)) +
  ggplot2::geom_sf(data = states1, color = 'black', fill = NA, linewidth = 1.1)

# If it looks the same, continue

# Convert back to regular data frame
df_all1 <- sfheaders::sf_to_df(df_all1, fill = T)
df_all <- sfheaders::sf_to_df(df_all, fill = T)

# Combine
full_df <- df_all |>
  dplyr::full_join(df_all1, by = c('claytotal_r', 'sandtotal_r',
                                   'silttotal_r', 'sfg_id', 'point_id'))

# Replace column names
colnames(full_df) <- c('clay', 'sand', 'silt', 'sfg_id', 'point_id',
                       'long', 'lat', 'x', 'y')

full_df <- full_df |> dplyr::select(-c(sfg_id, point_id))

# Find minimum and maximum easting & northing
min_x <- min(full_df$x)
max_x <- max(full_df$x)
min_y <- min(full_df$y)
max_y <- max(full_df$y)

# Interpolate between min & max
# by = 64000 for 8x8 km grid so 8^2 = 64 km
xs <- seq(from = min_x, to = max_x, by = 64000)
ys <- seq(from = min_y, to = max_y, by = 64000)

points <- expand.grid(xs = xs, ys = ys)

# Make spatial
points <- sf::st_as_sf(points, coords = c('xs', 'ys'))

# Make data spatial again
full_df <- sf::st_as_sf(full_df, coords = c('x', 'y'))

# Find closest points
closest <- sf::st_distance(x = points, y = full_df)

# Find minimum distance
# Looking for closest grid point to each data point
# Length should be nrow(data)
mins <- apply(closest, 2, which.min)

# Add to data
full_df <- sfheaders::sf_to_df(full_df, fill = T)
full_df <- dplyr::select(full_df, -c(sfg_id, point_id))
full_df$point_id <- mins

# Make spatial points into regular dataframe again
points <- sfheaders::sf_to_df(points, fill = T)
points <- dplyr::select(points, -sfg_id)

# Summarize
df_aggregate <- full_df |>
  dplyr::group_by(point_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   median_x = median(x),
                   median_y = median(y),
                   middle_long = median(long),
                   middle_lat = median(lat))

df_aggregate <- df_aggregate |>
  dplyr::left_join(points, by = 'point_id')

sf_aggregate_xy <- sf::st_as_sf(df_aggregate, coords = c('x', 'y'))
sf_aggregate_longlat <- sf::st_as_sf(df_aggregate, coords = c('middle_long', 'middle_lat'))
sf_aggregate_xy_orig <- sf::st_as_sf(df_aggregate, coords = c('median_x', 'median_y'))

sf::st_crs(sf_aggregate_xy) <- 'EPSG:26916'
sf::st_crs(sf_aggregate_longlat) <- 'EPSG:4326'
sf::st_crs(sf_aggregate_xy_orig) <- 'EPSG:26916'

# Plot to make sure values look correct
ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = sf_aggregate_longlat, ggplot2::aes(color = sand))


# Plot to make sure points are consistent
ggplot2::ggplot() +
  ggplot2::geom_sf(data = states1, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = sf_aggregate_xy, color = 'red') +
  ggplot2::geom_sf(data = sf_aggregate_xy_orig, color = 'blue')
