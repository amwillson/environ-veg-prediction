## Match PLS grid to PLS points

rm(list = ls())

# Load gridded data
load('data/processed/PLS/8km.RData')

# Map of states
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Map of gridded centroids
comp_dens |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1)

# Load point data
load('data/processed/PLS/total.RData')

# Change CRS
ecosystem <- sf::st_as_sf(ecosystem, coords = c('x', 'y'),
                          crs = 'EPSG:4326')
ecosystem <- sf::st_transform(ecosystem, crs = 'EPSG:3175')
ecosystem <- sfheaders::sf_to_df(ecosystem, fill = TRUE)
ecosystem <- dplyr::select(ecosystem, -sfg_id, -point_id)

# Map of points
ecosystem |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ecosystem), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_color_discrete(type = c('darkgreen', 'yellow', 'lightgreen'))

# Divide point data into equal parts
ecosystem1 <- ecosystem[1:10000,]
ecosystem2 <- ecosystem[10001:20000,]
ecosystem3 <- ecosystem[20001:30000,]
ecosystem4 <- ecosystem[30001:40000,]
ecosystem5 <- ecosystem[40001:50000,]
ecosystem6 <- ecosystem[50001:60000,]
ecosystem7 <- ecosystem[60001:70000,]
ecosystem8 <- ecosystem[70001:80000,]
ecosystem9 <- ecosystem[80001:90000,]
ecosystem10 <- ecosystem[90001:100000,]
ecosystem11 <- ecosystem[100001:110000,]
ecosystem12 <- ecosystem[110001:120000,]
ecosystem13 <- ecosystem[120001:130000,]
ecosystem14 <- ecosystem[130001:140000,]
ecosystem15 <- ecosystem[140001:150000,]
ecosystem16 <- ecosystem[150001:160000,]
ecosystem17 <- ecosystem[160001:170000,]
ecosystem18 <- ecosystem[170001:180000,]
ecosystem19 <- ecosystem[180001:190000,]
ecosystem20 <- ecosystem[190001:200000,]
ecosystem21 <- ecosystem[200001:210000,]
ecosystem22 <- ecosystem[210001:220000,]
ecosystem23 <- ecosystem[220001:230000,]
ecosystem24 <- ecosystem[230001:240000,]
ecosystem25 <- ecosystem[240001:250000,]
ecosystem26 <- ecosystem[250001:260000,]
ecosystem27 <- ecosystem[260001:270000,]
ecosystem28 <- ecosystem[270001:280000,]
ecosystem29 <- ecosystem[280001:290000,]
ecosystem30 <- ecosystem[290001:300000,]
ecosystem31 <- ecosystem[300001:310000,]
ecosystem32 <- ecosystem[310001:320000,]
ecosystem33 <- ecosystem[320001:330000,]
ecosystem34 <- ecosystem[330001:340000,]
ecosystem35 <- ecosystem[340001:350000,]
ecosystem36 <- ecosystem[350001:360000,]
ecosystem37 <- ecosystem[360001:370000,]
ecosystem38 <- ecosystem[370001:380000,]
ecosystem39 <- ecosystem[380001:390000,]
ecosystem40 <- ecosystem[390001:400000,]
ecosystem41 <- ecosystem[400001:410000,]
ecosystem42 <- ecosystem[410001:420000,]
ecosystem43 <- ecosystem[420001:430000,]
ecosystem44 <- ecosystem[430001:440000,]
ecosystem45 <- ecosystem[440001:450000,]
ecosystem46 <- ecosystem[450001:460000,]
ecosystem47 <- ecosystem[460001:470000,]
ecosystem48 <- ecosystem[470001:480000,]
ecosystem49 <- ecosystem[480001:490000,]
ecosystem50 <- ecosystem[490001:500000,]
ecosystem51 <- ecosystem[500001:510000,]
ecosystem52 <- ecosystem[510001:520000,]
ecosystem53 <- ecosystem[520001:530000,]
ecosystem54 <- ecosystem[530001:540000,]
ecosystem55 <- ecosystem[540001:550000,]
ecosystem56 <- ecosystem[550001:560000,]
ecosystem57 <- ecosystem[560001:570000,]
ecosystem58 <- ecosystem[570001:580000,]
ecosystem59 <- ecosystem[580001:590000,]
ecosystem60 <- ecosystem[590001:600000,]
ecosystem61 <- ecosystem[600001:610000,]
ecosystem62 <- ecosystem[610001:620000,]
ecosystem63 <- ecosystem[620001:630000,]
ecosystem64 <- ecosystem[630001:640000,]
ecosystem65 <- ecosystem[640001:650000,]
ecosystem66 <- ecosystem[650001:660000,]
ecosystem67 <- ecosystem[660001:670000,]
ecosystem68 <- ecosystem[670001:680000,]
ecosystem69 <- ecosystem[680001:690000,]
ecosystem70 <- ecosystem[690001:691349,]

## Subset 1

# Coordinates
ecosystem1_xy <- dplyr::select(ecosystem1, x, y)
grid_xy <- dplyr::select(comp_dens, x, y)

# Distance
dists <- fields::rdist(ecosystem1_xy, grid_xy)
# Closest centroid to each point
closest_grid <- apply(dists, 1, which.min)

# Slice grid to match points
grid_slice <- dplyr::slice(comp_dens, closest_grid)

# Check to make sure all points fall within the grid cell with the closest centroid
# (the only time this should not happen is at state boundaries)
test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem1, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with Lake Michigan and we can't do anything about that

save_point_grid <- test_df

rm(dists, ecosystem1, ecosystem1_xy, test, test_df, test_slice)

## Subset 2

ecosystem2_xy <- dplyr::select(ecosystem2, x, y)

dists <- fields::rdist(ecosystem2_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem2, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem2, ecosystem2_xy, test, test_df, test_slice)

## Subset 3

ecosystem3_xy <- dplyr::select(ecosystem3, x, y)

dists <- fields::rdist(ecosystem3_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem3, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem3, ecosystem3_xy, test, test_df, test_slice)

## Subset 4

ecosystem4_xy <- dplyr::select(ecosystem4, x, y)

dists <- fields::rdist(ecosystem4_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem4, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem4, ecosystem4_xy, test, test_df, test_slice)

## Subset 5

ecosystem5_xy <- dplyr::select(ecosystem5, x, y)

dists <- fields::rdist(ecosystem5_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem5, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem5, ecosystem5_xy, test, test_df, test_slice)

## Subset 6

ecosystem6_xy <- dplyr::select(ecosystem6, x, y)

dists <- fields::rdist(ecosystem6_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem6, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid extent

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem6, ecosystem6_xy, test, test_df, test_slice)

## Subset 7

ecosystem7_xy <- dplyr::select(ecosystem7, x, y)

dists <- fields::rdist(ecosystem7_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem7, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem7, ecosystem7_xy, test, test_df, test_slice)

## Subset 8

ecosystem8_xy <- dplyr::select(ecosystem8, x, y)

dists <- fields::rdist(ecosystem8_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem8, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem8, ecosystem8_xy, test, test_df, test_slice)

## Subset 9

ecosystem9_xy <- dplyr::select(ecosystem9, x, y)

dists <- fields::rdist(ecosystem9_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem9, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem9, ecosystem9_xy, test, test_df, test_slice)

## Subset 10

ecosystem10_xy <- dplyr::select(ecosystem10, x, y)

dists <- fields::rdist(ecosystem10_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem10, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem10, ecosystem10_xy, test, test_df, test_slice)

## Subset 11

ecosystem11_xy <- dplyr::select(ecosystem11, x, y)

dists <- fields::rdist(ecosystem11_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem11, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem11, ecosystem11_xy, test, test_df, test_slice)

## Subset 12

ecosystem12_xy <- dplyr::select(ecosystem12, x, y)

dists <- fields::rdist(ecosystem12_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem12, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem12, ecosystem12_xy, test, test_df, test_slice)

## Subset 13

ecosystem13_xy <- dplyr::select(ecosystem13, x, y)

dists <- fields::rdist(ecosystem13_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem13, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem13, ecosystem13_xy, test, test_df, test_slice)

## Subset 14

ecosystem14_xy <- dplyr::select(ecosystem14, x, y)

dists <- fields::rdist(ecosystem14_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem14, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem14, ecosystem14_xy, test, test_df, test_slice)

## Subset 15

ecosystem15_xy <- dplyr::select(ecosystem15, x, y)

dists <- fields::rdist(ecosystem15_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem15, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem15, ecosystem15_xy, test, test_df, test_slice)

## Subset 16

ecosystem16_xy <- dplyr::select(ecosystem16, x, y)

dists <- fields::rdist(ecosystem16_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem16, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because thye're at the boundary of one of the great lakes or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem16, ecosystem16_xy, test, test_df, test_slice)

## Subset 17

ecosystem17_xy <- dplyr::select(ecosystem17, x, y)

dists <- fields::rdist(ecosystem17_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem17, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with great lakes

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem17, ecosystem17_xy, test, test_df, test_slice)

## Subset 18

ecosystem18_xy <- dplyr::select(ecosystem18, x, y)

dists <- fields::rdist(ecosystem18_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem18, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem18, ecosystem18_xy, test, test_df, test_slice)

## Subset 19

ecosystem19_xy <- dplyr::select(ecosystem19, x, y)

dists <- fields::rdist(ecosystem19_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem19, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at teh boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem19, ecosystem19_xy, test, test_df, test_slice)

## Subset 20

ecosystem20_xy <- dplyr::select(ecosystem20, x, y)

dists <- fields::rdist(ecosystem20_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem20, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem20, ecosystem20_xy, test, test_df, test_slice)

## Subset 21

ecosystem21_xy <- dplyr::select(ecosystem21, x, y)

dists <- fields::rdist(ecosystem21_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem21, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem21, ecosystem21_xy, test, test_df, test_slice)

## Subset 22

ecosystem22_xy <- dplyr::select(ecosystem22, x, y)

dists <- fields::rdist(ecosystem22_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem22, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cells

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem22, ecosystem22_xy, test, test_df, test_slice)

## Subset 23

ecosystem23_xy <- dplyr::select(ecosystem23, x, y)

dists <- fields::rdist(ecosystem23_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem23, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're on the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem23, ecosystem23_xy, test, test_df, test_slice)

## Subset 24

ecosystem24_xy <- dplyr::select(ecosystem24, x, y)

dists <- fields::rdist(ecosystem24_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem24, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at teh boundary with lake superior or canada

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem24, ecosystem24_xy, test, test_df, test_slice)

## Subset 25

ecosystem25_xy <- dplyr::select(ecosystem25, x, y)

dists <- fields::rdist(ecosystem25_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem25, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem25, ecosystem25_xy, test, test_df, test_slice)

## Subset 26

ecosystem26_xy <- dplyr::select(ecosystem26, x, y)

dists <- fields::rdist(ecosystem26_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem26, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem26, ecosystem26_xy, test, test_df, test_slice)

## Subset 27

ecosystem27_xy <- dplyr::select(ecosystem27, x, y)

dists <- fields::rdist(ecosystem27_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem27, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem27, ecosystem27_xy, test, test_df, test_slice)

## Subset 28

ecosystem28_xy <- dplyr::select(ecosystem28, x, y)

dists <- fields::rdist(ecosystem28_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem28, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem28, ecosystem28_xy, test, test_df, test_slice)

## Subset 29

ecosystem29_xy <- dplyr::select(ecosystem29, x, y)

dists <- fields::rdist(ecosystem29_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem29, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boudnary with canada or with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem29, ecosystem29_xy, test, test_df, test_slice)

## Subset 30

ecosystem30_xy <- dplyr::select(ecosystem30, x, y)

dists <- fields::rdist(ecosystem30_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem30, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with canada or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem30, ecosystem30_xy, test, test_df, test_slice)

## Subset 31

ecosystem31_xy <- dplyr::select(ecosystem31, x, y)

dists <- fields::rdist(ecosystem31_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem31, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake superior, canada or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem31, ecosystem31_xy, test, test_df, test_slice)

## Subset 32

ecosystem32_xy <- dplyr::select(ecosystem32, x, y)

dists <- fields::rdist(ecosystem32_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem32, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem32, ecosystem32_xy, test, test_df, test_slice)

## Subset 33

ecosystem33_xy <- dplyr::select(ecosystem33, x, y)

dists <- fields::rdist(ecosystem33_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem33, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem33, ecosystem33_xy, test, test_df, test_slice)

## Subset 34

ecosystem34_xy <- dplyr::select(ecosystem34, x, y)

dists <- fields::rdist(ecosystem34_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem34, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at teh boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem34, ecosystem34_xy, test, test_df, test_slice)

## Subset 35

ecosystem35_xy <- dplyr::select(ecosystem35, x, y)

dists <- fields::rdist(ecosystem35_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem35, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem35, ecosystem35_xy, test, test_df, test_slice)

## Subset 36

ecosystem36_xy <- dplyr::select(ecosystem36, x, y)

dists <- fields::rdist(ecosystem36_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem36, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem36, ecosystem36_xy, test, test_df, test_slice)

## Subset 37

ecosystem37_xy <- dplyr::select(ecosystem37, x, y)

dists <- fields::rdist(ecosystem37_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem37, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem37, ecosystem37_xy, test, test_df, test_slice)

## Subset 38

ecosystem38_xy <- dplyr::select(ecosystem38, x, y)

dists <- fields::rdist(ecosystem38_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem38, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with canada or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem38, ecosystem38_xy, test, test_df, test_slice)

## Subset 39

ecosystem39_xy <- dplyr::select(ecosystem39, x, y)

dists <- fields::rdist(ecosystem39_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem39, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem39, ecosystem39_xy, test, test_df, test_slice)

## Subset 40

ecosystem40_xy <- dplyr::select(ecosystem40, x, y)

dists <- fields::rdist(ecosystem40_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem40, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with canada

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem40, ecosystem40_xy, test, test_df, test_slice)

## Subset 41

ecosystem41_xy <- dplyr::select(ecosystem41, x, y)

dists <- fields::rdist(ecosystem41_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem41, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake superior

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem41, ecosystem41_xy, test, test_df, test_slice)

## Subset 42

ecosystem42_xy <- dplyr::select(ecosystem42, x, y)

dists <- fields::rdist(ecosystem42_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem42, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're on the boundary with lake superior or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem42, ecosystem42_xy, test, test_df, test_slice)

## Subset 43

ecosystem43_xy <- dplyr::select(ecosystem43, x, y)

dists <- fields::rdist(ecosystem43_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem43, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem43, ecosystem43_xy, test, test_df, test_slice)

## Subset 44

ecosystem44_xy <- dplyr::select(ecosystem44, x, y)

dists <- fields::rdist(ecosystem44_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem44, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake superior or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem44, ecosystem44_xy, test, test_df, test_slice)

## Subset 45

ecosystem45_xy <- dplyr::select(ecosystem45, x, y)

dists <- fields::rdist(ecosystem45_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem45, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake superior

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem45, ecosystem45_xy, test, test_df, test_slice)

## Subset 46

ecosystem46_xy <- dplyr::select(ecosystem46, x, y)

dists <- fields::rdist(ecosystem46_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem46, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = mi, color = 'black', fill = NA)

# Accept all because they're on the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem46, ecosystem46_xy, test, test_df, test_slice)

## Subset 47

ecosystem47_xy <- dplyr::select(ecosystem47, x, y)

dists <- fields::rdist(ecosystem47_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem47, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem47, ecosystem47_xy, test, test_df, test_slice)

## Subset 48

ecosystem48_xy <- dplyr::select(ecosystem48, x, y)

dists <- fields::rdist(ecosystem48_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem48, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem48, ecosystem48_xy, test, test_df, test_slice)

## Subset 49

ecosystem49_xy <- dplyr::select(ecosystem49, x, y)

dists <- fields::rdist(ecosystem49_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem49, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem49, ecosystem49_xy, test, test_df, test_slice)

## Subset 50

ecosystem50_xy <- dplyr::select(ecosystem50, x, y)

dists <- fields::rdist(ecosystem50_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem50, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem50, ecosystem50_xy, test, test_df, test_slice)

## Subset 51

ecosystem51_xy <- dplyr::select(ecosystem51, x, y)

dists <- fields::rdist(ecosystem51_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem51, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem51, ecosystem51_xy, test, test_df, test_slice)

## Subset 52

ecosystem52_xy <- dplyr::select(ecosystem52, x, y)

dists <- fields::rdist(ecosystem52_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem52, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with a great lake

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem52, ecosystem52_xy, test, test_df, test_slice)

## Subset 53

ecosystem53_xy <- dplyr::select(ecosystem53, x, y)

dists <- fields::rdist(ecosystem53_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem53, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake superior

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem53, ecosystem53_xy, test, test_df, test_slice)

## Subset 54

ecosystem54_xy <- dplyr::select(ecosystem54, x, y)

dists <- fields::rdist(ecosystem54_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem54, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem54, ecosystem54_xy, test, test_df, test_slice)

## Subset 55

ecosystem55_xy <- dplyr::select(ecosystem55, x, y)

dists <- fields::rdist(ecosystem55_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem55, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem55, ecosystem55_xy, test, test_df, test_slice)

## Subset 56

ecosystem56_xy <- dplyr::select(ecosystem56, x, y)

dists <- fields::rdist(ecosystem56_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem56, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# No points outside grid cell bounds

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem56, ecosystem56_xy, test, test_df, test_slice)

## Subset 57

ecosystem57_xy <- dplyr::select(ecosystem57, x, y)

dists <- fields::rdist(ecosystem57_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem57, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem57, ecosystem57_xy, test, test_df, test_slice)

## Subset 58

ecosystem58_xy <- dplyr::select(ecosystem58, x, y)

dists <- fields::rdist(ecosystem58_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem58, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem58, ecosystem58_xy, test, test_df, test_slice)

## Subset 59

ecosystem59_xy <- dplyr::select(ecosystem59, x, y)

dists <- fields::rdist(ecosystem59_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem59, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem59, ecosystem59_xy, test, test_df, test_slice)

## Subset 60

ecosystem60_xy <- dplyr::select(ecosystem60, x, y)

dists <- fields::rdist(ecosystem60_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem60, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem60, ecosystem60_xy, test, test_df, test_slice)

## Subset 61

ecosystem61_xy <- dplyr::select(ecosystem61, x, y)

dists <- fields::rdist(ecosystem61_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem61, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem61, ecosystem61_xy, test, test_df, test_slice)

## Subset 62

ecosystem62_xy <- dplyr::select(ecosystem62, x, y)

dists <- fields::rdist(ecosystem62_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem62, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem62, ecosystem62_xy, test, test_df, test_slice)

## Subset 63

ecosystem63_xy <- dplyr::select(ecosystem63, x, y)

dists <- fields::rdist(ecosystem63_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem63, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem63, ecosystem63_xy, test, test_df, test_slice)

## Subset 64

ecosystem64_xy <- dplyr::select(ecosystem64, x, y)

dists <- fields::rdist(ecosystem64_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem64, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem64, ecosystem64_xy, test, test_df, test_slice)

## Subset 65

ecosystem65_xy <- dplyr::select(ecosystem65, x, y)

dists <- fields::rdist(ecosystem65_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem65, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem65, ecosystem65_xy, test, test_df, test_slice)

## Subset 66

ecosystem66_xy <- dplyr::select(ecosystem66, x, y)

dists <- fields::rdist(ecosystem66_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem66, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem66, ecosystem66_xy, test, test_df, test_slice)

## Subset 67

ecosystem67_xy <- dplyr::select(ecosystem67, x, y)

dists <- fields::rdist(ecosystem67_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem67, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem67, ecosystem67_xy, test, test_df, test_slice)

## Subset 68

ecosystem68_xy <- dplyr::select(ecosystem68, x, y)

dists <- fields::rdist(ecosystem68_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem68, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're on the boundary with lake michigan or a state not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem68, ecosystem68_xy, test, test_df, test_slice)

## Subset 69

ecosystem69_xy <- dplyr::select(ecosystem69, x, y)

dists <- fields::rdist(ecosystem69_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem69, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan or with a sstate not within our domain

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem69, ecosystem69_xy, test, test_df, test_slice)

## Subset 70

ecosystem70_xy <- dplyr::select(ecosystem70, x, y)

dists <- fields::rdist(ecosystem70_xy, grid_xy)
closest_grid <- apply(dists, 1, which.min)

grid_slice <- dplyr::slice(comp_dens, closest_grid)

test_slice <- grid_slice |>
  dplyr::mutate(min_x = x - 4000,
                max_x = x + 4000,
                min_y = y - 4000,
                max_y = y + 4000) |>
  dplyr::rename(grid_x = x,
                grid_y = y)

test_df <- cbind(ecosystem70, test_slice)

test_df <- test_df |>
  dplyr::mutate(check_x = dplyr::if_else(x > min_x & x < max_x, TRUE, FALSE),
                check_y = dplyr::if_else(y > min_y & y < max_y, TRUE, FALSE),
                check = dplyr::if_else(check_x == TRUE & check_y == TRUE, TRUE, FALSE))

test <- test_df[which(test_df$check == FALSE),]

ggplot2::ggplot() +
  ggplot2::geom_point(data = test, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Accept all because they're at the boundary with lake michigan

save_point_grid <- rbind(save_point_grid, test_df)

rm(dists, ecosystem70, ecosystem70_xy, test, test_df, test_slice)

# Check to make sure our points make sense

save_point_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = grid_x)) +
  ggplot2::geom_abline()

save_point_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = y, y = grid_y)) +
  ggplot2::geom_abline()

# Add corresponding grid cell ID to total ecosystem dataframe
ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject taxon dataframe
taxon <- sf::st_as_sf(taxon, coords = c('x', 'y'),
                      crs = 'EPSG:4326')
taxon <- sf::st_transform(taxon, crs = 'EPSG:3175')
taxon <- sfheaders::sf_to_df(taxon, fill = TRUE)
taxon <- dplyr::select(taxon, -sfg_id, -point_id)

# Add corresponding grid cell ID to total taxon dataframe
taxon <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = taxon, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject and save
ecosystem <- sf::st_as_sf(ecosystem, coords = c('x', 'y'),
                          crs = 'EPSG:3175')
ecosystem <- sf::st_transform(ecosystem, crs = 'EPSG:4326')
ecosystem <- sfheaders::sf_to_df(ecosystem, fill = TRUE)
ecosystem <- dplyr::select(ecosystem, -sfg_id, -point_id)

taxon <- sf::st_as_sf(taxon, coords = c('x', 'y'),
                      crs = 'EPSG:3175')
taxon <- sf::st_transform(taxon, crs = 'EPSG:4326')
taxon <- sfheaders::sf_to_df(taxon, fill = TRUE)
taxon <- dplyr::select(taxon, -sfg_id, -point_id)

save(ecosystem, taxon, file = 'data/processed/PLS/total_matched.RData')

## Individual states

# Illinois
load('data/processed/PLS/illinois_format.RData')

# Reproject
illinois <- sf::st_as_sf(illinois, coords = c('x', 'y'),
                         crs = 'EPSG:4326')
illinois <- sf::st_transform(illinois, crs = 'EPSG:3175')
illinois <- sfheaders::sf_to_df(illinois, fill = TRUE)
illinois <- dplyr::select(illinois, -sfg_id, -point_id)

illinois_ecosystem <- sf::st_as_sf(illinois_ecosystem, coords = c('x', 'y'),
                                   crs = 'EPSG:4326')
illinois_ecosystem <- sf::st_transform(illinois_ecosystem, crs = 'EPSG:3175')
illinois_ecosystem <- sfheaders::sf_to_df(illinois_ecosystem, fill = TRUE)
illinois_ecosystem <- dplyr::select(illinois_ecosystem, -sfg_id, -point_id)

# Add corresponding grid cell ID to each dataframe
illinois <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = illinois, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

illinois_ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = illinois_ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject again and save
illinois <- sf::st_as_sf(illinois, coords = c('x', 'y'),
                         crs = 'EPSG:3175')
illinois <- sf::st_transform(illinois, crs = 'EPSG:4326')
illinois <- sfheaders::sf_to_df(illinois, fill = TRUE)
illinois <- dplyr::select(illinois, -sfg_id, -point_id)

illinois_ecosystem <- sf::st_as_sf(illinois_ecosystem, coords = c('x', 'y'),
                                   crs = 'EPSG:3175')
illinois_ecosystem <- sf::st_transform(illinois_ecosystem, crs = 'EPSG:4326')
illinois_ecosystem <- sfheaders::sf_to_df(illinois_ecosystem, fill = TRUE)
illinois_ecosystem <- dplyr::select(illinois_ecosystem, -sfg_id, -point_id)

save(illinois, illinois_ecosystem, file = 'data/processed/PLS/illinois_matched.RData')

# Indiana
load('data/processed/PLS/indiana_format.RData')

# Reproject
indiana <- sf::st_as_sf(indiana, coords = c('x', 'y'),
                        crs = 'EPSG:4326')
indiana <- sf::st_transform(indiana, crs = 'EPSG:3175')
indiana <- sfheaders::sf_to_df(indiana, fill = TRUE)
indiana <- dplyr::select(indiana, -sfg_id, -point_id)

indiana_ecosystem <- sf::st_as_sf(indiana_ecosystem, coords = c('x', 'y'),
                                  crs = 'EPSG:4326')
indiana_ecosystem <- sf::st_transform(indiana_ecosystem, crs = 'EPSG:3175')
indiana_ecosystem <- sfheaders::sf_to_df(indiana_ecosystem, fill = TRUE)
indiana_ecosystem <- dplyr::select(indiana_ecosystem, -sfg_id, -point_id)

# Add corresponding grid cell ID to each dataframe
indiana <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = indiana, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

indiana_ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = indiana_ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject again and save
indiana <- sf::st_as_sf(indiana, coords = c('x', 'y'),
                        crs = 'EPSG:3175')
indiana <- sf::st_transform(indiana, crs = 'EPSG:4326')
indiana <- sfheaders::sf_to_df(indiana, fill = TRUE)
indiana <- dplyr::select(indiana, -sfg_id, -point_id)

indiana_ecosystem <- sf::st_as_sf(indiana_ecosystem, coords = c('x', 'y'),
                                  crs = 'EPSG:3175')
indiana_ecosystem <- sf::st_transform(indiana_ecosystem, crs = 'EPSG:4326')
indiana_ecosystem <- sfheaders::sf_to_df(indiana_ecosystem, fill = TRUE)
indiana_ecosystem <- dplyr::select(indiana_ecosystem, -sfg_id, -point_id)

save(indiana, indiana_ecosystem, file = 'data/processed/PLS/indiana_matched.RData')

# Michigan
load('data/processed/PLS/lowmichigan_process.RData')

# Reproject
lowmichigan <- sf::st_as_sf(lowmichigan, coords = c('x', 'y'),
                            crs = 'EPSG:4326')
lowmichigan <- sf::st_transform(lowmichigan, crs = 'EPSG:3175')
lowmichigan <- sfheaders::sf_to_df(lowmichigan, fill = TRUE)
lowmichigan <- dplyr::select(lowmichigan, -sfg_id, -point_id)

lowmichigan_ecosystem <- sf::st_as_sf(lowmichigan_ecosystem, coords = c('x', 'y'),
                                      crs = 'EPSG:4326')
lowmichigan_ecosystem <- sf::st_transform(lowmichigan_ecosystem, crs = 'EPSG:3175')
lowmichigan_ecosystem <- sfheaders::sf_to_df(lowmichigan_ecosystem, fill = TRUE)
lowmichigan_ecosystem <- dplyr::select(lowmichigan_ecosystem, -sfg_id, -point_id)

# Add corresponding grid cell ID to each dataframe
lowmichigan <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = lowmichigan, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

lowmichigan_ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = lowmichigan_ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject again and save
lowmichigan <- sf::st_as_sf(lowmichigan, coords = c('x', 'y'),
                            crs = 'EPSG:3175')
lowmichigan <- sf::st_transform(lowmichigan, crs = 'EPSG:4326')
lowmichigan <- sfheaders::sf_to_df(lowmichigan, fill = TRUE)
lowmichigan <- dplyr::select(lowmichigan, -sfg_id, -point_id)

lowmichigan_ecosystem <- sf::st_as_sf(lowmichigan_ecosystem, coords = c('x', 'y'),
                                      crs = 'EPSG:3175')
lowmichigan_ecosystem <- sf::st_transform(lowmichigan_ecosystem, crs = 'EPSG:4326')
lowmichigan_ecosystem <- sfheaders::sf_to_df(lowmichigan_ecosystem, fill = TRUE)
lowmichigan_ecosystem <- dplyr::select(lowmichigan_ecosystem, -sfg_id, -point_id)

save(lowmichigan, lowmichigan_ecosystem, file = 'data/processed/PLS/lowmichigan_matched.RData')

# Reproject

load('data/processed/PLS/upmichigan_process.RData')

upmichigan <- sf::st_as_sf(upmichigan, coords = c('x', 'y'),
                           crs = 'EPSG:4326')
upmichigan <- sf::st_transform(upmichigan, crs = 'EPSG:3175')
upmichigan <- sfheaders::sf_to_df(upmichigan, fill = TRUE)
upmichigan <- dplyr::select(upmichigan, -sfg_id, -point_id)

upmichigan_ecosystem <- sf::st_as_sf(upmichigan_ecosystem, coords = c('x', 'y'),
                                     crs = 'EPSG:4326')
upmichigan_ecosystem <- sf::st_transform(upmichigan_ecosystem, crs = 'EPSG:3175')
upmichigan_ecosystem <- sfheaders::sf_to_df(upmichigan_ecosystem, fill = TRUE)
upmichigan_ecosystem <- dplyr::select(upmichigan_ecosystem, -sfg_id, -point_id)

# Add corresponding grid cell ID to each dataframe
upmichigan <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = upmichigan, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

upmichigan_ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = upmichigan_ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject again and save
upmichigan <- sf::st_as_sf(upmichigan, coords = c('x', 'y'),
                           crs = 'EPSG:3175')
upmichigan <- sf::st_transform(upmichigan, crs = 'EPSG:4326')
upmichigan <- sfheaders::sf_to_df(upmichigan, fill = TRUE)
upmichigan <- dplyr::select(upmichigan, -sfg_id, -point_id)

upmichigan_ecosystem <- sf::st_as_sf(upmichigan_ecosystem, coords = c('x', 'y'),
                                     crs = 'EPSG:3175')
upmichigan_ecosystem <- sf::st_transform(upmichigan_ecosystem, crs = 'EPSG:4326')
upmichigan_ecosystem <- sfheaders::sf_to_df(upmichigan_ecosystem, fill = TRUE)
upmichigan_ecosystem <- dplyr::select(upmichigan_ecosystem, -sfg_id, -point_id)

save(upmichigan, upmichigan_ecosystem, file = 'data/processed/PLS/upmichigan_matched.RData')

# Minnesota
load('data/processed/PLS/minnesota_process.RData')

# Reproject
minnesota <- sf::st_as_sf(minnesota, coords = c('x', 'y'),
                          crs = 'EPSG:4326')
minnesota <- sf::st_transform(minnesota, crs = 'EPSG:3175')
minnesota <- sfheaders::sf_to_df(minnesota, fill = TRUE)
minnesota <- dplyr::select(minnesota, -sfg_id, -point_id)

minnesota_ecosystem <- sf::st_as_sf(minnesota_ecosystem, coords = c('x', 'y'),
                                    crs = 'EPSG:4326')
minnesota_ecosystem <- sf::st_transform(minnesota_ecosystem, crs = 'EPSG:3175')
minnesota_ecosystem <- sfheaders::sf_to_df(minnesota_ecosystem, fill = TRUE)
minnesota_ecosystem <- dplyr::select(minnesota_ecosystem, -sfg_id, -point_id)

# Add corresponding grid cell ID to each dataframe
minnesota <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = minnesota, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

minnesota_ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = minnesota_ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject again and save
minnesota <- sf::st_as_sf(minnesota, coords = c('x', 'y'),
                          crs = 'EPSG:3175')
minnesota <- sf::st_transform(minnesota, crs = 'EPSG:4326')
minnesota <- sfheaders::sf_to_df(minnesota, fill = TRUE)
minnesota <- dplyr::select(minnesota, -sfg_id, -point_id)

minnesota_ecosystem <- sf::st_as_sf(minnesota_ecosystem, coords = c('x', 'y'),
                                    crs = 'EPSG:3175')
minnesota_ecosystem <- sf::st_transform(minnesota_ecosystem, crs = 'EPSG:4326')
minnesota_ecosystem <- sfheaders::sf_to_df(minnesota_ecosystem, fill = TRUE)
minnesota_ecosystem <- dplyr::select(minnesota_ecosystem, -sfg_id, -point_id)

save(minnesota, minnesota_ecosystem, file = 'data/processed/PLS/minnesota_matched.RData')

# Wisconsin
load('data/processed/PLS/wisconsin_process.RData')

# Reproject
wisconsin <- sf::st_as_sf(wisconsin, coords = c('x', 'y'),
                          crs = 'EPSG:4326')
wisconsin <- sf::st_transform(wisconsin, crs = 'EPSG:3175')
wisconsin <- sfheaders::sf_to_df(wisconsin, fill = TRUE)
wisconsin <- dplyr::select(wisconsin, -sfg_id, -point_id)

wisconsin_ecosystem <- sf::st_as_sf(wisconsin_ecosystem, coords = c('x', 'y'),
                                    crs = 'EPSG:4326')
wisconsin_ecosystem <- sf::st_transform(wisconsin_ecosystem, crs = 'EPSG:3175')
wisconsin_ecosystem <- sfheaders::sf_to_df(wisconsin_ecosystem, fill = TRUE)
wisconsin_ecosystem <- dplyr::select(wisconsin_ecosystem, -sfg_id, -point_id)

# Add corresponding grid cell ID to each dataframe
wisconsin <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = wisconsin, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

wisconsin_ecosystem <- save_point_grid |>
  dplyr::select(x, y, id) |>
  dplyr::right_join(y = wisconsin_ecosystem, by = c('x', 'y')) |>
  dplyr::rename(grid_ID = id)

# Reproject again and save
wisconsin <- sf::st_as_sf(wisconsin, coords = c('x', 'y'),
                          crs = 'EPSG:3175')
wisconsin <- sf::st_transform(wisconsin, crs = 'EPSG:4326')
wisconsin <- sfheaders::sf_to_df(wisconsin, fill = TRUE)
wisconsin <- dplyr::select(wisconsin, -sfg_id, -point_id)

wisconsin_ecosystem <- sf::st_as_sf(wisconsin_ecosystem, coords = c('x', 'y'),
                                    crs = 'EPSG:3175')
wisconsin_ecosystem <- sf::st_transform(wisconsin_ecosystem, crs = 'EPSG:4326')
wisconsin_ecosystem <- sfheaders::sf_to_df(wisconsin_ecosystem, fill = TRUE)
wisconsin_ecosystem <- dplyr::select(wisconsin_ecosystem, -sfg_id, -point_id)

save(wisconsin, wisconsin_ecosystem, file = 'data/processed/PLS/wisconsin_matched.RData')