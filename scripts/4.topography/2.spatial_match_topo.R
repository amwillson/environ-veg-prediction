## Matching topography and PLS spatially

rm(list = ls())

#### ILLINOIS ####
load('data/raw/topography/il_topo.RData')

load('data/processed/PLS/illinois_format.RData')

# Remove negative points because we know land in illinois is not below sea level
il_topo <- dplyr::filter(il_topo, elevation >= 0)

# Format illinois data
illinois <- illinois |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls illinois into 20 sections
illinois1 <- illinois[1:2641,]
illinois2 <- illinois[2642:5283,]
illinois3 <- illinois[5284:7925,]
illinois4 <- illinois[7926:10567,]
illinois5 <- illinois[10568:13209,]
illinois6 <- illinois[13210:15851,]
illinois7 <- illinois[15852:18493,]
illinois8 <- illinois[18494:21135,]
illinois9 <- illinois[21136:23777,]
illinois10 <- illinois[23778:26419,]
illinois11 <- illinois[26420:29061,]
illinois12 <- illinois[29062:31703,]
illinois13 <- illinois[31704:34345,]
illinois14 <- illinois[34346:36987,]
illinois15 <- illinois[36988:39629,]
illinois16 <- illinois[39630:42271,]
illinois17 <- illinois[42272:44913,]
illinois18 <- illinois[44914:47555,]
illinois19 <- illinois[47556:50197,]
illinois20 <- illinois[50198:52839,]
illinois21 <- illinois[52840:55481,]
illinois22 <- illinois[55482:58123,]
illinois23 <- illinois[58124:60765,]
illinois24 <- illinois[60766:63407,]
illinois25 <- illinois[63408:66049,]
illinois26 <- illinois[66050:68691,]
illinois27 <- illinois[68692:71333,]
illinois28 <- illinois[71334:73975,]
illinois29 <- illinois[73976:76617,]
illinois30 <- illinois[76618:79232,]

# Select coordinates
coords_illinois1 <- dplyr::select(illinois1, y, x)
coords_illinois2 <- dplyr::select(illinois2, y, x)
coords_illinois3 <- dplyr::select(illinois3, y, x)
coords_illinois4 <- dplyr::select(illinois4, y, x)
coords_illinois5 <- dplyr::select(illinois5, y, x)
coords_illinois6 <- dplyr::select(illinois6, y, x)
coords_illinois7 <- dplyr::select(illinois7, y, x)
coords_illinois8 <- dplyr::select(illinois8, y, x)
coords_illinois9 <- dplyr::select(illinois9, y, x)
coords_illinois10 <- dplyr::select(illinois10, y, x)
coords_illinois11 <- dplyr::select(illinois11, y, x)
coords_illinois12 <- dplyr::select(illinois12, y, x)
coords_illinois13 <- dplyr::select(illinois13, y, x)
coords_illinois14 <- dplyr::select(illinois14, y, x)
coords_illinois15 <- dplyr::select(illinois15, y, x)
coords_illinois16 <- dplyr::select(illinois16, y, x)
coords_illinois17 <- dplyr::select(illinois17, y, x)
coords_illinois18 <- dplyr::select(illinois18, y, x)
coords_illinois19 <- dplyr::select(illinois19, y, x)
coords_illinois20 <- dplyr::select(illinois20, y, x)
coords_illinois21 <- dplyr::select(illinois21, y, x)
coords_illinois22 <- dplyr::select(illinois22, y, x)
coords_illinois23 <- dplyr::select(illinois23, y, x)
coords_illinois24 <- dplyr::select(illinois24, y, x)
coords_illinois25 <- dplyr::select(illinois25, y, x)
coords_illinois26 <- dplyr::select(illinois26, y, x)
coords_illinois27 <- dplyr::select(illinois27, y, x)
coords_illinois28 <- dplyr::select(illinois28, y, x)
coords_illinois29 <- dplyr::select(illinois29, y, x)
coords_illinois30 <- dplyr::select(illinois30, y, x)

# Add column names
colnames(coords_illinois1) <- colnames(coords_illinois2) <- colnames(coords_illinois3) <-
  colnames(coords_illinois4) <- colnames(coords_illinois5) <- colnames(coords_illinois6) <-
  colnames(coords_illinois7) <- colnames(coords_illinois8) <- colnames(coords_illinois9) <- 
  colnames(coords_illinois10) <- colnames(coords_illinois11) <- colnames(coords_illinois12) <-
  colnames(coords_illinois13) <- colnames(coords_illinois14) <- colnames(coords_illinois15) <-
  colnames(coords_illinois16) <- colnames(coords_illinois17) <- colnames(coords_illinois18) <-
  colnames(coords_illinois19) <- colnames(coords_illinois20) <- colnames(coords_illinois21) <- 
  colnames(coords_illinois22) <- colnames(coords_illinois23) <- colnames(coords_illinois24) <-
  colnames(coords_illinois25) <- colnames(coords_illinois26) <- colnames(coords_illinois27) <-
  colnames(coords_illinois28) <- colnames(coords_illinois29) <- colnames(coords_illinois30) <-
  c('lat', 'long')

# Subset the topography data to the lat/long extent of each pls subsection
coords_df_IL1 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois1$long) - 0.1 & x <= max(coords_illinois1$long) + 0.1 &
                  y >= min(coords_illinois1$lat) - 0.1 & y <= max(coords_illinois1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL2 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois2$long) - 0.1 & x <= max(coords_illinois2$long) + 0.1 &
                  y >= min(coords_illinois2$lat) - 0.1 & y <= max(coords_illinois2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL3 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois3$long) - 0.1 & x <= max(coords_illinois3$long) + 0.1 &
                  y >= min(coords_illinois3$lat) - 0.1 & y <= max(coords_illinois3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL4 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois4$long) - 0.1 & x <= max(coords_illinois4$long) + 0.1 &
                  y >= min(coords_illinois4$lat) - 0.1 & y <= max(coords_illinois4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL5 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois5$long) - 0.1 & x <= max(coords_illinois5$long) + 0.1 &
                  y >= min(coords_illinois5$lat) - 0.1 & y <= max(coords_illinois5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL6 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois6$long) - 0.1 & x <= max(coords_illinois6$long) + 0.1 &
                  y >= min(coords_illinois6$lat) - 0.1 & y <= max(coords_illinois6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL7 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois7$long) - 0.1 & x <= max(coords_illinois7$long) + 0.1 &
                  y >= min(coords_illinois7$lat) - 0.1 & y <= max(coords_illinois7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL8 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois8$long) - 0.1 & x <= max(coords_illinois8$long) + 0.1 &
                  y >= min(coords_illinois8$lat) - 0.1 & y <= max(coords_illinois8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL9 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois9$long) - 0.1 & x <= max(coords_illinois9$long) + 0.1 &
                  y >= min(coords_illinois9$lat) - 0.1 & y <= max(coords_illinois9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL10 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois10$long) - 0.1 & x <= max(coords_illinois10$long) + 0.1 &
                  y >= min(coords_illinois10$lat) - 0.1 & y <= max(coords_illinois10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL11 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois11$long) - 0.1 & x <= max(coords_illinois11$long) + 0.1 &
                  y >= min(coords_illinois11$lat) - 0.1 & y <= max(coords_illinois11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL12 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois12$long) - 0.1 & x <= max(coords_illinois12$long) + 0.1 &
                  y >= min(coords_illinois12$lat) - 0.1 & y <= max(coords_illinois12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL13 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois13$long) - 0.1 & x <= max(coords_illinois13$long) + 0.1 &
                  y >= min(coords_illinois13$lat) - 0.1 & y <= max(coords_illinois13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL14 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois14$long) - 0.1 & x <= max(coords_illinois14$long) + 0.1 &
                  y >= min(coords_illinois14$lat) - 0.1 & y <= max(coords_illinois14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL15 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois15$long) - 0.1 & x <= max(coords_illinois15$long) + 0.1 &
                  y >= min(coords_illinois15$lat) - 0.1 & y <= max(coords_illinois15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL16 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois16$long) - 0.1 & x <= max(coords_illinois16$long) + 0.1 &
                  y >= min(coords_illinois16$lat) - 0.1 & y <= max(coords_illinois16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL17 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois17$long) - 0.1 & x <= max(coords_illinois17$long) + 0.1 &
                  y >= min(coords_illinois17$lat) - 0.1 & y <= max(coords_illinois17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL18 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois18$long) - 0.1 & x <= max(coords_illinois18$long) + 0.1 &
                  y >= min(coords_illinois18$lat) - 0.1 & y <= max(coords_illinois18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL19 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois19$long) - 0.1 & x <= max(coords_illinois19$long) + 0.1 &
                  y >= min(coords_illinois19$lat) - 0.1 & y <= max(coords_illinois19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL20 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois20$long) - 0.1 & x <= max(coords_illinois20$long) + 0.1 &
                  y >= min(coords_illinois20$lat) - 0.1 & y <= max(coords_illinois20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL21 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois21$long) - 0.1 & x <= max(coords_illinois21$long) + 0.1 &
                  y >= min(coords_illinois21$lat) - 0.1 & y <= max(coords_illinois21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL22 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois22$long) - 0.1 & x <= max(coords_illinois22$long) + 0.1 &
                  y >= min(coords_illinois22$lat) - 0.1 & y <= max(coords_illinois22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL23 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois23$long) - 0.1 & x <= max(coords_illinois23$long) + 0.1 &
                  y >= min(coords_illinois23$lat) - 0.1 & y <= max(coords_illinois23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL24 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois24$long) - 0.1 & x <= max(coords_illinois24$long) + 0.1 &
                  y >= min(coords_illinois24$lat) - 0.1 & y <= max(coords_illinois24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL25 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois25$long) - 0.1 & x <= max(coords_illinois25$long) + 0.1 &
                  y >= min(coords_illinois25$lat) - 0.1 & y <= max(coords_illinois25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL26 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois26$long) - 0.1 & x <= max(coords_illinois26$long) + 0.1 &
                  y >= min(coords_illinois26$lat) - 0.1 & y <= max(coords_illinois26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL27 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois27$long) - 0.1 & x <= max(coords_illinois27$long) + 0.1 &
                  y >= min(coords_illinois27$lat) - 0.1 & y <= max(coords_illinois27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL28 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois28$long) - 0.1 & x <= max(coords_illinois28$long) + 0.1 &
                  y >= min(coords_illinois28$lat) - 0.1 & y <= max(coords_illinois28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL29 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois29$long) - 0.1 & x <= max(coords_illinois29$long) + 0.1 &
                  y >= min(coords_illinois29$lat) - 0.1 & y <= max(coords_illinois29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IL30 <- il_topo |>
  dplyr::filter(x >= min(coords_illinois30$long) - 0.1 & x <= max(coords_illinois30$long) + 0.1 &
                  y >= min(coords_illinois30$lat) - 0.1 & y <= max(coords_illinois30$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_IL1, coords_illinois1)
# Find the  closest soil point to each PLS point 
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS points
# We'll add to this subsequently
select_df_IL <- il_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_illinois1$long) - 0.1 & x <= max(coords_illinois1$long) + 0.1 &
                  y >= min(coords_illinois1$lat) - 0.1 & y <= max(coords_illinois1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
IL_topo_pls <- cbind(select_df_IL, illinois1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_IL1, coords_illinois1, select_df_IL, illinois1)

# Repeat all these steps for the remaining 29 subsets

# Subset 2
dists <- fields::rdist(coords_df_IL2, coords_illinois2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois2$long) - 0.1 & x <= max(coords_illinois2$long) + 0.1 &
                  y >= min(coords_illinois2$lat) - 0.1 & y <= max(coords_illinois2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois2)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL2, coords_illinois2, select_df_IL, illinois2)

# Subset 3
dists <- fields::rdist(coords_df_IL3, coords_illinois3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois3$long) - 0.1 & x <= max(coords_illinois3$long) + 0.1 &
                  y >= min(coords_illinois3$lat) - 0.1 & y <= max(coords_illinois3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois3)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL3, coords_illinois3, select_df_IL, illinois3)

# Subset 4
dists <- fields::rdist(coords_df_IL4, coords_illinois4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois4$long) - 0.1 & x <= max(coords_illinois4$long) + 0.1 &
                  y >= min(coords_illinois4$lat) - 0.1 & y <= max(coords_illinois4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois4)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL4, coords_illinois4, select_df_IL, illinois4)

# Subset 5
dists <- fields::rdist(coords_df_IL5, coords_illinois5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois5$long) - 0.1 & x <= max(coords_illinois5$long) + 0.1 &
                  y >= min(coords_illinois5$lat) - 0.1 & y <= max(coords_illinois5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois5)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL5, coords_illinois5, select_df_IL, illinois5)

# Subset 6
dists <- fields::rdist(coords_df_IL6, coords_illinois6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois6$long) - 0.1 & x <= max(coords_illinois6$long) + 0.1 &
                  y >= min(coords_illinois6$lat) - 0.1 & y <= max(coords_illinois6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois6)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL6, coords_illinois6, select_df_IL, illinois6)

# Subset 7
dists <- fields::rdist(coords_df_IL7, coords_illinois7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois7$long) - 0.1 & x <= max(coords_illinois7$long) + 0.1 &
                  y >= min(coords_illinois7$lat) - 0.1 & y <= max(coords_illinois7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois7)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL7, coords_illinois7, select_df_IL, illinois7)

# Subset 8
dists <- fields::rdist(coords_df_IL8, coords_illinois8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois8$long) - 0.1 & x <= max(coords_illinois8$long) + 0.1 &
                  y >= min(coords_illinois8$lat) - 0.1 & y <= max(coords_illinois8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois8)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL8, coords_illinois8, select_df_IL, illinois8)

# Subset 9
dists <- fields::rdist(coords_df_IL9, coords_illinois9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois9$long) - 0.1 & x <= max(coords_illinois9$long) + 0.1 &
                  y >= min(coords_illinois9$lat) - 0.1 & y <= max(coords_illinois9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois9)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL9, coords_illinois9, select_df_IL, illinois9)

# Subset 10
dists <- fields::rdist(coords_df_IL10, coords_illinois10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois10$long) - 0.1 & x <= max(coords_illinois10$long) + 0.1 &
                  y >= min(coords_illinois10$lat) - 0.1 & y <= max(coords_illinois10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois10)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL10, coords_illinois10, select_df_IL, illinois10)

# Subset 11
dists <- fields::rdist(coords_df_IL11, coords_illinois11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois11$long) - 0.1 & x <= max(coords_illinois11$long) + 0.1 &
                  y >= min(coords_illinois11$lat) - 0.1 & y <= max(coords_illinois11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois11)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL11, coords_illinois11, select_df_IL, illinois11)

# Subset 12
dists <- fields::rdist(coords_df_IL12, coords_illinois12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois12$long) - 0.1 & x <= max(coords_illinois12$long) + 0.1 &
                  y >= min(coords_illinois12$lat) - 0.1 & y <= max(coords_illinois12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois12)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL12, coords_illinois12, select_df_IL, illinois12)

# Subset 13
dists <- fields::rdist(coords_df_IL13, coords_illinois13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois13$long) - 0.1 & x <= max(coords_illinois13$long) + 0.1 &
                  y >= min(coords_illinois13$lat) - 0.1 & y <= max(coords_illinois13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois13)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL13, coords_illinois13, select_df_IL, illinois13)

# Subset 14
dists <- fields::rdist(coords_df_IL14, coords_illinois14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois14$long) - 0.1 & x <= max(coords_illinois14$long) + 0.1 &
                  y >= min(coords_illinois14$lat) - 0.1 & y <= max(coords_illinois14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois14)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL14, coords_illinois14, select_df_IL, illinois14)

# Subset 15
dists <- fields::rdist(coords_df_IL15, coords_illinois15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois15$long) - 0.1 & x <= max(coords_illinois15$long) + 0.1 &
                  y >= min(coords_illinois15$lat) - 0.1 & y <= max(coords_illinois15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois15)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL15, coords_illinois15, select_df_IL, illinois15)

# Subset 16
dists <- fields::rdist(coords_df_IL16, coords_illinois16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois16$long) - 0.1 & x <= max(coords_illinois16$long) + 0.1 &
                  y >= min(coords_illinois16$lat) - 0.1 & y <= max(coords_illinois16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois16)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL16, coords_illinois16, select_df_IL, illinois16)

# Subset 17
dists <- fields::rdist(coords_df_IL17, coords_illinois17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois17$long) - 0.1 & x <= max(coords_illinois17$long) + 0.1 &
                  y >= min(coords_illinois17$lat) - 0.1 & y <= max(coords_illinois17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois17)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL17, coords_illinois17, select_df_IL, illinois17)

# Subset 18
dists <- fields::rdist(coords_df_IL18, coords_illinois18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois18$long) - 0.1 & x <= max(coords_illinois18$long) + 0.1 &
                  y >= min(coords_illinois18$lat) - 0.1 & y <= max(coords_illinois18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois18)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL18, coords_illinois18, select_df_IL, illinois18)

# Subset 19
dists <- fields::rdist(coords_df_IL19, coords_illinois19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois19$long) - 0.1 & x <= max(coords_illinois19$long) + 0.1 &
                  y >= min(coords_illinois19$lat) - 0.1 & y <= max(coords_illinois19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois19)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL19, coords_illinois19, select_df_IL, illinois19)

# Subset 20
dists <- fields::rdist(coords_df_IL20, coords_illinois20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois20$long) - 0.1 & x <= max(coords_illinois20$long) + 0.1 &
                  y >= min(coords_illinois20$lat) - 0.1 & y <= max(coords_illinois20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois20)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL20, coords_illinois20, select_df_IL, illinois20)

# Subset 21
dists <- fields::rdist(coords_df_IL21, coords_illinois21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois21$long) - 0.1 & x <= max(coords_illinois21$long) + 0.1 &
                  y >= min(coords_illinois21$lat) - 0.1 & y <= max(coords_illinois21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois21)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL21, coords_illinois21, select_df_IL, illinois21)

# Subset 22
dists <- fields::rdist(coords_df_IL22, coords_illinois22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois22$long) - 0.1 & x <= max(coords_illinois22$long) + 0.1 &
                  y >= min(coords_illinois22$lat) - 0.1 & y <= max(coords_illinois22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois22)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL22, coords_illinois22, select_df_IL, illinois22)

# Subset 23
dists <- fields::rdist(coords_df_IL23, coords_illinois23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois23$long) - 0.1 & x <= max(coords_illinois23$long) + 0.1 &
                  y >= min(coords_illinois23$lat) - 0.1 & y <= max(coords_illinois23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois23)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL23, coords_illinois23, select_df_IL, illinois23)

# Subset 24
dists <- fields::rdist(coords_df_IL24, coords_illinois24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois24$long) - 0.1 & x <= max(coords_illinois24$long) + 0.1 &
                  y >= min(coords_illinois24$lat) - 0.1 & y <= max(coords_illinois24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois24)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL24, coords_illinois24, select_df_IL, illinois24)

# Subset 25
dists <- fields::rdist(coords_df_IL25, coords_illinois25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois25$long) - 0.1 & x <= max(coords_illinois25$long) + 0.1 &
                  y >= min(coords_illinois25$lat) - 0.1 & y <= max(coords_illinois25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois25)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL25, coords_illinois25, select_df_IL, illinois25)

# Subset 26
dists <- fields::rdist(coords_df_IL26, coords_illinois26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois26$long) - 0.1 & x <= max(coords_illinois26$long) + 0.1 &
                  y >= min(coords_illinois26$lat) - 0.1 & y <= max(coords_illinois26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois26)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL26, coords_illinois26, select_df_IL, illinois26)

# Subset 27
dists <- fields::rdist(coords_df_IL27, coords_illinois27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois27$long) - 0.1 & x <= max(coords_illinois27$long) + 0.1 &
                  y >= min(coords_illinois27$lat) - 0.1 & y <= max(coords_illinois27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <-cbind(select_df_IL, illinois27)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL27, coords_illinois27, select_df_IL, illinois27)

# Subset 28
dists <- fields::rdist(coords_df_IL28, coords_illinois28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois28$long) - 0.1 & x <= max(coords_illinois28$long) + 0.1 &
                  y >= min(coords_illinois28$lat) - 0.1 & y <= max(coords_illinois28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois28)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL28, coords_illinois28, select_df_IL, illinois28)

# Subset 29
dists <- fields::rdist(coords_df_IL29, coords_illinois29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois29$long) - 0.1 & x <= max(coords_illinois29$long) + 0.1 &
                  y >= min(coords_illinois29$lat) - 0.1 & y <= max(coords_illinois29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois29)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL29, coords_illinois29, select_df_IL, illinois29)

# Subset 30
dists <- fields::rdist(coords_df_IL30, coords_illinois30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- il_topo |>
  dplyr::filter(x >= min(coords_illinois30$long) - 0.1 & x <= max(coords_illinois30$long) + 0.1 &
                  y >= min(coords_illinois30$lat) - 0.1 & y <= max(coords_illinois30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IL, illinois30)
IL_topo_pls <- rbind(IL_topo_pls, temp)

rm(closest_points, coords_df_IL30, coords_illinois30, select_df_IL, illinois30)

# Update column names
colnames(IL_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope', 
                           'aspect', 'state', 'pls_x', 'pls_y', 
                           'L1_tree1', 'L1_tree2', 'L1_tree3', 'L1_tree4', 
                           'uniqueID')

# Save IL topography. Confirmed in same order as illinois and illinois ecosystem
IL_topo <- dplyr::select(IL_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(IL_topo, file = 'data/processed/topography/processed_topo_il.RData')

#### INDIANA ####
rm(list = ls())

load('data/raw/topography/in_topo.RData')

load('data/processed/PLS/indiana_format.RData')

# Remove negative points because we know land in indiana is not below sea level
in_topo <- dplyr::filter(in_topo, elevation >= 0)

# Format indiana data
indiana <- indiana |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide pls indiana into 20 sections
indiana1 <- indiana[1:3959,]
indiana2 <- indiana[3960:7919,]
indiana3 <- indiana[7920:11879,]
indiana4 <- indiana[11880:15839,]
indiana5 <- indiana[15840:19799,]
indiana6 <- indiana[19800:23759,]
indiana7 <- indiana[23760:27719,]
indiana8 <- indiana[27720:31679,]
indiana9 <- indiana[31680:35639,]
indiana10 <- indiana[35640:39599,]
indiana11 <- indiana[39600:43559,]
indiana12 <- indiana[43560:47519,]
indiana13 <- indiana[47520:51479,]
indiana14 <- indiana[51480:55439,]
indiana15 <- indiana[55440:59399,]
indiana16 <- indiana[59400:63359,]
indiana17 <- indiana[63360:67319,]
indiana18 <- indiana[67320:71279,]
indiana19 <- indiana[71280:75239,]
indiana20 <- indiana[75240:79177,]

# Select coordinates
coords_indiana1 <- dplyr::select(indiana1, y, x)
coords_indiana2 <- dplyr::select(indiana2, y, x)
coords_indiana3 <- dplyr::select(indiana3, y, x)
coords_indiana4 <- dplyr::select(indiana4, y, x)
coords_indiana5 <- dplyr::select(indiana5, y, x)
coords_indiana6 <- dplyr::select(indiana6, y, x)
coords_indiana7 <- dplyr::select(indiana7, y, x)
coords_indiana8 <- dplyr::select(indiana8, y, x)
coords_indiana9 <- dplyr::select(indiana9, y, x)
coords_indiana10 <- dplyr::select(indiana10, y, x)
coords_indiana11 <- dplyr::select(indiana11, y, x)
coords_indiana12 <- dplyr::select(indiana12, y, x)
coords_indiana13 <- dplyr::select(indiana13, y, x)
coords_indiana14 <- dplyr::select(indiana14, y, x)
coords_indiana15 <- dplyr::select(indiana15, y, x)
coords_indiana16 <- dplyr::select(indiana16, y, x)
coords_indiana17 <- dplyr::select(indiana17, y, x)
coords_indiana18 <- dplyr::select(indiana18, y, x)
coords_indiana19 <- dplyr::select(indiana19, y, x)
coords_indiana20 <- dplyr::select(indiana20, y, x)

# Add column names
colnames(coords_indiana1) <- colnames(coords_indiana2) <- colnames(coords_indiana3) <-
  colnames(coords_indiana4) <- colnames(coords_indiana5) <- colnames(coords_indiana6) <-
  colnames(coords_indiana7) <- colnames(coords_indiana8) <- colnames(coords_indiana9) <-
  colnames(coords_indiana10) <- colnames(coords_indiana11) <- colnames(coords_indiana12) <-
  colnames(coords_indiana13) <- colnames(coords_indiana14) <- colnames(coords_indiana15) <-
  colnames(coords_indiana16) <- colnames(coords_indiana17) <- colnames(coords_indiana18) <-
  colnames(coords_indiana19) <- colnames(coords_indiana20) <- c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_IN1 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana1$long) - 0.1 & x <= max(coords_indiana1$long) + 0.1 &
                  y >= min(coords_indiana1$lat) - 0.1 & y <= max(coords_indiana1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN2 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana2$long) - 0.1 & x <= max(coords_indiana2$long) + 0.1 &
                  y >= min(coords_indiana2$lat) - 0.1 & y <= max(coords_indiana2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN3 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana3$long) - 0.1 & x <= max(coords_indiana3$long) + 0.1 &
                  y >= min(coords_indiana3$lat) - 0.1 & y <= max(coords_indiana3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN4 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana4$long) - 0.1 & x <= max(coords_indiana4$long) + 0.1 &
                  y >= min(coords_indiana4$lat) - 0.1 & y <= max(coords_indiana4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN5 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana5$long) - 0.1 & x <= max(coords_indiana5$long) + 0.1 &
                  y >= min(coords_indiana5$lat) - 0.1 & y <= max(coords_indiana5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN6 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana6$long) - 0.1 & x <= max(coords_indiana6$long) + 0.1 &
                  y >= min(coords_indiana6$lat) - 0.1 & y <= max(coords_indiana6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN7 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana7$long) - 0.1 & x <= max(coords_indiana7$long) + 0.1 &
                  y >= min(coords_indiana7$lat) - 0.1 & y <= max(coords_indiana7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN8 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana8$long) - 0.1 & x <= max(coords_indiana8$long) + 0.1 &
                  y >= min(coords_indiana8$lat) - 0.1 & y <= max(coords_indiana8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN9 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana9$long) - 0.1 & x <= max(coords_indiana9$long) + 0.1 &
                  y >= min(coords_indiana9$lat) - 0.1 & y <= max(coords_indiana9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN10 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana10$long) - 0.1 & x <= max(coords_indiana10$long) + 0.1 &
                  y >= min(coords_indiana10$lat) - 0.1 & y <= max(coords_indiana10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN11 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana11$long) - 0.1 & x <= max(coords_indiana11$long) + 0.1 &
                  y >= min(coords_indiana11$lat) - 0.1 & y <= max(coords_indiana11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN12 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana12$long) - 0.1 & x <= max(coords_indiana12$long) + 0.1 &
                  y >= min(coords_indiana12$lat) - 0.1 & y <= max(coords_indiana12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN13 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana13$long) - 0.1 & x <= max(coords_indiana13$long) + 0.1 &
                  y >= min(coords_indiana13$lat) - 0.1 & y <= max(coords_indiana13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN14 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana14$long) - 0.1 & x <= max(coords_indiana14$long) + 0.1 &
                  y >= min(coords_indiana14$lat) - 0.1 & y <= max(coords_indiana14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN15 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana15$long) - 0.1 & x <= max(coords_indiana15$long) + 0.1 &
                  y >= min(coords_indiana15$lat) - 0.1 & y <= max(coords_indiana15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN16 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana16$long) - 0.1 & x <= max(coords_indiana16$long) + 0.1 &
                  y >= min(coords_indiana16$lat) - 0.1 & y <= max(coords_indiana16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN17 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana17$long) - 0.1 & x <= max(coords_indiana17$long) + 0.1 &
                  y >= min(coords_indiana17$lat) - 0.1 & y <= max(coords_indiana17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN18 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana18$long) - 0.1 & x <= max(coords_indiana18$long) + 0.1 &
                  y >= min(coords_indiana18$lat) - 0.1 & y <= max(coords_indiana18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN19 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana19$long) - 0.1 & x <= max(coords_indiana19$long) + 0.1 &
                  y >= min(coords_indiana19$lat) - 0.1 & y <= max(coords_indiana19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_IN20 <- in_topo |>
  dplyr::filter(x >= min(coords_indiana20$long) - 0.1 & x <= max(coords_indiana20$long) + 0.1 &
                  y >= min(coords_indiana20$lat) - 0.1 & y <= max(coords_indiana20$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_IN1, coords_indiana1)
# Find the closest soil point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS pionts
# We'll add to this subsequently
select_df_IN <- in_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_indiana1$long) - 0.1 & x <= max(coords_indiana1$long) + 0.1 &
                  y >= min(coords_indiana1$lat) - 0.1 & y <= max(coords_indiana1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
IN_topo_pls <- cbind(select_df_IN, indiana1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_IN1, coords_indiana1, select_df_IN, indiana1)

# Repeat all these steps for the remaining 19 subsets

# Subset 2
dists <- fields::rdist(coords_df_IN2, coords_indiana2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana2$long) - 0.1 & x <= max(coords_indiana2$long) + 0.1 &
                  y >= min(coords_indiana2$lat) - 0.1 & y <= max(coords_indiana2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana2)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN2, coords_indiana2, select_df_IN, indiana2)

# Subset 3
dists <- fields::rdist(coords_df_IN3, coords_indiana3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana3$long) - 0.1 & x <= max(coords_indiana3$long) + 0.1 &
                  y >= min(coords_indiana3$lat) - 0.1 & y <= max(coords_indiana3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana3)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN3, coords_indiana3, select_df_IN, indiana3)

# Subset 4
dists <- fields::rdist(coords_df_IN4, coords_indiana4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana4$long) - 0.1 & x <= max(coords_indiana4$long) + 0.1 &
                  y >= min(coords_indiana4$lat) - 0.1 & y <= max(coords_indiana4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana4)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN4, coords_indiana4, select_df_IN, indiana4)

# Subset 5
dists <- fields::rdist(coords_df_IN5, coords_indiana5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana5$long) - 0.1 & x <= max(coords_indiana5$long) + 0.1 &
                  y >= min(coords_indiana5$lat) - 0.1 & y <= max(coords_indiana5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana5)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN5, coords_indiana5, select_df_IN, indiana5)

# Subset 6
dists <- fields::rdist(coords_df_IN6, coords_indiana6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana6$long) - 0.1 & x <= max(coords_indiana6$long) + 0.1 &
                  y >= min(coords_indiana6$lat) - 0.1 & y <= max(coords_indiana6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana6)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN6, coords_indiana6, select_df_IN, indiana6)

# Subset 7
dists <- fields::rdist(coords_df_IN7, coords_indiana7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana7$long) - 0.1 & x <= max(coords_indiana7$long) + 0.1 &
                  y >= min(coords_indiana7$lat) - 0.1 & y <= max(coords_indiana7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana7)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN7, coords_indiana7, select_df_IN, indiana7)

# Subset 8
dists <- fields::rdist(coords_df_IN8, coords_indiana8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana8$long) - 0.1 & x <= max(coords_indiana8$long) + 0.1 &
                  y >= min(coords_indiana8$lat) - 0.1 & y <= max(coords_indiana8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana8)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN8, coords_indiana8, select_df_IN, indiana8)

# Subset 9
dists <- fields::rdist(coords_df_IN9, coords_indiana9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana9$long) - 0.1 & x <= max(coords_indiana9$long) + 0.1 &
                  y >= min(coords_indiana9$lat) - 0.1 & y <= max(coords_indiana9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana9)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN9, coords_indiana9, select_df_IN, indiana9)

# Subset 10
dists <- fields::rdist(coords_df_IN10, coords_indiana10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana10$long) - 0.1 & x <= max(coords_indiana10$long) + 0.1 &
                  y >= min(coords_indiana10$lat) - 0.1 & y <= max(coords_indiana10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana10)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN10, coords_indiana10, select_df_IN, indiana10)

# Subset 11
dists <- fields::rdist(coords_df_IN11, coords_indiana11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana11$long) - 0.1 & x <= max(coords_indiana11$long) + 0.1 &
                  y >= min(coords_indiana11$lat) - 0.1 & y <= max(coords_indiana11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana11)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN11, coords_indiana11, select_df_IN, indiana11)

# Subset 12
dists <- fields::rdist(coords_df_IN12, coords_indiana12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana12$long) - 0.1 & x <= max(coords_indiana12$long) + 0.1 &
                  y >= min(coords_indiana12$lat) - 0.1 & y <= max(coords_indiana12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana12)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN12, coords_indiana12, select_df_IN, indiana12)

# Subset 13
dists <- fields::rdist(coords_df_IN13, coords_indiana13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana13$long) - 0.1 & x <= max(coords_indiana13$long) + 0.1 &
                  y >= min(coords_indiana13$lat) - 0.1 & y <= max(coords_indiana13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana13)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN13, coords_indiana13, select_df_IN, indiana13)

# Subset 14
dists <- fields::rdist(coords_df_IN14, coords_indiana14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana14$long) - 0.1 & x <= max(coords_indiana14$long) + 0.1 &
                  y >= min(coords_indiana14$lat) - 0.1 & y <= max(coords_indiana14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana14)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN14, coords_indiana14, select_df_IN, indiana14)

# Subset 15
dists <- fields::rdist(coords_df_IN15, coords_indiana15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana15$long) - 0.1 & x <= max(coords_indiana15$long) + 0.1 &
                  y >= min(coords_indiana15$lat) - 0.1 & y <= max(coords_indiana15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana15)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN15, coords_indiana15, select_df_IN, indiana15)

# Subset 16
dists <- fields::rdist(coords_df_IN16, coords_indiana16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana16$long) - 0.1 & x <= max(coords_indiana16$long) + 0.1 &
                  y >= min(coords_indiana16$lat) - 0.1 & y <= max(coords_indiana16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana16)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN16, coords_indiana16, select_df_IN, indiana16)

# Subset 17
dists <- fields::rdist(coords_df_IN17, coords_indiana17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana17$long) - 0.1 & x <= max(coords_indiana17$long) + 0.1 &
                  y >= min(coords_indiana17$lat) - 0.1 & y <= max(coords_indiana17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana17)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN17, coords_indiana17, select_df_IN, indiana17)

# Subset 18
dists <- fields::rdist(coords_df_IN18, coords_indiana18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana18$long) - 0.1 & x <= max(coords_indiana18$long) + 0.1 &
                  y >= min(coords_indiana18$lat) - 0.1 & y <= max(coords_indiana18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana18)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN18, coords_indiana18, select_df_IN, indiana18)

# Subset 19
dists <- fields::rdist(coords_df_IN19, coords_indiana19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana19$long) - 0.1 & x <= max(coords_indiana19$long) + 0.1 &
                  y >= min(coords_indiana19$lat) - 0.1 & y <= max(coords_indiana19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana19)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN19, coords_indiana19, select_df_IN, indiana19)

# Subset 20
dists <- fields::rdist(coords_df_IN20, coords_indiana20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- in_topo |>
  dplyr::filter(x >= min(coords_indiana20$long) - 0.1 & x <= max(coords_indiana20$long) + 0.1 &
                  y >= min(coords_indiana20$lat) - 0.1 & y <= max(coords_indiana20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_IN, indiana20)
IN_topo_pls <- rbind(IN_topo_pls, temp)

rm(closest_points, coords_df_IN20, coords_indiana20, select_df_IN, indiana20)

# Update column names
colnames(IN_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope',
                           'aspect', 'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2', 'uniqueID')

# Save IN topography. Confirmed in same order as indiana and indiana_ecosystem
IN_topo <- dplyr::select(IN_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(IN_topo, file = 'data/processed/topography/processed_topo_in.RData')

#### MICHIGAN ####
rm(list = ls())

load('data/raw/topography/mi_topo.RData')

load('data/processed/PLS/lowmichigan_process.RData')
load('data/processed/PLS/upmichigan_process.RData')

# Remove negative points because we know land in michigan is not below sea level
mi_topo <- dplyr::filter(mi_topo, elevation >= 0)

# Format lower michigan data
lowmichigan <- lowmichigan |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::rename(SPP1 = species1,
                SPP2 = species2)

# Format upper michigan data
upmichigan <- upmichigan |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything())

michigan <- rbind(lowmichigan, upmichigan)
michigan_ecosystem <- rbind(lowmichigan_ecosystem,
                            upmichigan_ecosystem)

# Divide pls michigan into 20 sections
michigan1 <- michigan[1:4018,]
michigan2 <- michigan[4019:8037,]
michigan3 <- michigan[8038:12056,]
michigan4 <- michigan[12057:16075,]
michigan5 <- michigan[16076:20094,]
michigan6 <- michigan[20095:24113,]
michigan7 <- michigan[24114:28632,]
michigan8 <- michigan[28633:30633,]
michigan9 <- michigan[30634:34652,]
michigan10 <- michigan[34653:38671,]
michigan11 <- michigan[38672:42690,]
michigan12 <- michigan[42691:46709,]
michigan13 <- michigan[46710:50728,]
michigan14 <- michigan[50729:54747,]
michigan15 <- michigan[54748:58766,]
michigan16 <- michigan[58767:62786,]
michigan17 <- michigan[62787:66805,]
michigan18 <- michigan[66806:71324,]
michigan19 <- michigan[71325:74843,]
michigan20 <- michigan[74844:78862,]
michigan21 <- michigan[78863:82881,]
michigan22 <- michigan[82882:86900,]
michigan23 <- michigan[86901:90919,]
michigan24 <- michigan[90920:94938,]
michigan25 <- michigan[94939:98957,]
michigan26 <- michigan[98958:102976,]
michigan27 <- michigan[102977:106995,]
michigan28 <- michigan[106996:111014,]
michigan29 <- michigan[111015:115733,]
michigan30 <- michigan[115734:120554,]

# Select coordinates
coords_michigan1 <- dplyr::select(michigan1, y, x)
coords_michigan2 <- dplyr::select(michigan2, y, x)
coords_michigan3 <- dplyr::select(michigan3, y, x)
coords_michigan4 <- dplyr::select(michigan4, y, x)
coords_michigan5 <- dplyr::select(michigan5, y, x)
coords_michigan6 <- dplyr::select(michigan6, y, x)
coords_michigan7 <- dplyr::select(michigan7, y, x)
coords_michigan8 <- dplyr::select(michigan8, y, x)
coords_michigan9 <- dplyr::select(michigan9, y, x)
coords_michigan10 <- dplyr::select(michigan10, y, x)
coords_michigan11 <- dplyr::select(michigan11, y, x)
coords_michigan12 <- dplyr::select(michigan12, y, x)
coords_michigan13 <- dplyr::select(michigan13, y, x)
coords_michigan14 <- dplyr::select(michigan14, y, x)
coords_michigan15 <- dplyr::select(michigan15, y, x)
coords_michigan16 <- dplyr::select(michigan16, y, x)
coords_michigan17 <- dplyr::select(michigan17, y, x)
coords_michigan18 <- dplyr::select(michigan18, y, x)
coords_michigan19 <- dplyr::select(michigan19, y, x)
coords_michigan20 <- dplyr::select(michigan20, y, x)
coords_michigan21 <- dplyr::select(michigan21, y, x)
coords_michigan22 <- dplyr::select(michigan22, y, x)
coords_michigan23 <- dplyr::select(michigan23, y, x)
coords_michigan24 <- dplyr::select(michigan24, y, x)
coords_michigan25 <- dplyr::select(michigan25, y, x)
coords_michigan26 <- dplyr::select(michigan26, y, x)
coords_michigan27 <- dplyr::select(michigan27, y, x)
coords_michigan28 <- dplyr::select(michigan28, y, x)
coords_michigan29 <- dplyr::select(michigan29, y, x)
coords_michigan30 <- dplyr::select(michigan30, y, x)

# Add column names
colnames(coords_michigan1) <- colnames(coords_michigan2) <- colnames(coords_michigan3) <-
  colnames(coords_michigan4) <- colnames(coords_michigan5) <- colnames(coords_michigan6) <-
  colnames(coords_michigan7) <- colnames(coords_michigan8) <- colnames(coords_michigan9) <-
  colnames(coords_michigan10) <- colnames(coords_michigan11) <- colnames(coords_michigan12) <-
  colnames(coords_michigan13) <- colnames(coords_michigan14) <- colnames(coords_michigan15) <-
  colnames(coords_michigan16) <- colnames(coords_michigan17) <- colnames(coords_michigan18) <-
  colnames(coords_michigan19) <- colnames(coords_michigan20) <- colnames(coords_michigan21) <-
  colnames(coords_michigan22) <- colnames(coords_michigan23) <- colnames(coords_michigan24) <-
  colnames(coords_michigan25) <- colnames(coords_michigan26) <- colnames(coords_michigan27) <-
  colnames(coords_michigan28) <- colnames(coords_michigan29) <- colnames(coords_michigan30) <-
  c('lat', 'long')

# Subset the soil data to the lat/long extent of each pls subsection
# Then extract only the lat/long columns
coords_df_MI1 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan1$long) - 0.1 & x <= max(coords_michigan1$long) + 0.1 &
                  y >= min(coords_michigan1$lat) - 0.1 & y <= max(coords_michigan1$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI2 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan2$long) - 0.1 & x <= max(coords_michigan2$long) + 0.1 &
                  y >= min(coords_michigan2$lat) - 0.1 & y <= max(coords_michigan2$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI3 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan3$long) - 0.1 & x <= max(coords_michigan3$long) + 0.1 &
                  y >= min(coords_michigan3$lat) - 0.1 & y <= max(coords_michigan3$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI4 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan4$long) - 0.1 & x <= max(coords_michigan4$long) + 0.1 &
                  y >= min(coords_michigan4$lat) - 0.1 & y <= max(coords_michigan4$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI5 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan5$long) - 0.1 & x <= max(coords_michigan5$long) + 0.1 &
                  y >= min(coords_michigan5$lat) - 0.1 & y <= max(coords_michigan5$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI6 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan6$long) - 0.1 & x <= max(coords_michigan6$long) + 0.1 &
                  y >= min(coords_michigan6$lat) - 0.1 & y <= max(coords_michigan6$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI7 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan7$long) - 0.1 & x <= max(coords_michigan7$long) + 0.1 &
                  y >= min(coords_michigan7$lat) - 0.1 & y <= max(coords_michigan7$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI8 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan8$long) - 0.1 & x <= max(coords_michigan8$long) + 0.1 &
                  y >= min(coords_michigan8$lat) - 0.1 & y <= max(coords_michigan8$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI9 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan9$long) - 0.1 & x <= max(coords_michigan9$long) + 0.1 &
                  y >= min(coords_michigan9$lat) - 0.1 & y <= max(coords_michigan9$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI10 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan10$long) - 0.1 & x <= max(coords_michigan10$long) + 0.1 &
                  y >= min(coords_michigan10$lat) - 0.1 & y <= max(coords_michigan10$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI11 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan11$long) - 0.1 & x <= max(coords_michigan11$long) + 0.1 &
                  y >= min(coords_michigan11$lat) - 0.1 & y <= max(coords_michigan11$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI12 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan12$long) - 0.1 & x <= max(coords_michigan12$long) + 0.1 &
                  y >= min(coords_michigan12$lat) - 0.1 & y <= max(coords_michigan12$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI13 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan13$long) - 0.1 & x <= max(coords_michigan13$long) + 0.1 &
                  y >= min(coords_michigan13$lat) - 0.1 & y <= max(coords_michigan13$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI14 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan14$long) - 0.1 & x <= max(coords_michigan14$long) + 0.1 &
                  y >= min(coords_michigan14$lat) - 0.1 & y <= max(coords_michigan14$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI15 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan15$long) - 0.1 & x <= max(coords_michigan15$long) + 0.1 &
                  y >= min(coords_michigan15$lat) - 0.1 & y <= max(coords_michigan15$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI16 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan16$long) - 0.1 & x <= max(coords_michigan16$long) + 0.1 &
                  y >= min(coords_michigan16$lat) - 0.1 & y <= max(coords_michigan16$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI17 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan17$long) - 0.1 & x <= max(coords_michigan17$long) + 0.1 &
                  y >= min(coords_michigan17$lat) - 0.1 & y <= max(coords_michigan17$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI18 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan18$long) - 0.1 & x <= max(coords_michigan18$long) + 0.1 &
                  y >= min(coords_michigan18$lat) - 0.1 & y <= max(coords_michigan18$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI19 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan19$long) - 0.1 & x <= max(coords_michigan19$long) + 0.1 &
                  y >= min(coords_michigan19$lat) - 0.1 & y <= max(coords_michigan19$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI20 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan20$long) - 0.1 & x <= max(coords_michigan20$long) + 0.1 &
                  y >= min(coords_michigan20$lat) - 0.1 & y <= max(coords_michigan20$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI21 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan21$long) - 0.1 & x <= max(coords_michigan21$long) + 0.1 &
                  y >= min(coords_michigan21$lat) - 0.1 & y <= max(coords_michigan21$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI22 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan22$long) - 0.1 & x <= max(coords_michigan22$long) + 0.1 &
                  y >= min(coords_michigan22$lat) - 0.1 & y <= max(coords_michigan22$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI23 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan23$long) - 0.1 & x <= max(coords_michigan23$long) + 0.1 &
                  y >= min(coords_michigan23$lat) - 0.1 & y <= max(coords_michigan23$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI24 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan24$long) - 0.1 & x <= max(coords_michigan24$long) + 0.1 &
                  y >= min(coords_michigan24$lat) - 0.1 & y <= max(coords_michigan24$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI25 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan25$long) - 0.1 & x <= max(coords_michigan25$long) + 0.1 &
                  y >= min(coords_michigan25$lat) - 0.1 & y <= max(coords_michigan25$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI26 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan26$long) - 0.1 & x <= max(coords_michigan26$long) + 0.1 &
                  y >= min(coords_michigan26$lat) - 0.1 & y <= max(coords_michigan26$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI27 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan27$long) - 0.1 & x <= max(coords_michigan27$long) + 0.1 &
                  y >= min(coords_michigan27$lat) - 0.1 & y <= max(coords_michigan27$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI28 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan28$long) - 0.1 & x <= max(coords_michigan28$long) + 0.1 &
                  y >= min(coords_michigan28$lat) - 0.1 & y <= max(coords_michigan28$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI29 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan29$long) - 0.1 & x <= max(coords_michigan29$long) + 0.1 &
                  y >= min(coords_michigan29$lat) - 0.1 & y <= max(coords_michigan29$lat) + 0.1) |>
  dplyr::select(y, x)
coords_df_MI30 <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan30$long) - 0.1 & x <= max(coords_michigan30$long) + 0.1 &
                  y >= min(coords_michigan30$lat) - 0.1 & y <= max(coords_michigan30$lat) + 0.1) |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the pls (1) and soil (2) dataframes
dists <- fields::rdist(coords_df_MI1, coords_michigan1)
# Find the closest soil point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately delete dists because it's huge
rm(dists)

# Make new dataframe of corresponding soil data to the PLS points
# We'll add to this subsequently
select_df_MI <- mi_topo |>
  # Filter as done above to make sure indexing matches
  dplyr::filter(x >= min(coords_michigan1$long) - 0.1 & x <= max(coords_michigan1$long) + 0.1 &
                  y >= min(coords_michigan1$lat) - 0.1 & y <= max(coords_michigan1$lat) + 0.1) |>
  # subset by row numbers given by which.min
  dplyr::slice(closest_points)

# Column bind with PLS data
MI_topo_pls <- cbind(select_df_MI, michigan1)

# Remove unnecessary objects to keep memory usage lower
rm(closest_points, coords_df_MI1, coords_michigan1, select_df_MI, michigan1)

# Repeat all these steps for the remaining 39 subsets

# Subset 2
dists <- fields::rdist(coords_df_MI2, coords_michigan2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan2$long) - 0.1 & x <= max(coords_michigan2$long) + 0.1 &
                  y >= min(coords_michigan2$lat) - 0.1 & y <= max(coords_michigan2$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan2)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI2, coords_michigan2, select_df_MI, michigan2)

# Subset 3
dists <- fields::rdist(coords_df_MI3, coords_michigan3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan3$long) - 0.1 & x <= max(coords_michigan3$long) + 0.1 &
                  y >= min(coords_michigan3$lat) - 0.1 & y <= max(coords_michigan3$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan3)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI3, coords_michigan3, select_df_MI, michigan3)

# Subset 4
dists <- fields::rdist(coords_df_MI4, coords_michigan4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan4$long) - 0.1 & x <= max(coords_michigan4$long) + 0.1 &
                  y >= min(coords_michigan4$lat) - 0.1 & y <= max(coords_michigan4$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan4)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI4, coords_michigan4, select_df_MI, michigan4)

# Subset 5
dists <- fields::rdist(coords_df_MI5, coords_michigan5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan5$long) - 0.1 & x <= max(coords_michigan5$long) + 0.1 &
                  y >= min(coords_michigan5$lat) - 0.1 & y <= max(coords_michigan5$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan5)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI5, coords_michigan5, select_df_MI, michigan5)

# Subset 6
dists <- fields::rdist(coords_df_MI6, coords_michigan6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan6$long) - 0.1 & x <= max(coords_michigan6$long) + 0.1 &
                  y >= min(coords_michigan6$lat) - 0.1 & y <= max(coords_michigan6$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan6)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI6, coords_michigan6, select_df_MI, michigan6)

# Subset 7
dists <- fields::rdist(coords_df_MI7, coords_michigan7)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan7$long) - 0.1 & x <= max(coords_michigan7$long) + 0.1 &
                  y >= min(coords_michigan7$lat) - 0.1 & y <= max(coords_michigan7$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan7)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI7, coords_michigan7, select_df_MI, michigan7)

# Subset 8
dists <- fields::rdist(coords_df_MI8, coords_michigan8)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan8$long) - 0.1 & x <= max(coords_michigan8$long) + 0.1 &
                  y >= min(coords_michigan8$lat) - 0.1 & y <= max(coords_michigan8$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan8)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI8, coords_michigan8, select_df_MI, michigan8)

# Subset 9
dists <- fields::rdist(coords_df_MI9, coords_michigan9)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan9$long) - 0.1 & x <= max(coords_michigan9$long) + 0.1 &
                  y >= min(coords_michigan9$lat) - 0.1 & y <= max(coords_michigan9$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan9)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI9, coords_michigan9, select_df_MI, michigan9)

# Subset 10
dists <- fields::rdist(coords_df_MI10, coords_michigan10)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan10$long) - 0.1 & x <= max(coords_michigan10$long) + 0.1 &
                  y >= min(coords_michigan10$lat) - 0.1 & y <= max(coords_michigan10$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan10)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI10, coords_michigan10, select_df_MI, michigan10)

# Subset 11
dists <- fields::rdist(coords_df_MI11, coords_michigan11)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan11$long) - 0.1 & x <= max(coords_michigan11$long) + 0.1 &
                  y >= min(coords_michigan11$lat) - 0.1 & y <= max(coords_michigan11$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan11)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI11, coords_michigan11, select_df_MI, michigan11)

# Subset 12
dists <- fields::rdist(coords_df_MI12, coords_michigan12)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan12$long) - 0.1 & x <= max(coords_michigan12$long) + 0.1 &
                  y >= min(coords_michigan12$lat) - 0.1 & y <= max(coords_michigan12$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan12)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI12, coords_michigan12, select_df_MI, michigan12)

# Subset 13
dists <- fields::rdist(coords_df_MI13, coords_michigan13)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan13$long) - 0.1 & x <= max(coords_michigan13$long) + 0.1 &
                  y >= min(coords_michigan13$lat) - 0.1 & y <= max(coords_michigan13$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan13)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI13, coords_michigan13, select_df_MI, michigan13)

# Subset 14
dists <- fields::rdist(coords_df_MI14, coords_michigan14)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan14$long) - 0.1 & x <= max(coords_michigan14$long) + 0.1 &
                  y >= min(coords_michigan14$lat) - 0.1 & y <= max(coords_michigan14$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan14)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI14, coords_michigan14, select_df_MI, michigan14)

# Subset 15
dists <- fields::rdist(coords_df_MI15, coords_michigan15)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan15$long) - 0.1 & x <= max(coords_michigan15$long) + 0.1 &
                  y >= min(coords_michigan15$lat) - 0.1 & y <= max(coords_michigan15$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan15)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI15, coords_michigan15, select_df_MI, michigan15)

# Subset 16
dists <- fields::rdist(coords_df_MI16, coords_michigan16)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan16$long) - 0.1 & x <= max(coords_michigan16$long) + 0.1 &
                  y >= min(coords_michigan16$lat) - 0.1 & y <= max(coords_michigan16$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan16)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI16, coords_michigan16, select_df_MI, michigan16)

# Subset 17
dists <- fields::rdist(coords_df_MI17, coords_michigan17)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan17$long) - 0.1 & x <= max(coords_michigan17$long) + 0.1 &
                  y >= min(coords_michigan17$lat) - 0.1 & y <= max(coords_michigan17$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan17)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI17, coords_michigan17, select_df_MI, michigan17)

# Subset 18
dists <- fields::rdist(coords_df_MI18, coords_michigan18)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan18$long) - 0.1 & x <= max(coords_michigan18$long) + 0.1 &
                  y >= min(coords_michigan18$lat) - 0.1 & y <= max(coords_michigan18$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan18)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI18, coords_michigan18, select_df_MI, michigan18)

# Subset 19
dists <- fields::rdist(coords_df_MI19, coords_michigan19)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan19$long) - 0.1 & x <= max(coords_michigan19$long) + 0.1 &
                  y >= min(coords_michigan19$lat) - 0.1 & y <= max(coords_michigan19$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan19)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI19, coords_michigan19, select_df_MI, michigan19)

# Subset 20
dists <- fields::rdist(coords_df_MI20, coords_michigan20)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan20$long) - 0.1 & x <= max(coords_michigan20$long) + 0.1 &
                  y >= min(coords_michigan20$lat) - 0.1 & y <= max(coords_michigan20$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan20)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI20, coords_michigan20, select_df_MI, michigan20)

# Subset 21
dists <- fields::rdist(coords_df_MI21, coords_michigan21)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan21$long) - 0.1 & x <= max(coords_michigan21$long) + 0.1 &
                  y >= min(coords_michigan21$lat) - 0.1 & x <= max(coords_michigan21$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan21)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI21, coords_michigan21, select_df_MI, michigan21)

# Subset 22
dists <- fields::rdist(coords_df_MI22, coords_michigan22)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan22$long) - 0.1 & x <= max(coords_michigan22$long) + 0.1 &
                  y >= min(coords_michigan22$lat) - 0.1 & y <= max(coords_michigan22$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan22)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI22, coords_michigan22, select_df_MI, michigan22)

# Subset 23
dists <- fields::rdist(coords_df_MI23, coords_michigan23)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan23$long) - 0.1 & x <= max(coords_michigan23$long) + 0.1 &
                  y >= min(coords_michigan23$lat) - 0.1 & y <= max(coords_michigan23$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan23)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI23, coords_michigan23, select_df_MI, michigan23)

# Subset 24
dists <- fields::rdist(coords_df_MI24, coords_michigan24)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan24$long) - 0.1 & x <= max(coords_michigan24$long) + 0.1 &
                  y >= min(coords_michigan24$lat) - 0.1 & y <= max(coords_michigan24$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan24)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI24, coords_michigan24, select_df_MI, michigan24)

# Subset 25
dists <- fields::rdist(coords_df_MI25, coords_michigan25)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan25$long) - 0.1 & x <= max(coords_michigan25$long) + 0.1 &
                  y >= min(coords_michigan25$lat) - 0.1 & y <= max(coords_michigan25$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan25)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI25, coords_michigan25, select_df_MI, michigan25)

# Subset 26
dists <- fields::rdist(coords_df_MI26, coords_michigan26)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan26$long) - 0.1 & x <= max(coords_michigan26$long) + 0.1 &
                  y >= min(coords_michigan26$lat) - 0.1 & y <= max(coords_michigan26$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan26)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI26, coords_michigan26, select_df_MI, michigan26)

# Subset 27
dists <- fields::rdist(coords_df_MI27, coords_michigan27)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan27$long) - 0.1 & x <= max(coords_michigan27$long) + 0.1 &
                  y >= min(coords_michigan27$lat) - 0.1 & y <= max(coords_michigan27$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan27)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI27, coords_michigan27, select_df_MI, michigan27)

# Subset 28
dists <- fields::rdist(coords_df_MI28, coords_michigan28)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan28$long) - 0.1 & x <= max(coords_michigan28$long) + 0.1 &
                  y >= min(coords_michigan28$lat) - 0.1 & x <= max(coords_michigan28$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan28)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI28, coords_michigan28, select_df_MI, michigan28)

# Subset 29
dists <- fields::rdist(coords_df_MI29, coords_michigan29)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan29$long) - 0.1 & x <= max(coords_michigan29$long) + 0.1 &
                  y >= min(coords_michigan29$lat) - 0.1 & y <= max(coords_michigan29$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan29)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI29, coords_michigan29, select_df_MI, michigan29)

# Subset 30
dists <- fields::rdist(coords_df_MI30, coords_michigan30)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- mi_topo |>
  dplyr::filter(x >= min(coords_michigan30$long) - 0.1 & x <= max(coords_michigan30$long) + 0.1 &
                  y >= min(coords_michigan30$lat) - 0.1 & y <= max(coords_michigan30$lat) + 0.1) |>
  dplyr::slice(closest_points)

temp <- cbind(select_df_MI, michigan30)
MI_topo_pls <- rbind(MI_topo_pls, temp)

rm(closest_points, coords_df_MI30, coords_michigan30, select_df_MI, michigan30)

# Update column names
colnames(MI_topo_pls) <- c('topo_x', 'topo_y', 'elevation', 'slope',
                           'aspect', 'state', 'pls_x', 'pls_y',
                           'L1_tree1', 'L1_tree2')

# Save MI topography. Confirmed in same order as michigan and michigan_ecosystem
MI_topo <- dplyr::select(MI_topo_pls, topo_x, topo_y, elevation, slope, aspect, pls_x, pls_y)
save(MI_topo, file = 'data/processed/topography/processed_topo_mi.RData')

#### MINNESOTA ####
rm(list = ls())

