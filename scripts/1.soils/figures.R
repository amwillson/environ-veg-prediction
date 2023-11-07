# Make map to compare against
states <- map_data('state', region = c('illinois', 'indiana', 'michigan', 'minnesota', 'wisconsin'))

# Take out values that are not (0,1) bounded
for_removal <- which(soils_df$sandtotal_r < 0 | soils_df$sandtotal_r > 100 |
                       soils_df$silttotal_r < 0 | soils_df$silttotal_r > 100 |
                       soils_df$claytotal_r < 0 | soils_df$claytotal_r > 100)
soils_df <- soils_df[-for_removal,]

# Plot each variable
soils_df |>
  dplyr::rename(`% sand` = sandtotal_r) |>
  ggplot(aes(x = x, y = y, color = `% sand`)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_map(projection = 'albers', lat0 = 50, lat1 = 40) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrBr', limits = c(0, 100)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
soils_df |>
  dplyr::rename(`% silt` = silttotal_r) |>
  ggplot(aes(x = x, y = y, color = `% silt`)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_map(projection = 'albers', lat0 = 50, lat1 = 40) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrBr', limits = c(0, 100)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
soils_df |>
  dplyr::rename(`% clay` = claytotal_r) |>
  ggplot(aes(x = x, y = y, color = `% clay`)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  coord_map(projection = 'albers', lat0 = 50, lat1 = 40) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrBr', limits = c(0, 100)) +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
