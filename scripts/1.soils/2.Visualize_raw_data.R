# Figures of raw soil data

# https://pubmed.ncbi.nlm.nih.gov/28307854/
# ^ for global root distribution

rm(list = ls())

library(ggplot2)
library(tidyr)
library(cowplot)

## Loading data and renaming to compare different experiments
load('data/raw/gssurgo_average_030_3000m.RData')

df_average_030 <- df_all

rm(df_all, df_IL, df_IN, df_MI, df_MN, df_WI)

load('data/raw/gssurgo_average_2550_3000m.RData')

df_average_2550 <- df_all

rm(df_all, df_IL, df_IN, df_MI, df_MN, df_WI)

load('data/raw/gssurgo_dominant_030_3000m.RData')

df_dominant_030 <- df_all

rm(df_all, df_IL, df_IN, df_MI, df_MN, df_WI)

load('data/raw/gssurgo_dominant_2550_3000m.RData')

df_dominant_2550 <- df_all

rm(df_all, df_IL, df_IN, df_MI, df_MN, df_WI)

states <- ggplot2::map_data('state', region = c('illinois', 'indiana', 'michigan',
                                                'minnesota', 'wisconsin'))

## Average 0-30 cm
average_030 <- df_average_030 |>
  tidyr::pivot_longer(cols = claytotal_r:silttotal_r, names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'claytotal_r', 'Clay', var),
                var = dplyr::if_else(var == 'sandtotal_r', 'Sand', var),
                var = dplyr::if_else(var == 'silttotal_r', 'Silt', var)) |>
  dplyr::filter(val >= 0 & val <= 100) |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'YlOrRd', name = '%', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Weighted average over 0-30 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 15, face = 'bold', hjust = 0.5))
average_030

## Average 25-50 cm
average_2550 <- df_average_2550 |>
  tidyr::pivot_longer(cols = claytotal_r:silttotal_r, names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'claytotal_r', 'Clay', var),
                var = dplyr::if_else(var == 'sandtotal_r', 'Sand', var),
                var = dplyr::if_else(var == 'silttotal_r', 'Silt', var)) |>
  dplyr::filter(val >= 0 & val <= 100) |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'YlOrRd', name = '%', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Weighted average over 25-50 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
average_2550

## Dominant 0-30 cm
dominant_030 <- df_dominant_030 |>
  tidyr::pivot_longer(cols = claytotal_r:silttotal_r, names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'claytotal_r', 'Clay', var),
                var = dplyr::if_else(var == 'sandtotal_r', 'Sand', var),
                var = dplyr::if_else(var == 'silttotal_r', 'Silt', var)) |>
  dplyr::filter(val >= 0 & val <= 100) |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'YlOrRd', name = '%', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Dominant component over 0-30 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
dominant_030

# Dominant 25-50 cm
dominant_2550 <- df_dominant_2550 |>
  tidyr::pivot_longer(cols = claytotal_r:silttotal_r, names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'claytotal_r', 'Clay', var),
                var = dplyr::if_else(var == 'sandtotal_r', 'Sand', var),
                var = dplyr::if_else(var == 'silttotal_r', 'Silt', var)) |>
  dplyr::filter(val >= 0 & val <= 100) |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'YlOrRd', name = '%', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Dominant component over 25-50 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
dominant_2550

cowplot::plot_grid(average_030, average_2550,
                   dominant_030, dominant_2550, 
                   nrow = 2, ncol = 2)

## Look at magnitude and spatial distribution of differences
## comparing with average 0-30 cm
diff_average_2550 <- df_average_030 |>
  dplyr::rename(clay030 = claytotal_r,
                silt030 = silttotal_r,
                sand030 = sandtotal_r) |>
  dplyr::full_join(df_average_2550, by = c('x', 'y')) |>
  dplyr::filter(clay030 >= 0 & clay030 <= 100 &
                  sand030 >= 0 & sand030 <= 100 &
                  silt030 >= 0 & silt030 <= 100 &
                  claytotal_r >= 0 & claytotal_r <= 100 &
                  sandtotal_r >= 0 & sandtotal_r <= 100 &
                  silttotal_r >= 0 & silttotal_r <= 100) |>
  dplyr::mutate(Clay = abs(clay030 - claytotal_r),
                Sand = abs(sand030 - sandtotal_r),
                Silt = abs(silt030 - silttotal_r)) |>
  tidyr::pivot_longer(cols = Clay:Silt, names_to = 'var', values_to = 'val') |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'Reds', name = 'Difference', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Average over 0-30 cm vs. average over 25-50 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
diff_average_2550  

diff_dominant_030 <- df_average_030 |>
  dplyr::rename(clay030 = claytotal_r,
                silt030 = silttotal_r,
                sand030 = sandtotal_r) |>
  dplyr::full_join(df_dominant_030, by = c('x', 'y')) |>
  dplyr::filter(clay030 >= 0 & clay030 <= 100 &
                  sand030 >= 0 & sand030 <= 100 &
                  silt030 >= 0 & silt030 <= 100 &
                  claytotal_r >= 0 & claytotal_r <= 100 &
                  sandtotal_r >= 0 & sandtotal_r <= 100 &
                  silttotal_r >= 0 & silttotal_r <= 100) |>
  dplyr::mutate(Clay = abs(clay030 - claytotal_r),
                Sand = abs(sand030 - sandtotal_r),
                Silt = abs(silt030 - silttotal_r)) |>
  tidyr::pivot_longer(cols = Clay:Silt, names_to = 'var', values_to = 'val') |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'Reds', name = 'Difference', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Average over 0-30 cm vs. dominant component over 0-30 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
diff_dominant_030  

diff_dominant_2550 <- df_average_030 |>
  dplyr::rename(clay030 = claytotal_r,
                silt030 = silttotal_r,
                sand030 = sandtotal_r) |>
  dplyr::full_join(df_dominant_2550, by = c('x', 'y')) |>
  dplyr::filter(clay030 >= 0 & clay030 <= 100 &
                  sand030 >= 0 & sand030 <= 100 &
                  silt030 >= 0 & silt030 <= 100 &
                  claytotal_r >= 0 & claytotal_r <= 100 &
                  sandtotal_r >= 0 & sandtotal_r <= 100 &
                  silttotal_r >= 0 & silttotal_r <= 100) |>
  dplyr::mutate(Clay = abs(clay030 - claytotal_r),
                Sand = abs(sand030 - sandtotal_r),
                Silt = abs(silt030 - silttotal_r)) |>
  tidyr::pivot_longer(cols = Clay:Silt, names_to = 'var', values_to = 'val') |>
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, color = val)) +
  ggplot2::geom_point() +
  ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_map(projection = 'albers', lat0 = 50, lat1 = 37) +
  ggplot2::scale_color_distiller(palette = 'Reds', name = 'Difference', limits = c(0, 100)) +
  ggplot2::facet_wrap(~var) +
  ggplot2::ggtitle('Average over 0-30 cm vs. dominant component over 25-50 cm') +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5))
diff_dominant_2550

cowplot::plot_grid(diff_average_2550, diff_dominant_030, diff_dominant_2550,
                   nrow = 2, ncol = 2)
