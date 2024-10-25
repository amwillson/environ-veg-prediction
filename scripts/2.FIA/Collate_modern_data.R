## Collate modern data

rm(list = ls())

#### 1. Vegetation ####

# Load data
load('data/processed/FIA/gridded_all_plots.RData')

# Take only necessary columns from stem density
fia_density <- stem_density_agg2 |>
  dplyr::select(cell, x, y, total_stem_density) |>
  dplyr::distinct()

# Pivot fractional composition to match PLS format
fia_fc <- fractional_composition_agg2 |>
  dplyr::select(cell, x, y, taxon, fcomp) |>
  tidyr::pivot_wider(names_from = 'taxon',
                     values_from = fcomp)

# Combine
fia_density_fcomp <- fia_density |>
  dplyr::full_join(y = fia_fc,
                   by = c('cell', 'x', 'y')) |>
  # Replace NA with very small number because that's how
  # PLS works (PLS estimates can't be exactly 0)
  dplyr::mutate(Ash = dplyr::if_else(is.na(Ash), 1e-5, Ash),
                Birch = dplyr::if_else(is.na(Birch), 1e-5, Birch),
                `Cedar/juniper` = dplyr::if_else(is.na(`Cedar/juniper`), 1e-5, `Cedar/juniper`),
                Fir = dplyr::if_else(is.na(Fir), 1e-5, Fir),
                `Poplar/tulip poplar` = dplyr::if_else(is.na(`Poplar/tulip poplar`), 1e-5, `Poplar/tulip poplar`),
                Spruce = dplyr::if_else(is.na(Spruce), 1e-5, Spruce),
                Tamarack = dplyr::if_else(is.na(Tamarack), 1e-5, Tamarack),
                Oak = dplyr::if_else(is.na(Oak), 1e-5, Oak),
                Elm = dplyr::if_else(is.na(Elm), 1e-5, Elm),
                Pine = dplyr::if_else(is.na(Pine), 1e-5, Pine),
                Maple = dplyr::if_else(is.na(Maple), 1e-5, Maple),
                Basswood = dplyr::if_else(is.na(Basswood), 1e-5, Basswood),
                `Other hardwood` = dplyr::if_else(is.na(`Other hardwood`), 1e-5, `Other hardwood`),
                Cherry = dplyr::if_else(is.na(Cherry), 1e-5, Cherry),
                Ironwood = dplyr::if_else(is.na(Ironwood), 1e-5, Ironwood),
                Walnut = dplyr::if_else(is.na(Walnut), 1e-5, Walnut),
                Hemlock = dplyr::if_else(is.na(Hemlock), 1e-5, Hemlock),
                Hickory = dplyr::if_else(is.na(Hickory), 1e-5, Hickory),
                Beech = dplyr::if_else(is.na(Beech), 1e-5, Beech),
                `Black gum/sweet gum` = dplyr::if_else(is.na(`Black gum/sweet gum`), 1e-5, `Black gum/sweet gum`),
                Dogwood = dplyr::if_else(is.na(Dogwood), 1e-5, Dogwood))
