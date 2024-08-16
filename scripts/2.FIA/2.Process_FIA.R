## Formatting FIA data

## 1. change species
## 2. for each plot & year: calculate stem density (using number of stems & correction factor)
## 3. for each plot & year: calculate fractional composition (taxon stem density / plot stem density)
## 4. average over years
##----new script ----
## 5. find plots within each grid cell
## 6. aggregate to 8km grid
## 7. average stem density & composition over grid cells

rm(list = ls())

load('data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData')

# Calculate stem density
tree_stem_den <- tree_data |>
  dplyr::group_by(spcd, plt_cn) |>
  dplyr::summarize(n2 = dplyr::n(),
                   density2 = n2 * unique(tpa_unadj) * (1/0.404686))
