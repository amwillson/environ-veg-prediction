#### STEP 2-4

## Splitting FIA data into in-sample and OOS data
## I follow the in-sample OOS splits from the PLS data
## since we want the same OOS samples

rm(list = ls())

# Load FIA data
load('data/processed/FIA/xydata.RData')

# Load PLS OOS data
load('data/processed/PLS/xydata_out.RData')

# Take grid cells from PLS
pls_oos_cells <- pls_oos |>
  dplyr::select(x, y) |>
  dplyr::mutate(pls_oos = TRUE)

# Add whether or not the cells were OOS in PLS
# period to the modern full dataset
xydata_modern_oos <- xydata_modern |>
  dplyr::left_join(y = pls_oos_cells,
                   by = c('x', 'y')) |>
  dplyr::mutate(pls_oos = dplyr::if_else(is.na(pls_oos), FALSE, pls_oos),
                dataset = dplyr::if_else(pls_oos == TRUE, 'oos', 'insample'))

# Split data
fia_in <- xydata_modern_oos |>
  dplyr::filter(dataset == 'insample') |>
  dplyr::select(-pls_oos, -dataset)
fia_oos <- xydata_modern_oos |>
  dplyr::filter(dataset == 'oos') |>
  dplyr::select(-pls_oos, -dataset)

# Save
save(fia_in,
     file = 'data/processed/FIA/xydata_in.RData')
save(fia_oos,
     file = 'data/processed/FIA/xydata_out.RData')
