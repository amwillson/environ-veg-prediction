#### STEP 2-4

## Splitting FIA data into in-sample and OOS data
## I follow the in-sample OOS splits from the PLS data
## since we want the same OOS samples

## 1. Load data
## 2. Split data
## 3. Save

## Input: data/intermediate/FIA/xydata.RData
## Formatted dataframe with FIA vegetation data, soil, and modern
## climate data for all grid cells

## Input: data/processed/PLS/xydata_out.RData
## Out-of-sample grid cells from historical period
## Used to match up out-of-sample grid cells between time periods

## Output: data/processed/FIA/xydata_in.RData
## In-sample grid cells of the modern period vegetation,
## soil, and climate data
## Used in 5.1.fit_density_allcovar.R, 5.2.fit_density_climcovar.R,
## 5.3.fit_density_redcovar.R, 5.4.fit_density_xycovar.R,
## 5.8.fit_abundance_allcovar.R, 5.9.fit_abundance_climcovar.R,
## 5.10.fit_abundance_redcovar.R, 5.11.fit_abundance_xycovar.R
## 7.1.fit_density_allcovar.R, 7.2.fit_density_climcovar.R,
## 7.3.fit_density_redcovar.R, 7.4.fit_density_xycovar.R.,
## 7.8.fit_abundance_allcovar.R, 7.9.fit_abundance_climcovar.R,
## 7.10.fit_abundance_redcovar.R, 7.11.fit_abundance_xycovar.R

## Output: data/processed/FIA/xydata_out.RData
## Out-of-sample grid cells of the modern period vegetation,
## soil, and climate data
## Used in 4.6.density_modern_predictions.R,
## 4.13.abundance_modern_predictions.R, 5.6.density_modern_predictions.R,
## 5.13.abundance_modern_predictions.R, 6.6.density_modern_predictions.R,
## 6.13.abundance_modern_predictions.R, 7.6.density_modern_predictions.R,
## 7.13.abundance_modern_predictions.R

rm(list = ls())

#### 1. Load data ####

# Load FIA data
load('data/intermediate/FIA/xydata.RData')

# Load PLS OOS data
load('data/processed/PLS/xydata_out.RData')

#### 2. Split data ####

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

#### 3. Save ####

# Save
save(fia_in,
     file = 'data/processed/FIA/xydata_in.RData')
save(fia_oos,
     file = 'data/processed/FIA/xydata_out.RData')
