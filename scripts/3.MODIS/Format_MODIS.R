## Loading in MODIS data

rm(list = ls())

# Function for reading in data from Python
reticulate::source_python('scripts/read_pickle.py')

#### Cell 1: h10v04 ####

# Load latitudes
lats <- read_pickle_file(file = '/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lats_h10v04.pickle')
# Load longitudes
lons <- read_pickle_file(file = '/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lons_h10v04.pickle')
# Load datasets
modis <- read_pickle_file(file = '/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/modis_h10v04.pickle')

# Dimensions of modis datasets
# 23 = years
# 5 = datasets
# 2201 = lats
# 2584 = lons
dim(modis)

# Add dimnames
dimnames(modis) <- list(1:23,
                        c('LC_Type1', 'LC_Type2', 'LC_Type3',
                          'QC', 'LW'),
                        lats,
                        lons)

# Melt to dataframe
modis_df <- reshape2::melt(modis)
# Add column names
colnames(modis_df) <- c('Year', 'Dataset', 'y', 'x', 'Value')

# Remove modis array
rm(modis)

# Pivot wider so each dataset has its own column
modis_wide <- modis_df |>
  tidyr::pivot_wider(names_from = 'Dataset', values_from = 'Value')

# Filter for:
# 1. LW = 2 (land)
# 2. QC %in% c(0, 9) (land, climate-induced change to forest)
# 3. x & y within maximum extent of study region
