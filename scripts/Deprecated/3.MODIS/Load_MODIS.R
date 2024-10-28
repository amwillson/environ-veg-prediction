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
                        c('LC_Type1', 'QC', 'LW',
                          'LC_Type2', 'LC_Type3'),
                        lats,
                        lons)


# Save for processing on the VM
save(modis, file = '/Volumes/FileBackup/SDM_bigdata/MODIS/R_format/modis_h10v04.RData')

#### Cell 2: h10v05 ####

# Remove previous variables
rm(lats, lons, modis)

# Load latitudes
lats <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lats_h10v05.pickle')
# Load longitudes
lons <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lons_h10v05.pickle')
# Load datasets
modis <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/modis_h10v05.pickle')

# Dimensions of modis datasets
# 23 = years
# 5 = datasets
# 1862 = lats
# 2841 = lons
dim(modis)

# Add dimnames
dimnames(modis) <- list(1:23,
                        c('LC_Type1', 'QC', 'LW',
                          'LC_Type2', 'LC_Type3'),
                        lats,
                        lons)

# Save for processing on the VM
save(modis, file = '/Volumes/FileBackup/SDM_bigdata/MODIS/R_format/modis_h10v05.RData')

#### Cell 3: h11v04 ####

# Remove previous variables
rm(lats, lons, modis)

# Load latitudes
lats <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lats_h11v04.pickle')
# Load longitudes
lons <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lons_h11v04.pickle')
# Load datasets
modis <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/modis_h11v04.pickle')

# Dimensions of modis datasets
# 23 = years
# 5 = datasets
# 1811 = lats
# 2881 = lons
dim(modis)

# Add dimnames
dimnames(modis) <- list(1:23,
                        c('LC_Type1', 'QC', 'LW',
                          'LC_Type2', 'LC_Type3'),
                        lats,
                        lons)

# Save for processing on the VM
save(modis, file = '/Volumes/FileBackup/SDM_bigdata/MODIS/R_format/modis_h11v04.RData')

#### Cell 4: h11v05 ####

# Remove previous variables
rm(lats, lons, modis)

# Load latitudes
lats <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lats_h11v05.pickle')
# Load longitudes
lons <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lons_h11v05.pickle')
# Load datasets
modis <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/modis_h11v05.pickle')

# Dimensions of modis datasets
# 23 = years
# 5 = datasets
# 1657 = lats
# 3052 = lons
dim(modis)

# Add dimnames
dimnames(modis) <- list(1:23,
                        c('LC_Type1', 'QC', 'LW',
                          'LC_Type2', 'LC_Type3'),
                        lats,
                        lons)

# Save for processing on the VM
save(modis, file = '/Volumes/FileBackup/SDM_bigdata/MODIS/R_format/modis_h11v05.RData')

#### Cell 5: h12v04 ####

# Remove previous variables
rm(lats, lons, modis)

# Load latitudes
lats <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lats_h12v04.pickle')
# Load longitudes
lons <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/lons_h12v04.pickle')
# Load datasets
modis <- read_pickle_file('/Volumes/FileBackup/SDM_bigdata/MODIS/MODIS/modis_h12v04.pickle')

# Dimensions of modis datasets
# 23 = years
# 5 = datasets
# 1582 = lats
# 3109 = lons
dim(modis)

# Add dimnames
dimnames(modis) <- list(1:23,
                        c('LC_Type1', 'QC', 'LW',
                          'LC_Type2', 'LC_Type3'),
                        lats,
                        lons)

# Save for processing on the VM
save(modis, file = '/Volumes/FileBackup/SDM_bigdata/MODIS/R_format/modis_h12v04.RData')
