#### DEPRECATED
## Joining FIA data
# User manual: at data/raw/FIA_08082024/manual.pdf

rm(list = ls())

#### ILLINOIS ####

### Load data ###

# Condition table
IL_COND <- readr::read_csv('data/raw/FIA_08082024/IL_COND.csv')
# Plot table
IL_PLOT <- readr::read_csv('data/raw/FIA_08082024/IL_PLOT.csv')
# Tree table
IL_TREE <- readr::read_csv('data/raw/FIA_08082024/IL_TREE.csv')
# Species tables
SPECIES <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES.csv')

### PLOT table ###

IL_PLOT2 <- IL_PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                CYCLE, SUBCYCLE, # inventory cycle and subcycle number, retained in preivous PalEON analyses
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                QA_STATUS, # quality assurance
                SAMP_METHOD_CD) |> # filter for 1 = field visited
  dplyr::rename(PLT_CN = CN)

### COND table ###

IL_COND2 <- IL_COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key (condition to plot)
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                BALIVE) |> # live basal area
  dplyr::rename(COND_CN = CN)

### TREE table ###

IL_TREE2 <- IL_TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, TREE, # unique key (PLT_CN = foreign key, TREE to PLOT)
                CONDID, # part of foreign key acc. to FIA personnel
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                DIA, # diameter
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

### Combine tables ###

# Combine plot and condition
# More than one condition can occur on the same plot
IL_PLOTCOND <- IL_COND2 |>
  dplyr::full_join(y = IL_PLOT2,
                   by = c('PLT_CN', # foreign key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Combine plot & condition and tree
# Each tree should only fall into one condition
IL <- IL_TREE2 |>
  dplyr::left_join(y = IL_PLOTCOND,
                   by = c('PLT_CN', 'CONDID', # foreign key + CONDID at recommendation of FIA staff
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Filter out unwanted data
IL <- IL |>
  dplyr::filter(PLOT_STATUS_CD == 1, # 1 = sampled
                SAMP_METHOD_CD == 1, # 1 = field visited
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD == 1, # 1 = forested land
                STATUSCD == 1, # 1 = live tree
                P2A_GRM_FLG %in% c('N', NA)) |> # TO DO: MAKE SURE THAT FILTERING HERE IS CORRECT
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD, -P2A_GRM_FLG)

### Add in species names ###

SPECIES <- dplyr::select(SPECIES, SPCD, COMMON_NAME, GENUS, SCIENTIFIC_NAME)
SPECIES_GROUP <- dplyr::select(SPECIES_GROUP, SPGRPCD, NAME, CLASS)  

IL <- IL |>
  dplyr::left_join(y = SPECIES, by = 'SPCD') |>
  dplyr::left_join(y = SPECIES_GROUP, by = 'SPGRPCD')

### Save ###

save(IL, file = 'data/intermediate/IL_COND_PLOT_TREE_SPECIES.RData')

#### INDIANA ####

rm(list = ls())

### Load data ###

# Condition table
IN_COND <- readr::read_csv('data/raw/FIA_08082024/IN_COND.csv')
# Plot table
IN_PLOT <- readr::read_csv('data/raw/FIA_08082024/IN_PLOT.csv')
# Tree table
IN_TREE <- readr::read_csv('data/raw/FIA_08082024/IN_TREE.csv')
# Species tables
SPECIES <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES.csv')
SPECIES_GROUP <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES_GROUP.csv')

### PLOT table ###

IN_PLOT2 <- IN_PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                ELEV, # elevation, m
                QA_STATUS, # quality assurance
                SAMP_METHOD_CD) |> # filter for 1 = field visisted
  dplyr::rename(PLT_CN = CN)

### COND table ###

IN_COND2 <- IN_COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key (condition to plot)
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                SLOPE, ASPECT, # topography (degrees)
                BALIVE) |> # live basal area
  dplyr::rename(COND_CN = CN)

### TREE table ###

IN_TREE2 <- IN_TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, TREE, # unique key (PLT_CN = foreign key, TREE to PLOT)
                CONDID, # part of foreign key acc. to FIA personnel
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                SPGRPCD, # tree species group code
                DIA, # diameter
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

### Combine tables ###

# Combine plot and condition
# More than one condition can occur on the same plot
IN_PLOTCOND <- IN_COND2 |>
  dplyr::full_join(y = IN_PLOT2,
                   by = c('PLT_CN', # foreign key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Combine plot & condition and ree
# Each tree should only fall into one condition
IN <- IN_TREE2 |>
  dplyr::left_join(y = IN_PLOTCOND,
                   by = c('PLT_CN', 'CONDID', # foreign key + CONDID at recommendation of FIA staff
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Filter out unwanted data
IN <- IN |>
  dplyr::filter(PLOT_STATUS_CD == 1, # 1 = sampled
                SAMP_METHOD_CD == 1, # 1 = field visited
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD == 1, # 1 = forested land
                STATUSCD == 1, # 1 = live tree
                P2A_GRM_FLG %in% c('N', NA)) |> # TO DO: MAKE SURE THAT FILTERING HERE IS CORRECT
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD, -P2A_GRM_FLG)

### Add in species names ###

SPECIES <- dplyr::select(SPECIES, SPCD, COMMON_NAME, GENUS, SCIENTIFIC_NAME)
SPECIES_GROUP <- dplyr::select(SPECIES_GROUP, SPGRPCD, NAME, CLASS)

IN <- IN |>
  dplyr::left_join(y = SPECIES, by = 'SPCD') |>
  dplyr::left_join(y = SPECIES_GROUP, by = 'SPGRPCD')

### Save ###

save(IN, file = 'data/intermediate/IN_COND_PLOT_TREE_SPECIES.RData')

#### MICHIGAN ####

rm(list = ls())

### Load data ###

# Condition table
MI_COND <- readr::read_csv('data/raw/FIA_08082024/MI_COND.csv')
# Plot table
MI_PLOT <- readr::read_csv('data/raw/FIA_08082024/MI_PLOT.csv')
# Tree table
MI_TREE <- readr::read_csv('data/raw/FIA_08082024/MI_TREE.csv')
# Species tables
SPECIES <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES.csv')
SPECIES_GROUP <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES_GROUP.csv')

### PLOT table ###

MI_PLOT2 <- MI_PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                ELEV, # elevation, m
                QA_STATUS, # quality assurance
                SAMP_METHOD_CD) |> # filter for 1 = field visited
  dplyr::rename(PLT_CN = CN)

### COND table ###

MI_COND2 <- MI_COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key (condition to plot)
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                SLOPE, ASPECT, # topography (degrees)
                BALIVE) |> # live basal area
  dplyr::rename(COND_CN = CN)

### TREE table ###

MI_TREE2 <- MI_TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, TREE, # unique key (PLT_CN = foreign key, TREE to PLOT)
                CONDID, # part of foreign key acc. to FIA personnel
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                SPGRPCD, # tree species group code
                DIA, # diameter
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

### Combine tables ###

# Combine plot and condition
# More than one condition can occur on the same plot
MI_PLOTCOND <- MI_COND2 |>
  dplyr::full_join(y = MI_PLOT2,
                   by = c('PLT_CN', # foreign key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Combine plot & condition and tree
# Each tree should only fall into one condition
MI <- MI_TREE2 |>
  dplyr::left_join(y = MI_PLOTCOND,
                   by = c('PLT_CN', 'CONDID', # foreign key + CONDID at recommendation of FIA staff
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Filter out unwanted data
MI <- MI |>
  dplyr::filter(PLOT_STATUS_CD == 1, # 1 = sampled
                SAMP_METHOD_CD == 1, # 1 = field visited
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD == 1, # 1 = forested land
                STATUSCD == 1, # 1 = live tree
                P2A_GRM_FLG %in% c('N', NA)) |> # TO DO: MAKE SURE THAT FILTERING HERE IS CORRECT
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD, -P2A_GRM_FLG)

### Add in species names ###

SPECIES <- dplyr::select(SPECIES, SPCD, COMMON_NAME, GENUS, SCIENTIFIC_NAME)
SPECIES_GROUP <- dplyr::select(SPECIES_GROUP, SPGRPCD, NAME, CLASS)  

MI <- MI |>
  dplyr::left_join(y = SPECIES, by = 'SPCD') |>
  dplyr::left_join(y = SPECIES_GROUP, by = 'SPGRPCD')

### Save ###

save(MI, file = 'data/intermediate/MI_COND_PLOT_TREE_SPECIES.RData')

#### MINNESOTA ####

rm(list = ls())

### Load data ###

# Condition table
MN_COND <- readr::read_csv('data/raw/FIA_08082024/MN_COND.csv')
# Plot table
MN_PLOT <- readr::read_csv('data/raw/FIA_08082024/MN_PLOT.csv')
# Tree table
MN_TREE <- readr::read_csv('data/raw/FIA_08082024/MN_TREE.csv')
# Species tables
SPECIES <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES.csv')
SPECIES_GROUP <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES_GROUP.csv')

### PLOT table ###

MN_PLOT2 <- MN_PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                ELEV, # elevation, m
                QA_STATUS, # quality assurance
                SAMP_METHOD_CD) |> # filter for 1 = field visited
  dplyr::rename(PLT_CN = CN)

### COND table ###

MN_COND2 <- MN_COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key (condition to plot)
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                SLOPE, ASPECT, # topography (degrees)
                BALIVE) |> # live basal area
  dplyr::rename(COND_CN = CN)

### TREE table ###

### TREE table ###

MN_TREE2 <- MN_TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, TREE, # unique key (PLT_CN = foreign key, TREE to PLOT)
                CONDID, # part of foreign key acc. to FIA personnel
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                SPGRPCD, # tree species group code
                DIA, # diameter
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

### Combine tables ###

# Combine plot and condition
# More than one condition can occur on the same plot
MN_PLOTCOND <- MN_COND2 |>
  dplyr::full_join(y = MN_PLOT2,
                   by = c('PLT_CN', # foreign key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Combine plot & condition and tree
# Each tree should only fall into one condition
MN <- MN_TREE2 |>
  dplyr::left_join(y = MN_PLOTCOND,
                   by = c('PLT_CN', 'CONDID', # foreign key + CONDID at recommendation of FIA staff
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Filter out unwanted data
MN <- MN |>
  dplyr::filter(PLOT_STATUS_CD == 1, # 1 = sampled
                SAMP_METHOD_CD == 1, # 1 = field visited
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD == 1, # 1 = forested land
                STATUSCD == 1, # 1 = live tree
                P2A_GRM_FLG %in% c('N', NA)) |> # TO DO: MAKE SURE THAT FILTERING HERE IS CORRECT
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD, -P2A_GRM_FLG)

### Add in species names ###

SPECIES <- dplyr::select(SPECIES, SPCD, COMMON_NAME, GENUS, SCIENTIFIC_NAME)
SPECIES_GROUP <- dplyr::select(SPECIES_GROUP, SPGRPCD, NAME, CLASS)  

MN <- MN |>
  dplyr::left_join(y = SPECIES, by = 'SPCD') |>
  dplyr::left_join(y = SPECIES_GROUP, by = 'SPGRPCD')

### Save ###

save(MN, file = 'data/intermediate/MN_COND_PLOT_TREE_SPECIES.RData')

#### WISCONSIN ####

rm(list = ls())

### Load data ###

# Condition table
WI_COND <- readr::read_csv('data/raw/FIA_08082024/WI_COND.csv')
# Plot table
WI_PLOT <- readr::read_csv('data/raw/FIA_08082024/WI_PLOT.csv')
# Tree table
WI_TREE <- readr::read_csv('data/raw/FIA_08082024/WI_TREE.csv')
# Species tables
SPECIES <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES.csv')
SPECIES_GROUP <- readr::read_csv('data/raw/FIA_08082024/REF_SPECIES_GROUP.csv')

### PLOT table ###

WI_PLOT2 <- WI_PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                ELEV, # elevation, m
                QA_STATUS, # quality assurance
                SAMP_METHOD_CD) |> # filter for 1 = field visited
  dplyr::rename(PLT_CN = CN)

### COND table ###

WI_COND2 <- WI_COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key (condition to plot)
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                SLOPE, ASPECT, # topography (degrees)
                BALIVE) |> # live basal area
  dplyr::rename(COND_CN = CN)

### TREE table ###

WI_TREE2 <- WI_TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, TREE, # unique key (PLT_CN = foreign key, TREE to PLOT)
                CONDID, # part of foreign key acc. to FIA personnel
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                SPGRPCD, # tree species group code
                DIA, # diameter
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

### Combine tables ###

# Combine plot and condition
# More than one condition can occur on the same plot
WI_PLOTCOND <- WI_COND2 |>
  dplyr::full_join(y = WI_PLOT2,
                   by = c('PLT_CN', # foreign key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Combine plot & condition and tree
# Each tree should only fall into one condition
WI <- WI_TREE2 |>
  dplyr::left_join(y = WI_PLOTCOND,
                   by = c('PLT_CN', 'CONDID', # foreign key + CONDID at recommendation of FIA staff
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Filter out unwanted data
WI <- WI |>
  dplyr::filter(PLOT_STATUS_CD == 1, # 1 = sampled
                SAMP_METHOD_CD == 1, # 1 = field visited
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD == 1, # 1 = forested land
                STATUSCD == 1, # 1 = live tree
                P2A_GRM_FLG %in% c('N', NA)) |> # TO DO: MAKE SURE THAT FILTERING HERE IS CORRECT
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD, -P2A_GRM_FLG)

### Add in species names ###

SPECIES <- dplyr::select(SPECIES, SPCD, COMMON_NAME, GENUS, SCIENTIFIC_NAME)
SPECIES_GROUP <- dplyr::select(SPECIES_GROUP, SPGRPCD, NAME, CLASS)  

WI <- WI |>
  dplyr::left_join(y = SPECIES, by = 'SPCD') |>
  dplyr::left_join(y = SPECIES_GROUP, by = 'SPGRPCD')

### Save ###

save(WI, file = 'data/intermediate/WI_COND_PLOT_TREE_SPECIES.RData')

#### Combine ####

rm(list = ls())

# Load individual states
load(file = 'data/intermediate/IL_COND_PLOT_TREE_SPECIES.RData')
load(file = 'data/intermediate/IN_COND_PLOT_TREE_SPECIES.RData')
load(file = 'data/intermediate/MI_COND_PLOT_TREE_SPECIES.RData')
load(file = 'data/intermediate/MN_COND_PLOT_TREE_SPECIES.RData')
load(file = 'data/intermediate/WI_COND_PLOT_TREE_SPECIES.RData')

# Combine
FIA_combined <- rbind(IL, IN, MI, MN, WI)

# Save
save(FIA_combined, file = 'data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData')