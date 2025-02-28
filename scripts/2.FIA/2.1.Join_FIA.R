#### STEP 2-1

## Joining FIA data
## Downloaded COND, PLOT, SUBPLOT, TREE tables
## Download data: 08 August 2024
## Join all tables using relation IDs

## User manual: data/raw/FIA_08082024/manual.pdf
## This is the manual I followed for how to join
## supplemented with information from FIA/USFS employees
## on order and variables to pay attention to

## 1. Load data
## 2. Format PLOT tables
## 3. Format COND tables
## 4. Format SUBPLOT tables
## 5. Format TREE tables
## 6. Combine tables
## 7. Filter data
## 8. Save output

## Input: data/raw/FIA_08082024/IL_COND.csv
##        data/raw/FIA_08082024/IN_COND.csv
##        data/raw/FIA_08082024/MI_COND.csv
##        data/raw/FIA_08082024/MN_COND.csv
##        data/raw/FIA_08082024/WI_COND.csv
## Condition tables for each state in our study domain
## Contains information on the conditions present on each plot
## Used to filter for the forested condition

## Input: data/raw/FIA_08082024/IL_PLOT.csv
##        data/raw/FIA_08082024/IN_PLOT.csv
##        data/raw/FIA_08082024/MI_PLOT.csv
##        data/raw/FIA_08082024/MN_PLOT.csv
##        data/raw/FIA_08082024/WI_PLOT.csv
## Plot tables for each state in our study domain
## Contains information about the whole plot
## Used for coordinates and to filter sampled plots

## Input: data/raw/FIA_08082024/IL_SUBPLOT.csv
##        data/raw/FIA_08082024/IN_SUBPLOT.csv
##        data/raw/FIA_08082024/MI_SUBPLOT.csv
##        data/raw/FIA_08082024/MN_SUBPLOT.csv
##        data/raw/FIA_08082024/WI_SUBPLOT.csv
## Subplot tables for each state in our study domain
## Contains which subplot each tree is on
## Used to filter for only the main subplots that can be
## used to calculate stand-level variables

## Input: data/raw/FIA_08082024/IL_TREE.csv
##        data/raw/FIA_08082024/IN_TREE.csv
##        data/raw/FIA_08082024/MI_TREE.csv
##        data/raw/FIA_08082024/MN_TREE.csv
##        data/raw/FIA_08082024/WI_TREE.csv
## Tree tables for each state in our study domain
## Contains information on each tree in the plots/subplots
## Used to find total stem density and relative abundance from
## TPA_UNADJ and species identity

## Output: data/intermediate/FIA/combined_COND_PLOT_TREE_SPECIES.RData
## Contains all tree-level data from which total stem density
## and relative abundance are calculated
## Used in 2.2.Estimate_density_composition.R

rm(list = ls())

#### 1. Load data ####

# Condition tables
IL_COND <- readr::read_csv('data/raw/FIA_08082024/IL_COND.csv')
IN_COND <- readr::read_csv('data/raw/FIA_08082024/IN_COND.csv')
MI_COND <- readr::read_csv('data/raw/FIA_08082024/MI_COND.csv')
MN_COND <- readr::read_csv('data/raw/FIA_08082024/MN_COND.csv')
WI_COND <- readr::read_csv('data/raw/FIA_08082024/WI_COND.csv')

# Plot tables
IL_PLOT <- readr::read_csv('data/raw/FIA_08082024/IL_PLOT.csv')
IN_PLOT <- readr::read_csv('data/raw/FIA_08082024/IN_PLOT.csv')
MI_PLOT <- readr::read_csv('data/raw/FIA_08082024/MI_PLOT.csv')
MN_PLOT <- readr::read_csv('data/raw/FIA_08082024/MN_PLOT.csv')
WI_PLOT <- readr::read_csv('data/raw/FIA_08082024/WI_PLOT.csv')

# Subplot tables
IL_SUBPLOT <- readr::read_csv('data/raw/FIA_08082024/IL_SUBPLOT.csv')
IN_SUBPLOT <- readr::read_csv('data/raw/FIA_08082024/IN_SUBPLOT.csv')
MI_SUBPLOT <- readr::read_csv('data/raw/FIA_08082024/MI_SUBPLOT.csv')
MN_SUBPLOT <- readr::read_csv('data/raw/FIA_08082024/MN_SUBPLOT.csv')
WI_SUBPLOT <- readr::read_csv('data/raw/FIA_08082024/WI_SUBPLOT.csv')

# Tree tables
IL_TREE <- readr::read_csv('data/raw/FIA_08082024/IL_TREE.csv')
IN_TREE <- readr::read_csv('data/raw/FIA_08082024/IN_TREE.csv')
MI_TREE <- readr::read_csv('data/raw/FIA_08082024/MI_TREE.csv')
MN_TREE <- readr::read_csv('data/raw/FIA_08082024/MN_TREE.csv')
WI_TREE <- readr::read_csv('data/raw/FIA_08082024/WI_TREE.csv')

#### 2. Format PLOT tables ####

# Combine
PLOT <- rbind(IL_PLOT, IN_PLOT, MI_PLOT, MN_PLOT, WI_PLOT)

# Format
PLOT2 <- PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                CYCLE, SUBCYCLE, # inventory cycle and subcycle number, retained in previous PalEON analyses
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                QA_STATUS, # quality assurance
                SAMP_METHOD_CD) |> # filter for 1 = field visited
  dplyr::rename(PLT_CN = CN)

#### 3. Format COND tables ####

# Combine
COND <- rbind(IL_COND, IN_COND, MI_COND, MN_COND, WI_COND)

# Format
COND2 <- COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key (condition to plot)
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                BALIVE) |> # live basal area
  dplyr::rename(COND_CN = CN)

#### 4. Format SUBPLOT tables ####

# Combine
SUBPLOT <- rbind(IL_SUBPLOT, IN_SUBPLOT, MI_SUBPLOT, MN_SUBPLOT, WI_SUBPLOT)

# Format
SUBPLOT2 <- SUBPLOT |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, # unique key and foreign key (subplot to plot)
                STATECD, INVYR, UNITCD, COUNTYCD, PLOT, # natural key
                SUBPCOND, MICRCOND, MACRCOND) |> # foreign key (subplot to condition)
  dplyr::rename(SUBP_CN = CN)

#### 5. Format TREE tables ####

# Combine
TREE <- rbind(IL_TREE, IN_TREE, MI_TREE, MN_TREE, WI_TREE)

# Format
TREE2 <- TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, SUBP, TREE, # unique key (PLT_CN = foreign key, TREE to PLOT)
                CONDID, # part of foreign key acc. to FIA personnel
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                DIA, # diameter
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

#### 6. Combine tables ####

# Assign PLOT to each TREE
TREE_PLOT <- TREE2 |>
  dplyr::left_join(y = PLOT2,
                   by = c('PLT_CN', # unique key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT'))

# Assign COND to each TREE
TREE_PLOT_COND <- TREE_PLOT |>
  dplyr::left_join(y = COND2,
                   by = c('PLT_CN', 'CONDID',
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT'))

# Assign SUBPLOT to each TREE
TOTAL_FIA <- TREE_PLOT_COND |>
  dplyr::left_join(y = SUBPLOT2,
                   by = c('PLT_CN', 'SUBP', # unique key
                          'STATECD', 'INVYR', 'UNITCD', 'COUNTYCD', 'PLOT'))

#### 7. Filter data ####

TOTAL_FIA <- TOTAL_FIA |>
  dplyr::filter(PLOT_STATUS_CD %in% c(1, NA), # 1 = sampled and old plots (= NA)
                SAMP_METHOD_CD %in% c(1, NA), # 1 = field visited and old plots (= NA)
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD %in% c(1, NA), # 1 = forested land and old plots (= NA)
                SUBP %in% c(1,2,3,4), # keep only main 4 subplots (recommended by FIA staff)
                STATUSCD == 1) |> #, # 1 = live tree
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD) |>
  dplyr::mutate(DIA = DIA * 2.54)

#### 8. Save ####

save(TOTAL_FIA, file = 'data/intermediate/FIA/combined_COND_PLOT_TREE_SPECIES.RData')
