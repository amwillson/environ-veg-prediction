## Joining FIA data
## User manual: data/raw/FIA_08082024/manual.pdf

rm(list = ls())

#### Load data ####

# Add in subplot tables
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

# Tree tables
IL_TREE <- readr::read_csv('data/raw/FIA_08082024/IL_TREE.csv')
IN_TREE <- readr::read_csv('data/raw/FIA_08082024/IN_TREE.csv')
MI_TREE <- readr::read_csv('data/raw/FIA_08082024/MI_TREE.csv')
MN_TREE <- readr::read_csv('data/raw/FIA_08082024/MN_TREE.csv')
WI_TREE <- readr::read_csv('data/raw/FIA_08082024/WI_TREE.csv')

#### PLOT tables ####

# Combine
PLOT <- rbind(IL_PLOT, IN_PLOT, MI_PLOT, MN_PLOT, WI_PLOT)

# Format
PLOT2 <- PLOT |>
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

#### COND tables ####

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

#### TREE tables ####

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
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre the tree represents based on sampling design
  dplyr::rename(TREE_CN = CN)

#### Combine tables ####

# Combine plot and condition
# More than one condition can occur on the same plot
PLOTCOND <- COND2 |>
  dplyr::full_join(y = PLOT2,
                   by = c('PLT_CN', # foreign key
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

# Combine plot & condition and tree
# Each tree should only fall into one condition on one plot
TOTAL_FIA <- TREE2 |>
  dplyr::left_join(y = PLOTCOND,
                   by = c('PLT_CN', 'CONDID', # foreign key + CONDID at recommendation of FIA staff
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) # same because of foreign key

#### Filter ####

TOTAL_FIA <- TOTAL_FIA |>
  dplyr::filter(PLOT_STATUS_CD %in% c(1, NA), # 1 = sampled and old plots (= NA)
                SAMP_METHOD_CD %in% c(1, NA), # 1 = field visited and old plots (= NA)
                QA_STATUS %in% c(NA, 1, 7), # Production plot and old plots (= NA)
                COND_STATUS_CD %in% c(1, NA), # 1 = forested land and old plots (= NA)
                STATUSCD == 1, # 1 = live tree
                P2A_GRM_FLG %in% c('N', NA)) |> # TO DO: MAKE SURE THAT FILTERING HERE IS CORRECT
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD, -QA_STATUS,
                -COND_STATUS_CD, -STATUSCD, -P2A_GRM_FLG) |>
  dplyr::mutate(DIA = DIA * 2.54) |>
  dplyr::filter(DIA >= 20.32)

#### Save ####

save(TOTAL_FIA, file = 'data/intermediate/combined_COND_PLOT_TREE_SPECIES.RData')
