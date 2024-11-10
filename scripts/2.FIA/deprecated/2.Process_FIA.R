## Joining FIA data
# User manual: https://www.fs.usda.gov/rm/pubs/rmrs_gtr245.pdf

rm(list = ls())

#### ILLINOIS ####

# Condition table
IL_COND <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IL_COND.csv')
# Plot table
IL_PLOT <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IL_PLOT.csv')
# Tree table
IL_TREE <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IL_TREE.csv')
# Species tables
SPECIES <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/REF_SPECIES.csv')
SPECIES_GROUP <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/REF_SPECIES_GROUP.csv')

## Start with PLOT table
IL_PLOT2 <- IL_PLOT |>
  dplyr::select(CN, # primary key
                PREV_PLT_CN, # previous inventory's plot record
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # unique key
                PLOT_STATUS_CD, # sampling status of plot, filter 1 = sampled
                MEASYEAR, # measurement year
                LAT, LON, # NAD 83
                ELEV, # elevation, m
                QA_STATUS, # type of plot data collected
                SAMP_METHOD_CD) |> # filter for 1 = field visited
  dplyr::filter(PLOT_STATUS_CD == 1,
                SAMP_METHOD_CD == 1) |>
  dplyr::select(-PLOT_STATUS_CD, -SAMP_METHOD_CD) |>
  dplyr::rename(PLT_CN = CN)

## COND table

IL_COND2 <- IL_COND |>
  dplyr::select(CN, # primary key
                PLT_CN, # foreign key
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, # natural key
                CONDID, # unique and natural key
                COND_STATUS_CD, # basic land cover, filter 1 = forest land
                RESERVCD, OWNCD, OWNGRPCD, FORTYPCD, MAPDEN, STDSZCD, # all unique to condition
                SLOPE, ASPECT, # topography (degrees)
                BALIVE) |> # live basal area
  dplyr::filter(COND_STATUS_CD == 1) |>
  dplyr::select(-COND_STATUS_CD)

## TREE table
IL_TREE2 <- IL_TREE |>
  dplyr::select(CN, # primary key
                PLT_CN, # unique key
                PREV_TRE_CN, # previous tree sequence number
                INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, # natural key
                CONDID, # foreign key
                STATUSCD, # live cut or dead, filter 1 = live tree
                SPCD, # tree species code
                SPGRPCD, # tree species group code
                DIA, # diameter
                P2A_GRM_FLG, # if the tree is part of periodic inventory but not in plot?
                TPA_UNADJ) |> # number of trees per acre theoretically?
  dplyr::filter(STATUSCD == 1,
                P2A_GRM_FLG != 'Y') |>
  dplyr::select(-STATUSCD, -P2A_GRM_FLG)

## Combine TREE with COND

IL <- IL_TREE2 |>
  dplyr::left_join(y = IL_COND2, 
                   by = c('PLT_CN', 'CONDID', # joining variables
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT')) |> # should be the same
  dplyr::left_join(y = IL_PLOT2,
                   by = c('PLT_CN', # joining variable
                          'INVYR', 'STATECD', 'UNITCD', 'COUNTYCD', 'PLOT'))

## Plot
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:4269')

IL |>
  dplyr::filter(!is.na(BALIVE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = LON, y = LAT, color = BALIVE)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

IL |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = LON, y = LAT, color = DIA)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

## Add in species names

SPECIES <- dplyr::select(SPECIES, SPCD, COMMON_NAME, GENUS, SCIENTIFIC_NAME)
SPECIES_GROUP <- dplyr::select(SPECIES_GROUP, SPGRPCD, NAME, CLASS)  

IL <- IL |>
  dplyr::left_join(y = SPECIES, by = 'SPCD') |>
  dplyr::left_join(y = SPECIES_GROUP, by = 'SPGRPCD')

## Plot
IL |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = LON, y = LAT, color = NAME)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
