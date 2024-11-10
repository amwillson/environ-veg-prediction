## Processing FIA data

rm(list = ls())

# Load PLOT tables
IL_PLOT <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IL_PLOT.csv')
IN_PLOT <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IN_PLOT.csv')
MI_PLOT <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/MI_PLOT.csv')
MN_PLOT <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/MN_PLOT.csv')
WI_PLOT <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/WI_PLOT.csv')

# Load COND tables
IL_COND <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IL_COND.csv')
IN_COND <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/IN_COND.csv')
MI_COND <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/MI_COND.csv')
MN_COND <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/MN_COND.csv')
WI_COND <- readr::read_csv('/Volumes/FileBackup/SDM_bigdata/FIA/WI_COND.csv')

# Join PLOT and COND tables
IL_JOIN <- IL_PLOT |>
  dplyr::rename(PLT_CN = CN) |>
  dplyr::select(PLOT, PLT_CN, # unique sequence number for each plot record)
                INVYR, # inventory year
                STATECD, # state code
                PLOT, # unique identifier for each plot
                MEASYEAR, # measurement year (can be different from inventory year?)
                QA_STATUS, # quality assurance
                LAT, LON) |> # NAD 83- within ~ +/- 1 mile of actual plot
  dplyr::left_join(y = IL_COND, by = c('PLT_CN')) |>
  dplyr::filter(COND_STATUS_CD == 1) |>
  dplyr::filter(QA_STATUS %in% c(NA, 1, 7)) |>
  dplyr::select(-PLOT.y, -INVYR.y, -STATECD.y) |>
  dplyr::rename(PLOT = PLOT.x,
                INVYR = INVYR.x,
                STATECD = STATECD.x)
IN_JOIN <- IN_PLOT |>
  dplyr::rename(PLT_CN = CN) |>
  dplyr::select(PLOT, PLT_CN, INVYR, STATECD, PLOT, MEASYEAR, QA_STATUS, LAT, LON) |>
  dplyr::left_join(y = IN_COND, by = c('PLT_CN')) |>
  dplyr::filter(COND_STATUS_CD == 1) |>
  dplyr::filter(QA_STATUS %in% c(NA, 1, 7)) |>
  dplyr::select(-PLOT.y, -INVYR.y, -STATECD.y) |>
  dplyr::rename(PLOT = PLOT.x,
                INVYR = INVYR.x,
                STATECD = STATECD.x)
MI_JOIN <- MI_PLOT |>
  dplyr::rename(PLT_CN = CN) |>
  dplyr::select(PLOT, PLT_CN, INVYR, STATECD, PLOT, MEASYEAR, QA_STATUS, LAT, LON) |>
  dplyr::left_join(y = MI_COND, by = c('PLT_CN')) |>
  dplyr::filter(COND_STATUS_CD == 1) |>
  dplyr::filter(QA_STATUS %in% c(NA, 1, 7)) |>
  dplyr::select(-PLOT.y, -INVYR.y, -STATECD.y) |>
  dplyr::rename(PLOT = PLOT.x,
                INVYR = INVYR.x,
                STATECD = STATECD.x)
MN_JOIN <- MN_PLOT |>
  dplyr::rename(PLT_CN = CN) |>
  dplyr::select(PLOT, PLT_CN, INVYR, STATECD, PLOT, MEASYEAR, QA_STATUS, LAT, LON) |>
  dplyr::left_join(y = MN_COND, by = c('PLT_CN')) |>
  dplyr::filter(COND_STATUS_CD == 1) |>
  dplyr::filter(QA_STATUS %in% c(NA, 1, 7)) |>
  dplyr::select(-PLOT.y, -INVYR.y, -STATECD.y) |>
  dplyr::rename(PLOT = PLOT.x,
                INVYR = INVYR.x,
                STATECD = STATECD.x)
WI_JOIN <- WI_PLOT |>
  dplyr::rename(PLT_CN = CN) |>
  dplyr::select(PLOT, PLT_CN, INVYR, STATECD, PLOT, MEASYEAR, QA_STATUS, LAT, LON) |>
  dplyr::left_join(y = WI_COND, by = c('PLT_CN')) |>
  dplyr::filter(COND_STATUS_CD == 1) |>
  dplyr::filter(QA_STATUS %in% c(NA, 1, 7)) |>
  dplyr::select(-PLOT.y, -INVYR.y, -STATECD.y) |>
  dplyr::rename(PLOT = PLOT.x,
                INVYR = INVYR.x,
                STATECD = STATECD.x)

# Join
all(colnames(IL_JOIN) == colnames(IN_JOIN))
all(colnames(IN_JOIN) == colnames(MI_JOIN))
all(colnames(MI_JOIN) == colnames(MN_JOIN))
all(colnames(MN_JOIN) == colnames(WI_JOIN))

PLOT <- rbind(IL_JOIN, IN_JOIN, MI_JOIN, MN_JOIN, WI_JOIN)

# Select relevant columns
PLOT <- dplyr::select(PLOT, PLOT, PLT_CN, INVYR, STATECD, PLOT, MEASYEAR, LAT, LON)

# Specify CRS & reproject
PLOT <- sf::st_as_sf(PLOT, coords = c('LON', 'LAT'),
                     crs = 'EPSG:4269')
PLOT <- sf::st_transform(PLOT, crs = 'EPSG:4326')
PLOT <- sfheaders::sf_to_df(PLOT, fill = TRUE)
PLOT <- dplyr::select(PLOT, -sfg_id, -point_id)

# Map of states
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

PLOT |>
  dplyr::group_by(PLOT) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::coord_sf(crs = 'EPSG:4326')
