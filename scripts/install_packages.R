# Install packages required through full analysis
# Installing specific versions except where it threw an error

if(!require(remotes)) install.packages('remotes')
library(remotes)

remotes::install_version(package = 'cowplot',
                         version = '1.1.3')
remotes::install_version(package = 'dplyr',
                         version = '1.1.4')
install.packages('fields') # version 16.2
remotes::install_version(package = 'ggplot2',
                         version = '3.5.1')
remotes::install_version(package = 'maps',
                         version = '3.4.2')
remotes::install_version(package = 'MASS',
                         version = '7.3.61')
install.packages('mgcv') # version 1.9.1
install.packages('ncdf4') # version 1.23
remotes::install_version(package = 'randomForestSRC',
                         version = '3.3.1')
remotes::install_version(package = 'readr',
                         version = '2.1.5')
remotes::install_version(package = 'reshape2',
                         version = '1.4.4')
install.packages('sf') # version 1.0.19
remotes::install_version(package = 'tibble',
                         version = '3.2.1')
remotes::install_version(package = 'tidyr',
                         version = '1.3.1')
remotes::install_version(package = 'tune',
                         version = '1.2.1')
