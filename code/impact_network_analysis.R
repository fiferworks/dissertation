####IMPACT THROUGH NETWORKS####
# a list of packages used
pkgs <-
  c(
    'tidyverse',
    'sf',
    'ggmap',
    'ggplot2',
    'ggthemes',
    'extrafont',
    'showtext',
    'leaflet',
    'leafsync',
    'dplyr',
    'readxl',
    'mapview',
    'htmlwidgets',
    'viridis'
  )

# checks to see if you have the packages installed,
# if you don't, it will try to install them
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# #you need to install phantomjs for the interactive maps
# webshot::install_phantomjs()

#loading required packages
lapply(pkgs, library, character.only = TRUE)