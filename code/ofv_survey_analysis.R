#a list of packages used for this script
pkgs <-
  c('tidyverse', 'sf', 'devtools')

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)

# #install and library INA
# devtools::install_github("GarrettLab/INApreliminary")
# library(INApreliminary)

#####LOADING REQUIRED FILES####
df <- read_xlsx('ofv_clean_datasheet.xlsx')

#making sure R knows what variables are factors
df$symptoms <- as_factor(df$symptoms)
df$plant_cv <- as_factor(df$plant_cv)
df$family <- as_factor(df$family)
df$order <- as_factor(df$order)

#selecting only Tallahassee Museum sites
#str_detect with filter after creating a single string from 'muse' collapsed by | (OR)
m <- c('muse')
tm <- df %>%
  filter(str_detect(id, str_c(m, collapse = "|")))

#filtering by plant spp
filter(tm, )

# genmovnet


#

# #cleanup
# rm(list = ls())