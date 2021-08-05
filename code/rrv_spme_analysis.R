####SETUP####
pkgs <-
  c("tidyverse",
    "readxl",
    "writexl")

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# reading in master datasheet
df <-
  read_excel("data/ofv_pca_table.xlsx", col_types = "guess")



 
# #cleanup
# rm(list = ls())