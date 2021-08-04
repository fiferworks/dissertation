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
df <- read_excel("data/rrd_spme_master_datasheet.xlsx", col_types = "guess")

# changing remaining columns to factors
df <- df %>%
  mutate_if(is.character, as.factor)

####FIGURE OUT  HOW TO PIVOT WIDER ON THE SAMPLE NAME!!!####

df %>% pivot_wider(id_cols = Sample, names_from = `Peak Name`, values_from = `IS Relative Area (%)`)
