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
  read_excel("data/rrd_spme_master_datasheet.xlsx", col_types = "guess")

####DATA CLEANING####
# removing unidentified peaks
df <- df %>% filter(`Peak Name` > 0)

# assigning NAS to true zeros
df$`IS Relative Area (%)`[is.na(df$`IS Relative Area (%)`)] <- 0

# dropping blanks
df <- df %>% filter(`Injection Type` != 'blank')

# removing Nonyl Acetate Standard (it is in every sample)
df <- df %>% filter(`Peak Name` != 'Nonyl Acetate')

# changing remaining columns to factors
df <- df %>%
  mutate_if(is.character, as.factor)

####CREATING PIVOT TABLE####
df <- df %>% select("Sample", `IS Relative Area (%)`, `Peak Name`) %>%
  pivot_wider(
    names_from = `Peak Name`,
    values_from = `IS Relative Area (%)`,
    values_fill = 0,
    values_fn = sum
  )

#saving datasheet as excel spreadsheet
write_xlsx(df, "data/ofv_pca_table.xlsx")

#cleanup
rm(list = ls())