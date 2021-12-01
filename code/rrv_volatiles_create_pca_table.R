####SETUP####
pkgs <-
  c("tidyverse",
    "tidyselect")

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# reading in master datasheet
df <- read_csv("data/rrv_volatiles_master_datasheet.csv")

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
df <-
  df %>% select("Sample",
                `IS Relative Area (%)`,
                `Peak Name`,
                'Treatment',
                `Injection Method`) %>%
  pivot_wider(
    names_from = `Peak Name`,
    values_from = `IS Relative Area (%)`,
    values_fill = 0,
    values_fn = sum
  )


df <- df %>%  select("Sample",
                     'Treatment',
                     `Injection Method`, sort(colnames(.)))

#saving datasheet as excel spreadsheet
write_csv(df, "data/rrv_volatiles_pca_table.csv")

#cleanup
rm(list = ls())
