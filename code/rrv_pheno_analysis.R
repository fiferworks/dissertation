#####LOADING PACKAGES####
pkgs <-  c('tidyverse', 'readxl', 'lme4', 'nlme')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####READING IN THE REQUIRED FILES####
df <- read_xlsx('clean_pheno_datasheet.xlsx')

#filter by sites which were pruned
df <-
  df %>% filter(id == 'Pheno 11' |
                  id == 'Pheno 12' |
                  id == 'Pheno 13' | id == 'Pheno 14')

df <- df %>%
  dplyr::select(sample_no, id, month, other_mites, eriophyoids)

df$id <- as_factor(df$id)
df$month <- as_factor(df$month)



lme(eriophyoids ~ month, data = df, random = 1|sample_no)

