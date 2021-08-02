#loading required packages
pkgs <-
  c("tidyverse",
    "lubridate",
    "readxl",
    "writexl")

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in the file
df <- read_xlsx('pheno_datasheet.xlsx')

####ARRANGING COLUMNS####

#column for days
df$day <- lubridate::day(df$date)
df$month <- lubridate::month(df$date)
df$year <- lubridate::year(df$date)

####SETTING NAs TO ZEROS####
df$other_mites <- replace_na(df$other_mites, 0)
df$eriophyoids <- replace_na(df$eriophyoids, 0)

#calculating likely dry weights for earlier samples
mn <- mean(df$grams_dry_weight, na.rm = T)
er <- sd(df$grams_dry_weight, na.rm = T)
rndw <- abs(rnorm(24, mean = mn, sd = er))

#replaces the missing values with the simulated dry weights
df$grams_dry_weight <-
  replace(df$grams_dry_weight, is.na(df$grams_dry_weight), rndw)


#calculating mites per gram of dry plant tissue
df$mites_per_gram <-
  (df$eriophyoids + df$other_mites) / df$grams_dry_weight
df$pf_per_gram <- df$eriophyoids / df$grams_dry_weight

####SETTING NEW NAs TO ZEROS####
df$mites_per_gram <- replace_na(df$mites_per_gram, 0)
df$pf_per_gram <- replace_na(df$pf_per_gram, 0)


df <-
  select(
    df,
    'sample_no',
    'id',
    'date',
    'other_mites',
    'eriophyoids',
    'p_fructiphilus',
    'grams_dry_weight',
    'mites_per_gram',
    'pf_per_gram',
    'rose_spp',
    'symptoms',
    'rrv',
    'lon',
    'lat',
    'day',
    'month',
    'year',
    'city',
    'state',
    'county',
    'lon_lat',
    'shade',
    'collector',
    'notes'
  )

#saving
write_xlsx(df, 'clean_pheno_datasheet.xlsx')

#cleanup
rm(list = ls())