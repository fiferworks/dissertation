#a list of packages used for this script
pkgs <- c('tidyverse', 'readxl', 'writexl')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in the file
df <- read_excel('mite_survey_master.xlsx')

#column for days
df$day <- as.Date(df$date)
df$day <- sub('20\\w\\w\\-\\w\\w\\-', '', df$day)

#making a column for months
df$month <- as.Date(df$date)
df$month <- sub('20\\w\\w\\-', '', df$month)
#removes the dash, and anything that comes after it until the end of the string
df$month <- sub('\\-\\w*$', '', df$month)

#replaces numbers with month names
df$month <- sub('01', 'Jan', df$month)
df$month <- sub('02', 'Feb', df$month)
df$month <- sub('03', 'Mar', df$month)
df$month <- sub('04', 'Apr', df$month)
df$month <- sub('05', 'May', df$month)
df$month <- sub('06', 'Jun', df$month)
df$month <- sub('07', 'Jul', df$month)
df$month <- sub('08', 'Aug', df$month)
df$month <- sub('09', 'Sep', df$month)
df$month <- sub('10', 'Oct', df$month)
df$month <- sub('11', 'Nov', df$month)
df$month <- sub('12', 'Dec', df$month)

#making a column for years
df$year <- as.Date(df$date)
df$year <- sub('\\-\\w\\w\\-\\w*$', '', df$year)

#rearranging the columns
df <-
  dplyr::select(
    df,
    'sample_no',
    'id',
    'eriophyoids',
    'p_fructiphilus',
    'other_mites',
    'rose_spp',
    'symptoms',
    'grams_dry_weight',
    'shade',
    'rrv',
    'city',
    'state',
    'county',
    'lon',
    'lat',
    'lon_lat',
    'collector',
    'date',
    'day',
    'month',
    'year',
    'notes'
  )

#samples with predatory mites
pmites_survey <- filter(df, other_mites >= 1)

#samples with eriophyoid mites
emites_survey <- filter(df, eriophyoids >= 1)

#labels sites with eriophyoids as p_fructiphilus (we haven't found any other spp so far)
df$p_fructiphilus <-
  if_else(df$eriophyoids >= 1,
          df$p_fructiphilus <- 'Yes',
          df$p_fructiphilus <- 'No')

filter(df, eriophyoids >= 1)

#saving both files
write_xlsx(pmites_survey, 'pmites_survey.xlsx')
write_xlsx(emites_survey, 'eriophyoids_survey.xlsx')
write_xlsx(df, 'all_mites_survey.xlsx')

#cleanup
rm(list = ls())