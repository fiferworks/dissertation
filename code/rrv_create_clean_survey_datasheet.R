#a list of packages used for this script
pkgs <- c('tidyverse', 'readxl', 'lubridate', 'sf')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in the file
df <- read_excel('data/rrv_mite_survey_master.xlsx')

#making a column for months
df$month <- lubridate::month(df$date, abbr = FALSE, label = TRUE)

#making a column for years
df$year <- lubridate::year(df$date)

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
    'month',
    'year',
    'notes'
  )

#total mite samples
df <-
  df %>%  mutate(total_mites = other_mites + eriophyoids, .after = other_mites)

#labels sites with eriophyoids as p_fructiphilus (we haven't found any other spp so far)
df$p_fructiphilus <-
  if_else(df$eriophyoids >= 1,
          df$p_fructiphilus <- 'Yes',
          df$p_fructiphilus <- 'No')

#saving both files
write_csv(df, 'data/rrv_survey_clean_datasheet.csv')

#getting summary statistics for tables
df_fl <- df %>%  filter(state == 'FL')

#here I am averaging the coordinate positions and other information to compare
#mite populations between years
df_fl <- df_fl %>% group_by(city, year) %>% summarize(
  erios = sum(eriophyoids, na.rm = TRUE),
  se_erios = sd(eriophyoids, na.rm = TRUE) / sqrt(n()),
  other = sum(other_mites, na.rm = TRUE),
  se_other = sd(other_mites, na.rm = TRUE) / sqrt(n()),
  samples = n(),
  totals = sum(total_mites, na.rm = TRUE),
  lon = mean(lon, na.rm = TRUE),
  lat = mean(lat, na.rm = TRUE)
)

df_fl <-
  df_fl %>% mutate(erios_per = erios / samples,
                   .after = samples,
                   pfruct = erios > 0)

write_csv(df_fl, 'data/rrv_survey_fl_table.csv')

#cleanup
rm(list = ls())