#a list of packages used for this script
pkgs <-  c("tidyverse", "readxl", "lubridate")

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in each sheet into a dataframe
prelim <-
  read_xlsx("data/rrv_olfactometer_martini_data.xlsx", sheet = 1)
sht_1 <-
  read_xlsx("data/rrv_olfactometer_fife_data.xlsx", sheet = 1)
sht_2 <-
  read_xlsx("data/rrv_olfactometer_fife_data.xlsx", sheet = 2)
sht_3 <-
  read_xlsx("data/rrv_olfactometer_fife_data.xlsx", sheet = 3)
sht_4 <-
  read_xlsx("data/rrv_olfactometer_fife_data.xlsx", sheet = 4)
ben <-
  read_xlsx("data/rrv_olfactometer_ben_data.xlsx", sheet = 1)

#dropping unused columns)
sht_1 <- select(sht_1, -c(notes, time_mins))
sht_2 <- select(sht_2, -c(notes, time_mins))
sht_3 <- select(sht_3, -c(notes, time_mins))
sht_4 <- select(sht_4, -c(notes, time_mins))
ben <- select(ben, -c(notes, time_mins))

#fixing dates
sht_1$date <- lubridate::as_date(sht_1$date)
sht_2$date <- lubridate::as_date(sht_2$date)
sht_3$date <- lubridate::as_date(sht_3$date)
sht_4$date <- lubridate::as_date(sht_4$date)
ben$date <- lubridate::as_date(ben$date)


#combining datasheets
df <- full_join(prelim, sht_1)
df <- full_join(df, sht_2)
df <- full_join(df, sht_3)
df <- full_join(df, sht_4)
df <- full_join(df, ben)


#rearranging the datasheet columns
df <-
  select(
    df,
    'mite_no',
    'time_sec',
    'left',
    'right',
    'choice',
    'trial',
    'date',
  'experiment',
    'control',
    'no_response',
    'concentration',
    'rose_label',
    'RRV'
  )

#replacing the mite_no with its column number (no mite was tested more than once)
df$mite_no <- 1:nrow(df)

#making a new column to evaluate choices
df$chs <- df$choice
df$chs <- sub('experiment', '1', df$chs)
df$chs <- sub('control', '-1', df$chs)
df$chs <- sub('no choice', '0', df$chs)

# #filtering out 'no choice' results for analysis
# choices <- filter(df, df$choice != 'no choice')

#saving as a flat .csv file
write_csv(df, "data/rrv_all_olfactometer_flat.csv")

#cleanup
rm(list = ls(all.names = TRUE))