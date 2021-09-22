#a list of packages used for this script
pkgs <-  c("tidyverse", "readxl")

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in each sheet into a dataframe
prelim <-
  read_xlsx("data/rrv_olfactometer_martini_data.xlsx", sheet = 1) %>% add_column(recorder = 'Xavier Martini')
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
sht_1 <-
  select(sht_1,-c(notes, time_mins)) %>% add_column(recorder = 'Austin Fife')
sht_2 <-
  select(sht_2,-c(notes, time_mins)) %>% add_column(recorder = 'Austin Fife')
sht_3 <-
  select(sht_3,-c(notes, time_mins)) %>% add_column(recorder = 'Austin Fife')
sht_4 <-
  select(sht_4,-c(notes, time_mins)) %>% add_column(recorder = 'Austin Fife')
ben <-
  select(ben,-c(notes, time_mins)) %>% add_column(recorder = 'Ben Reimer')

#combining datasheets without dropping columns that some datasheets don't have
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
    'RRV',
    'recorder'
  ) %>%
  rename(rrv = RRV)

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