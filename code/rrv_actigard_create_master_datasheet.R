####PACKAGES####
pkgs <- c('tidyverse', 'readxl', 'tools')

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####GRIFFIN DATA####
#reading in file, data from Griffin 2018 actigard trial
griff_2018 <- read_excel(
  'data/rrv_actigard_trial_2018_griffin.xlsx',
  sheet = 1,
  col_types = c(
    'guess',
    'guess',
    'date',
    'guess',
    'guess',
    'guess',
    'guess',
    'guess',
    'guess'
  )
)

#reading in file, data from Griffin 2019 actigard trial
griff_2019 <- read_excel(
  'data/rrv_actigard_trial_2019_griffin.xlsx',
  sheet = 1,
  col_types = c('guess',
                'guess',
                'date',
                'guess')
)

#splitting plant and block into different columns
griff_2018 <-
  separate(griff_2018, `Plant & Block`, c('Plant', 'Block'), sep = 1)
griff_2019 <-
  separate(griff_2019, `Plant & Block`, c('Plant', 'Block'), sep = 1)

#adding a treatment column by duplicating the 'Plant' column
griff_2019$Treatment <- griff_2019$Plant

#this works because each letter was assigned a specific treatment)
griff_2019$Treatment[griff_2019$Treatment == 'A'] <- "Water"
griff_2019$Treatment[griff_2019$Treatment == 'B'] <- "Actigard 100"
griff_2019$Treatment[griff_2019$Treatment == 'C'] <- "Actigard 50"
griff_2019$Treatment[griff_2019$Treatment == 'D'] <- "Kontos"

#combining the datasets from both years
griffin_trials <- bind_rows(griff_2018, griff_2019)

#we never ended up counting the number of flowers, droppping
griffin_trials <-
  select(griffin_trials,
         'Plant',
         'Block',
         'Treatment',
         'Date',
         'Mite Count')

griffin_trials <- rename(griffin_trials,
                         'Mites' = 'Mite Count')

#adding column for field site
griffin_trials$Field <- 'Griffin'

#standardizing treatment labels
griffin_trials$Treatment <-
  sub('Actigard 100', 'High', griffin_trials$Treatment)
griffin_trials$Treatment <-
  sub('Actigard 50', 'Low', griffin_trials$Treatment)

#removing incomplete records
griffin_trials <- griffin_trials %>% drop_na

####ATHENS DATA####
#getting Athens data from 2018 cleaned up
athns_2018 <-
  read_excel(
    "data/rrv_actigard_trial_2018_athens.xlsx",
    sheet = 1,
    col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess")
  )
athns_2018 <-
  separate(athns_2018, Grid, c('Plant', 'Block'), sep = '-')
athns_2018 <-
  select(athns_2018, 'Plant', 'Block', 'trt', 'Target Mites')

#adding recording date
athns_2018$date <- lubridate::ymd("2018-12-21", tz = "UTC")

#fixing names
athns_2018 <- rename(
  athns_2018,
  'Treatment' = 'trt',
  'Date' = 'date',
  'Mites' = 'Target Mites'
)

#adding in field column to Athens trials
athns_2018$Field <- 'Athens'

#making all variables lowercase
athns_2018$Plant <- toTitleCase(athns_2018$Plant)
athns_2018$Block <- toTitleCase(athns_2018$Block)
athns_2018$Treatment <- toTitleCase(athns_2018$Treatment)

#standardizing treatment labels
athns_2018$Treatment <-
  sub('Control', 'Untreated', athns_2018$Treatment)

#removing incomplete records
athns_2018 <- athns_2018 %>% drop_na()

####COMBINING DATASETS####
#uncomment the following line this if you want to include the Athens 2018 trial
df <- bind_rows(griffin_trials, athns_2018)

#adding combined id for plants
df$ID <- paste(df$Plant, df$Block, sep = '')

#rearranging columns
df <-
  select(df,
         'ID',
         'Mites',
         'Treatment',
         'Plant',
         'Block',
         'Field',
         'Date')

#saving output
write_csv(df, 'data/rrv_actigard_master_datasheet.csv')

#cleanup
rm(list = ls())
