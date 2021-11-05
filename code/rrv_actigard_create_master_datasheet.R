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

#splitting block into different column
griff_2018$Block <- griff_2018$`Plant & Block`
griff_2018 <-
  separate(griff_2018, 'Block', c('Plant', 'Block'), sep = 1)
griff_2018 <- griff_2018 %>% dplyr::select(-Plant)

#adding a treatment column by duplicating the 'Plant' column
griff_2019$Treatment <- griff_2019$`Plant & Block`
griff_2019 <-
  separate(griff_2019, 'Treatment', c('Treatment', 'Block'), sep = 1)

#this works because each letter was assigned a specific treatment)
griff_2019$Treatment[griff_2019$Treatment == 'A'] <- "Water"
griff_2019$Treatment[griff_2019$Treatment == 'B'] <- "Actigard 100"
griff_2019$Treatment[griff_2019$Treatment == 'C'] <- "Actigard 50"
griff_2019$Treatment[griff_2019$Treatment == 'D'] <- "Kontos"

#combining the datasets from both years
griffin_trials <- bind_rows(griff_2018, griff_2019)

#we never ended up counting the number of flowers, dropping
griffin_trials <- griffin_trials %>% rename(Plant = `Plant & Block`)

griffin_trials <- griffin_trials %>% dplyr::select('Plant',
                                                   'Block',
                                                   'Treatment',
                                                   'Mite Count',
                                                   'Date')


griffin_trials <- rename(griffin_trials,
                         'P.fructiphilus' = 'Mite Count')

#adding columns
griffin_trials$Field <- 'Griffin'
griffin_trials$`H-B Score` <- 0
griffin_trials$RRD <- 'No'
griffin_trials$`Sample #` <- 1:length(griffin_trials[[1]])

#standardizing treatment labels
griffin_trials$Treatment <-
  sub('Actigard 100', 'High', griffin_trials$Treatment)
griffin_trials$Treatment <-
  sub('Actigard 50', 'Low', griffin_trials$Treatment)

#removing skipped and missing samples
griffin_trials <- griffin_trials %>% drop_na('P.fructiphilus')

griffin_trials$Treatment <-
  gsub("Kontos", "Spiro", griffin_trials$Treatment)

griffin_trials$Block <- as.numeric(griffin_trials$Block)

#rearranging columns
griffin_trials <-
  griffin_trials %>% dplyr::select(`Sample #`,
                                   Plant,
                                   Block,
                                   Treatment,
                                   P.fructiphilus,
                                   RRD,
                                   `H-B Score`,
                                   Date,
                                   Field)


####ATHENS DATA####
#getting Athens data from 2018 cleaned up
athns_2018 <-
  read_excel(
    "data/rrv_actigard_trial_2018_athens.xlsx",
    sheet = 1,
    col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess")
  )
athns_2018 <-
  separate(athns_2018, Grid, c('Plant', 'Grid'), sep = '-')
athns_2018$Block <- athns_2018$Grid

athns_2018 <- athns_2018 %>% unite("Plant", Plant:Grid, sep = "")

#fixing names
athns_2018 <- athns_2018 %>% dplyr::select(-Note) %>% rename(
  'RRD' = 'Symptom',
  'Treatment' = 'trt',
  'P.fructiphilus' = 'Target Mites',
  'Other Mites' = 'Non-Target Mites'
)

#adding in field column to Athens trials
athns_2018$Field <- 'Athens'

#making all variables same case
athns_2018$Treatment <- toTitleCase(athns_2018$Treatment)

#standardizing treatment labels
athns_2018$Treatment <-
  sub('Control', 'NoTrt', athns_2018$Treatment)

#removing incomplete records
athns_2018 <- athns_2018 %>% drop_na()

athns_2018$Block <- as.numeric(athns_2018$Block)

#reading in audpc data
athns_audpc <-
  read_excel("data/rrv_actigard_trial_2018_athens.xlsx",
             sheet = 2,
             skip = 1)

# # looking up the dates
# athns_dates <- read_excel(
#   "data/rrv_actigard_trial_2018_athens.xlsx",
#   sheet = 2,
#   range = cell_rows(1), col_names = FALSE
# )
# athns_dates <- athns_dates %>% dplyr::select(-1,-2)
# athns_dates <- athns_dates %>%  pivot_longer(everything())
# athns_dates <- athns_dates %>% dplyr::select(value)

athns_audpc <- athns_audpc %>% rename(
  '2018-08-31' = `Week 1`,
  '2018-09-07' = `Week 2`,
  '2018-09-14' = `Week 3`,
  '2018-09-21' = `Week 4`,
  '2018-09-28' = `Week 5`,
  '2018-10-05' = `Week 6`,
  '2018-10-12' = `Week 7`,
  '2018-10-19' = `Week 8`,
  '2018-10-26' = `Week 9`,
  '2018-11-02' = `Week 10`,
  '2018-12-19' = `Week 17`
)

#renaming appropriate columns for data merging
athns_audpc$`Sample #` <- 1:length(athns_audpc[[1]])
athns_audpc <- athns_audpc %>% rename('Block' = 'Rep')

#making sure the treatments are the same
athns_audpc$Treatment <-
  sub('Non-inoc', 'NoTrt', athns_audpc$Treatment)

athns_2018$RRD <- sub('-', 'A', athns_2018$RRD)

athns_2018 <- full_join(athns_audpc, athns_2018)

#saving the output
write_csv(athns_2018, 'data/rrv_athens_disease_progress_2018.csv')

athns_2018 <- athns_2018 %>% pivot_longer(
  names_to = "Date",
  values_to = "H-B Score",
  cols = c(
    '2018-08-31',
    '2018-09-07',
    '2018-09-14',
    '2018-09-21',
    '2018-09-28',
    '2018-10-05',
    '2018-10-12',
    '2018-10-19',
    '2018-10-26',
    '2018-11-02',
    '2018-12-19'
  )
)

#converting to proper format
athns_2018$Date <- lubridate::ymd(athns_2018$Date, tz = 'UTC')
athns_2018 <- athns_2018 %>% arrange(Date)

#many of the values became duplicated when pivoting longer, so resetting the
#total values to zero for all but the last observations
athns_2018[1:(length(athns_2018$`Sample #`) - 48), ]$AUDPC <-
  athns_2018[1:(length(athns_2018$`Sample #`) - 48), ]$`Final disease severity (%)` <-
  athns_2018[1:(length(athns_2018$`Sample #`) - 48), ]$P.fructiphilus <-
  athns_2018[1:(length(athns_2018$`Sample #`) - 48), ]$`Other Mites` <-
  0

####COMBINING DATASETS####
df <- bind_rows(griffin_trials, athns_2018)

#resetting the sample order for this datasheet
df <- df %>% arrange(Field, Date)
df$`Sample #` <- 1:length(df[[1]])

#rearranging columns
df <- df %>% rename(`Total P.fructiphilus` = P.fructiphilus)
df <- df %>% dplyr::select(
  `Sample #`,
  Treatment,
  `Total P.fructiphilus`,
  `Other Mites`,
  Plant,
  Block,
  Field,
  `H-B Score`,
  RRD,
  AUDPC,
  `Final disease severity (%)`,
  Date
)

#saving output
write_csv(df, 'data/rrv_actigard_master_datasheet.csv')

#cleanup
rm(list = ls())
