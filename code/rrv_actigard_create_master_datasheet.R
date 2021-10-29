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
griff_2018 <- griff_2018 %>% select(-Plant)

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

griffin_trials$`% Disease` <-
  replace_na(griffin_trials$`% Disease`, 0)

#we never ended up counting the number of flowers, dropping
griffin_trials <- griffin_trials %>% rename(Plant = `Plant & Block`)

griffin_trials <- griffin_trials %>% dplyr::select('Plant',
                                                   'Block',
                                                   'Treatment',
                                                   'Mite Count',
                                                   'Date',
                                                   '% Disease')


griffin_trials <- rename(griffin_trials,
                         'P.fructiphilus' = 'Mite Count',
                         'Disease Severity (%)' = '% Disease')

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

#rearranging columns
griffin_trials <-
  griffin_trials %>% select(
    `Sample #`,
    Plant,
    Block,
    Treatment,
    P.fructiphilus,
    RRD,
    `H-B Score`,
    `Disease Severity (%)`,
    Date,
    Field
  )


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
athns_2018 <- athns_2018 %>% select(-Note) %>% rename(
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

#renaming appropriate columns for data merging
athns_audpc <- athns_audpc %>% rename('Block' = 'Rep')

#creating matching columns
athns_audpc$`Sample #` <- 1:length(athns_audpc[[1]])

#making sure the treatments are the same
athns_audpc$Treatment <-
  sub('Non-inoc', 'Untreated', athns_audpc$Treatment)

#now only contains the summary statistics
athns_summary <-
  left_join(athns_2018, athns_audpc) %>% select(-`Sample #`, -Field, -Block)

athns_summary$RRD <- sub('-', NA, athns_summary$RRD)

#saving the output
write_csv(athns_summary, 'data/rrv_athens_disease_progress_2018.csv')

#TODO: Use or lose Athens data here:
View(
  athns_audpc %>%  select(-AUDPC,-`Final disease severity (%)`) %>% pivot_longer(
    cols = c(
      `Week 1`,
      `Week 2`,
      `Week 3`,
      `Week 4`,
      `Week 5`,
      `Week 6`,
      `Week 7`,
      `Week 8`,
      `Week 9`,
      `Week 10`,
      `Week 17`
    )
  )
)







# getting the dates
athns_dates <- read_excel(
  "data/rrv_actigard_trial_2018_athens.xlsx",
  sheet = 2,
  range = cell_rows(1), col_names = FALSE
)
athns_dates <- athns_dates %>% select(-1,-2)


# #adding recording date
# athns_2018$date <- lubridate::ymd("2018-12-21", tz = "UTC")



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
