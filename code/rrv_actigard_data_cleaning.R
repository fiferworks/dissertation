####PACKAGES####
pkgs <- c('tidyverse', 'readxl')
lapply(pkgs, library, character.only = T)
rm(list = ls())

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

####GRIFFIN DATA####
#reading in file, data from Griffin 2018 actigard trial
q1 <- read_excel(
  'master_actigard_trial_data.xlsx',
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
q2 <- read_excel(
  'master_actigard_trial_data.xlsx',
  sheet = 4,
  col_types = c('guess',
                'guess',
                'date',
                'guess')
)


#splitting plant and block into different columns
q1 <- separate(q1, `Plant & Block`, c('Plant', 'Block'), sep = 1)
q2 <- separate(q2, `Plant & Block`, c('Plant', 'Block'), sep = 1)

#adding a treatment column by duplicating the 'Plant' column
q2$Treatment <- q2$Plant

#this works because each letter was assigned a specific treatment)
q2$Treatment[q2$Treatment == 'A'] <- "Water"
q2$Treatment[q2$Treatment == 'B'] <- "Actigard 100"
q2$Treatment[q2$Treatment == 'C'] <- "Actigard 50"
q2$Treatment[q2$Treatment == 'D'] <- "Kontos"

#combining the datasets from both years
q1 <- bind_rows(q1, q2)

#dropping unused columns
q1 <-
  select(q1, 'Plant', 'Block', 'Treatment', 'Date', 'Mite Count')

#making things lowercase for ease of use when coding
q1 <- rename(
  q1,
  'plant' = 'Plant',
  'block' = 'Block',
  'treat' = 'Treatment',
  'date' = 'Date',
  'mites' = 'Mite Count'
)

#adding column for field site
q1$field <- 'griffin'

#forcing date to be read as a character to avoid errors
q1$date <- as.character(q1$date)

#making sure all variables are lowercase
q1$plant <- tolower(q1$plant)
q1$block <- tolower(q1$block)
q1$treat <- tolower(q1$treat)

#standardizing treatment labels
q1$treat <- sub('actigard 100', 'high', q1$treat)
q1$treat <- sub('actigard 50', 'low', q1$treat)

#removing incomplete records
q1 <- q1 %>% drop_na

df <- q1

# ####ATHENS DATA####
# #getting Athens data from 2018 cleaned up
# q3 <-
#   read_excel(
#     'RRV Jean 12.21.18.xlsx',
#     sheet = 1,
#     col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess")
#   )
# q3 <- separate(q3, Grid, c('Plant', 'Block'), sep = '-')
# q3 <- select(q3, 'Plant', 'Block', 'trt', 'Target Mites')
#
# #adding recording date
# q3$date <- c("2018-12-21")
#
# #fixing names
# q3 <- rename(
#   q3,
#   'plant' = 'Plant',
#   'block' = 'Block',
#   'treat' = 'trt',
#   'date' = 'date',
#   'mites' = 'Target Mites'
# )
#
# #adding in field column to Athens trials
# q3$field <- 'athens'
#
# #making all variables lowercase
# q3$plant <- tolower(q3$plant)
# q3$block <- tolower(q3$block)
# q3$treat <- tolower(q3$treat)
#
# #standardizing treatment labels
# q3$treat <- sub('control', 'untreated', q3$treat)
#
# #removing incomplete records
# q3 <- q3 %>% drop_na()

####COMBINING DATASETS####
#uncomment the following line this if you want to include the Athens 2018 trial
# df <- bind_rows(q1, q3)

#adding combined id for plants
df$id <- paste(df$plant, df$block, sep = '')

#getting summary stats for each treatment group
df <-
  df %>% group_by(treat) %>% mutate(
    per_plant = mean(mites),
    totals = sum(mites),
    sd = sd(mites),
    se = sd(mites) / sqrt(n()),
    log_xformed = log(mean(mites)),
    se_xformed = log(sd(mites) / sqrt(n())),
    n_samples = n()
  ) %>%
  ungroup()

#rearranging columns
df <-
  select(
    df,
    'mites',
    'totals',
    'per_plant',
    'sd',
    'se',
    'log_xformed',
    'se_xformed',
    'treat',
    'id',
    'plant',
    'block',
    'field',
    'date',
    'n_samples'
  )

#saving output
write_csv(df, 'actigrd.csv')

#cleanup
rm(list = ls())