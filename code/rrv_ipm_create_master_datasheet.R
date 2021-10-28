####SETUP####
pkgs <- c('tidyverse', 'readxl', 'writexl', 'lubridate')

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####GRIFFIN DATA####
#reading in file, data from IPM 2019 Georgia Trials
ga_trials <-
  read_excel(
    'data/rrv_ipm_trial_2019.xlsx',
    sheet = 1,
    col_types = c('guess',
                  'guess',
                  'guess',
                  'date',
                  'guess',
                  'guess')
  )


#splitting plant and block into different columns
ga_trials <-
  separate(ga_trials, `Block, Plant, Treat`, c('Block', 'Plant'), sep = 1)

#splitting out treatments as well
ga_trials <-
  separate(ga_trials, Plant, c('Plant', 'Treatment'), sep = -1)

#renames treatments to more readable form
ga_trials$Treatment[ga_trials$Treatment == 'M'] <- "Mites"
ga_trials$Treatment[ga_trials$Treatment == 'W'] <- "Water"
ga_trials$Treatment[ga_trials$Treatment == 'N'] <- "Ninja"
ga_trials$Treatment[ga_trials$Treatment == 'K'] <- "Kontos"
ga_trials$Treatment[ga_trials$Treatment == 'A'] <- "Actigard"
ga_trials$Treatment[ga_trials$Treatment == '+'] <- "MN"

#renaming a column to 'Field'
ga_trials <- ga_trials %>% rename(Field = `Athens/Griffin`)

#adding combined id for plants
ga_trials$ID <- paste(ga_trials$Plant, ga_trials$Block, sep = '')

#rearranging columns
ga_trials <-
  dplyr::select(
    ga_trials,
    'ID',
    'Plant',
    'Block',
    'Treatment',
    'Eriophyoids',
    `Other Mites`,
    'Date',
    'Field'
  ) %>% arrange(Date)

#### TALLAHASSEE DATASHEET ####
ta_trials <- read_excel('data/rrv_ipm_trial_2020-2021.xlsx')

ta_trials <- ta_trials %>% add_column(Field = 'Tallahassee')

# adding a column for mites/gram
ta_trials <-
  ta_trials %>% mutate(
    'mites/g' = (other_mites) / grams_dry_weight,
    'erios/g' = eriophyoids / grams_dry_weight,
    .before = grams_dry_weight
  )


ta_trials <- ta_trials %>% rename(
  'ID' = id,
  'Eriophyoids' = eriophyoids,
  `Other Mites` = other_mites,
  'Date' = date
)


#getting summary stats for each treatment group
ga_trials <-
  ga_trials %>% group_by(Treatment) %>% mutate(N = n()) %>% ungroup()

ga_trials <- ga_trials %>% add_column(
  'mites/g' = NA,
  'erios/g' = NA,
  'grams_dry_weight' = NA
)

ta_trials <-
  ta_trials %>% group_by(Treatment) %>% mutate(N = n()) %>% ungroup()

ta_trials <-
  ta_trials %>%  dplyr::select(-sample_no) %>% arrange(Date)

# reading in the predatory mite data
ta_pmites <-
  read_excel(path = 'data/rrv_ipm_pred_mites_jessie.xlsx')

ta_pmites <-
  ta_pmites %>% rename(ID = Number,
                       Tetranychoids = Herb,
                       Phytoseiids = Pred)

ta_pmites$ID <- paste("Pheno", ta_pmites$ID)

ta_trials <- ta_trials %>% left_join(ta_pmites)

df <- bind_rows(ta_trials, ga_trials)

df <-
  df %>% add_column(Month = month(df$Date, label = TRUE, abbr = FALSE))

df <- df %>%  dplyr::select(-notes,-Plant)

df <-
  df %>% dplyr::select(
    'ID',
    'Date',
    'Month',
    `mites/g`,
    `erios/g`,
    'grams_dry_weight',
    'Eriophyoids',
    'Tetranychoids',
    'Phytoseiids',
    `Other Mites`,
    'Block',
    'Treatment',
    'Field',
    'N'
  )

#getting summary stats for each treatment group for other mites
df <- df %>% group_by(Treatment) %>% mutate(
  'tetranychoids/plant' = mean(Tetranychoids, na.rm = TRUE),
  tet_totals = sum(Tetranychoids, na.rm = TRUE),
  tet_sd = sd(Tetranychoids, na.rm = TRUE),
  tet_se = sd(Tetranychoids, na.rm = TRUE) / sqrt(n()),
  'phytoseiids/plant' = mean(Phytoseiids, na.rm = TRUE),
  pred_totals = sum(Phytoseiids, na.rm = TRUE),
  tet_sd = sd(Phytoseiids, na.rm = TRUE),
  tet_se = sd(Phytoseiids, na.rm = TRUE) / sqrt(n()),
  'erios/plant' = mean(Eriophyoids, na.rm = TRUE),
  'erios/plant/g' = mean(Eriophyoids / grams_dry_weight),
  erio_totals = sum(Eriophyoids, na.rm = TRUE),
  erio_sd = sd(`erios/g`, na.rm = TRUE),
  erio_sd = sd(`erios/g`, na.rm = TRUE) / sqrt(n()),
  n_samples = n()
) %>%
  ungroup()

#rounding for graph
df$'erios/plant/g' <-
  round(df$'erios/plant/g', digits = 2)

df$Eriophyoids <- df$Eriophyoids %>% replace_na(0)
df$Tetranychoids <- df$Tetranychoids %>% replace_na(0)
df$Phytoseiids <- df$Phytoseiids %>% replace_na(0)

#saving the master file
write_csv(df, 'data/rrv_ipm_master_datasheet.csv')

#cleanup
rm(list = ls())
