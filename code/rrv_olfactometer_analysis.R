#loading packages
pkgs <- c('tidyverse')

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in olfactometer data, making sure R knows df are factors
df <-
  read_csv('data/rrv_all_olfactometer_flat.csv',
           col_types = cols(choice = col_factor(c(
             'no choice', 'control', 'experiment'
           ))))

tbl_colnames <-
  c(
    'trial',
    'statistic.X-squared',
    'parameter.df',
    'p.value',
    'method',
    'data.name',
    'observed.no choice',
    'observed.control',
    'observed.experiment',
    'expected.no choice',
    'expected.control',
    'expected.experiment',
    'residuals.no choice',
    'residuals.control',
    'residuals.experiment',
    'stdres.no choice',
    'stdres.control',
    'stdres.experiment'
  )
rrv_olfact_chisq_tests <- read_csv("\n", col_names = tbl_colnames)
trial_list <- list('air', 'rrv', 'rose', 'MeSA', 'limonene')

for (i in seq_along(trial_list)) {
  one_trial <- df %>%  filter(trial == unlist(trial_list[i]))
  congnt_table <- table(one_trial$choice)
  chisq_test <- as_tibble(t(unlist(chisq.test(congnt_table))))
  trial_name <- trial_list[i]
  rrv_olfact <- bind_cols(trial_name, chisq_test)
  rrv_olfact <- rrv_olfact %>% rename(trial = 1)
  rrv_olfact_chisq_tests <-
    bind_rows(rrv_olfact_chisq_tests, rrv_olfact)
}

rrv_olfact_chisq_tests <-
  rrv_olfact_chisq_tests %>%  select(-data.name)

write_csv(rrv_olfact_chisq_tests, "data/rrv_olfact_chisq_tests.csv")

#cleaning up
rm(list = ls())