#loading packages
load_pkgs <- c('tidyverse')
lapply(load_pkgs, library, character.only = T)

#reading in olfactometer data, making sure R knows df are factors
df <-
  read_csv('data/rrv_all_olfactometer_flat.csv', col_types = cols(choice = col_factor(c(
    'no choice', 'control', 'experiment'
  ))))

#filtering trials into thier respective groups
air_mites <- filter(df, df$trial == 'air')
rrv_mites <- filter(df, df$trial == 'rrv')
rose_mites <- filter(df, df$trial == 'rose')
mesa_mites <- filter(df, df$trial == 'MeSA')
lim_mites <- filter(df, df$trial == 'limonene')

#creating contingency tables
air_table <- table(air_mites$choice)
rrv_table <- table(rrv_mites$choice)
rose_table <- table(rose_mites$choice)
mesa_table <- table(mesa_mites$choice)
lim_table <- table(lim_mites$choice)

#chi-squared tests of data
rrv_olfact_chisq_tests <- bind_rows(as_tibble(t(unlist(
  chisq.test(air_table)
))),
as_tibble(t(unlist(
  chisq.test(rrv_table)
))),
as_tibble(t(unlist(
  chisq.test(rose_table)
))),
as_tibble(t(unlist(
  chisq.test(mesa_table)
))),
as_tibble(t(unlist(
  chisq.test(lim_table)
))))

write_csv(rrv_olfact_chisq_tests, "data/rrv_olfact_chisq_tests.csv")

#cleaning up
rm(list = ls())