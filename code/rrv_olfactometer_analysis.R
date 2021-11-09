#loading packages
pkgs <- c('tidyverse', 'lme4', 'car', 'multcomp', 'emmeans')

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in olfactometer data, making sure R knows df are factors
df <-
  read_csv('data/rrv_all_olfactometer_flat.csv', )

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
  chisq_test <- as_tibble(t(unlist(chisq.test(congnt_table[1:2]))))
  nchs <- as_tibble_row(congnt_table[3])
  nchs <- nchs %>% rename('observed.no choice' = 'no choice')
  nchs$`observed.no choice` <-
    as.character(nchs$`observed.no choice`)
  trial_name <- trial_list[i]
  rrv_olfact <-
    bind_cols(trial_name, nchs, chisq_test)
  rrv_olfact <- rrv_olfact %>% rename(trial = 1)
  rrv_olfact_chisq_tests <-
    bind_rows(rrv_olfact_chisq_tests, rrv_olfact)
}

rrv_olfact_chisq_tests <-
  rrv_olfact_chisq_tests %>%  dplyr::select(-data.name)

write_csv(rrv_olfact_chisq_tests, "data/rrv_olfact_chisq_tests.csv")

# preparing data for glmer model
df$outcome <- if_else(df$choice == 'experiment', 1, 0)

#generalized linear mixed models with individual mites as the random factor
choice_model <- glmer(outcome ~ trial + (1 | mite_no),
                      family = "binomial",
                      data = df)

time_2_choice_model <- glmer(time_sec ~ trial + (1 | mite_no),
                             family = "poisson",
                             data = df)

#making a compact letter display for each treatment
sink(file = 'data/rrv_olfact_cld_choice_model.txt')
#compact letter display
cld(emmeans(choice_model, "trial"))
#Simultaneous Tests for General Linear Hypotheses Multiple Comparisons of Means:
#Tukey Contrasts
summary(glht(choice_model, linfct = mcp(trial = 'Tukey')), test = adjusted("holm"))
#ANOVA
Anova(choice_model)
#model summary
summary(choice_model)
sink()

sink(file = 'data/rrv_olfact_cld_time_2_choice_model.txt')
#compact letter display
cld(emmeans(time_2_choice_model, "trial"))
#Simultaneous Tests for General Linear Hypotheses Multiple Comparisons of Means:
#Tukey Contrasts
summary(glht(time_2_choice_model, linfct = mcp(trial = 'Tukey')), test = adjusted("holm"))
#ANOVA
Anova(time_2_choice_model)
#model summary
summary(time_2_choice_model)
sink()

#cleaning up
rm(list = ls())
