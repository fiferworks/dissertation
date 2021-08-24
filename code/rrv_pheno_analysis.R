#####LOADING PACKAGES####
pkgs <-
  c('tidyverse',
    'lme4',
    'nlme',
    'car',
    'emmeans',
    'multcomp',
    'multcompView')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####READING IN THE REQUIRED FILES####
df <- read_csv('data/rrv_pheno_clean_datasheet.csv')
df$id <- as_factor(df$id)

#making sure months are interpreted correctly
df$month <- month(df$date, label = T, abbr = FALSE)

#adding year column
df$year <- year(df$date)


#filter by sites which were untreated for the
df <-
  df %>% filter(id == 'Pheno 11' |
                  id == 'Pheno 12' |
                  id == 'Pheno 13' | id == 'Pheno 14')

# df <- df %>%
#   dplyr::select(sample_no, id, month, year, other_mites, eriophyoids)



#### Stuck here####
# sink('data/rrv_pheno_analysis_results.txt')
glmer(eriophyoids ~ month + (1 | id),
      data = df,
      family = 'poisson')
q
summary(q)
plot(q)
Anova(q)

#multiple comparisons
summary(glht(q, linfct = mcp(treat = "Tukey")), test = adjusted("holm"))

# sink()