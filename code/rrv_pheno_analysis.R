#####LOADING PACKAGES####
pkgs <-  c('tidyverse', 'lme4', 'nlme', 'car', 'emmeans', 'multcompView')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####READING IN THE REQUIRED FILES####
df <- read_csv('data/rrv_pheno_clean_datasheet.csv')

#filter by sites which were pruned
df <-
  df %>% filter(id == 'Pheno 11' |
                  id == 'Pheno 12' |
                  id == 'Pheno 13' | id == 'Pheno 14')

df <- df %>%
  dplyr::select(sample_no, id, month, other_mites, eriophyoids)

df$id <- as_factor(df$id)
df$month <- as_factor(df$month)



sink('data/rrv_pheno_analysis_results.txt')
q <- glmer(eriophyoids ~ month*other_mites + (1|id), data = df, family = 'poisson')
summary(q)
plot(q)
Anova(q)

main_effects <-
  emmeans(q, list(pairwise ~ month), adjust = "tukey", details = "true")

#gives a compact letter display of estimated marginal means (lsmeans)
# CLD(q, Letters = letters, adjust = "tukey", details = "true")
# CLD(q, Letters = letters, by = "period", adjust = "tukey", details = "true")
# CLD(q, Letters = letters, by = "germ", adjust = "tukey", details = "true")
sink()