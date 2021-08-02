####PACKAGES####
pkgs <- c('tidyverse', 'lme4', 'car', 'multcomp', 'readxl')

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = T)

lapply(pkgs, library, character.only = T)

####MITE SURVEY ANALYSIS####

####READING IN THE REQUIRED FILES####
df <- read_xlsx('all_mites_survey.xlsx')

#making sure certain columns are considered as factors
df$p_fructiphilus <- as.factor(df$p_fructiphilus)
df$month <- as.factor(df$month)
df$collector <- as.factor(df$collector)

#focusing on mites in Florida
#filters out 'NAs'
df_fl <- filter(df, df$eriophyoids >= '0')

#filters out eriophyoids which aren't P. fructiphilus
df_fl <- filter(df_fl, df_fl$p_fructiphilus == 'Yes')

#filters data to only show Florida sites
df_fl <- filter(df_fl, df_fl$state == 'FL')

####DROPS A SPECIFIC SITE WHICH MIGHT BE AN OUTLIER####
####DOUBLE CHECK SITE BEFORE USING FOR ANALYSIS####
df_fl <- filter(df_fl, df_fl$id != 'James 114')

#the following tests are just a lark, so future editing will be easier
#anova
glm_1 <-
  glmer(eriophyoids ~ month + (1 |
                                 id), family = 'poisson', data = df_fl)

#ANOVA
Anova(glm_1)

#multiple comparisions
summary(glht(glm_1, linfct = mcp(month = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- glht(glm_1, linfct = mcp(month = "Tukey"))

#cleanup
rm(list = ls())