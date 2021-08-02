####PACKAGES####
pkgs <- c('tidyverse', 'lme4', 'car', 'multcomp')
lapply(pkgs, library, character.only = T)
rm(pkgs)

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

####ACTIGARD DATA ANALYSIS####
#reading in the data
df <- read_csv('actigrd.csv')

#assigning columns as factors
df$treat <- as_factor(df$treat)
df$id <- as_factor(df$id)
df$plant <- as_factor(df$plant)
df$block <- as_factor(df$block)
df$field <- as_factor(df$field)

#data is now ready to be analyzed
#glmer model
glm_1 <-
  glmer(mites ~ treat + (1 | block), family = 'poisson', data = df)

summary(glm_1)

#ANOVA
Anova(glm_1, type = c("III")) #treatment is significant

#multiple comparisions
#suggests that both the low Actigard rate and Kontos treatments are significantly different than the water treatment
summary(glht(glm_1, linfct = mcp(treat = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- glht(glm_1, linfct = mcp(treat = "Tukey"))

sink(file = 'actigard_cld_letters.txt')
cld(q)
sink()

#cleanup
rm(list = ls())