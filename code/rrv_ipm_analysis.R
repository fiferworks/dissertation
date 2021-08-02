####PACKAGES####
pkgs <- c('tidyverse', 'lme4', 'car', 'multcomp')
lapply(pkgs, library, character.only = T)
rm(pkgs)

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

###########################################################################################
#IMPORTANT NOTE! WE DID NOT RECOVER ERIOPHYOIDS FROM THESE SAMPLES! THESE ARE OTHER MITES!#
###########################################################################################

####IPM DATA ANALYSIS####
#reading in the data
df <- read_csv('ipm.csv')

#assigning columns as factors
df$treat <- as_factor(df$treat)
df$id <- as_factor(df$id)
df$plant <- as_factor(df$plant)
df$block <- as_factor(df$block)
df$field <- as_factor(df$field)

#Athens only
athns <- filter(df, field == 'Athens')

#Griffin only
grifn <- filter(df, field == 'Griffin')

#data are now ready to be analyzed

#glmer model of other mites for all sites
glm_1 <-
  glmer(other ~ treat + field + (1 |
                                   block),
        family = 'poisson',
        data = df)

summary(glm_1)

#ANOVA
Anova(glm_1, type = c("III")) #treatment is significant

#multiple comparisions
summary(glht(glm_1, linfct = mcp(treat = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- glht(glm_1, linfct = mcp(treat = "Tukey"))

sink(file = 'ipm_cld_letters.txt')
cld(q)
sink()

####ATHENS SITES####
#glmer model of other mites for all sites
glm_2 <-
  glmer(other ~ treat + (1 | block),
        family = 'poisson',
        data = athns)

summary(glm_2)

#ANOVA
Anova(glm_2, type = c("III")) #treatment is significant

#multiple comparisions
summary(glht(glm_2, linfct = mcp(treat = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
w <- glht(glm_2, linfct = mcp(treat = "Tukey"))

sink(file = 'ipm_cld_letters_athns.txt')
cld(w)
sink()

####GRIFFIN SITES####
#glmer model of other mites for all sites
glm_3 <-
  glmer(other ~ treat + (1 | block),
        family = 'poisson',
        data = grifn)

summary(glm_3)

#ANOVA
Anova(glm_3, type = c("III")) #treatment is significant

#multiple comparisions
summary(glht(glm_3, linfct = mcp(treat = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
e <- glht(glm_3, linfct = mcp(treat = "Tukey"))

sink(file = 'ipm_cld_letters_grifn.txt')
cld(e)
sink()

#cleanup
rm(list = ls())