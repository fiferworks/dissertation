####PACKAGES####
pkgs <-
  c('tidyverse', 'lme4', 'car', 'multcomp', 'emmeans')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####IPM DATA ANALYSIS####
#reading in the data
df <- read_csv('data/ipm.csv')

#assigning columns as factors
df$Treatment <- as_factor(df$Treatment)
df$ID <- as_factor(df$ID)
df$Block <- as_factor(df$Block)
df$Field <- as_factor(df$Field)

#Athens only
athns <- filter(df, Field == 'Athens')

#Griffin only
grifn <- filter(df, Field == 'Griffin')

#Tallahassee only
talla <- filter(df, Field == 'Tallahassee')

##############
#ERIOPHYOIDS!#
##############
glm_erios <-
  glmer(Eriophyoids ~ Treatment + (1 | Field),
        family = 'poisson',
        data = df)

summary(glm_erios)

#ANOVA
Anova(glm_erios, type = c("III")) #treatment is significant

#making a compact letter display for each treatment
er <- emmeans(glm_erios, "Treatment")

sink(file = 'data/rrv_ipm_cld_all_erios.txt')
cld(er)
sink()

#########################################################
#IMPORTANT NOTE! THESE ARE OTHER MITES, NOT ERIOPHYOIDS!#
#########################################################

glm_other <-
  glmer(`Other Mites` ~ Treatment + (1 | Field),
        family = 'poisson',
        data = df)

summary(glm_other)

#ANOVA
Anova(glm_other, type = c("III")) #treatment is significant

#making a compact letter display for each treatment
q <- emmeans(glm_other, "Treatment")

sink(file = 'data/rrv_ipm_cld_other_mites.txt')
cld(q)
sink()

####ATHENS SITES####
###########################################################################################
#IMPORTANT NOTE! WE DID NOT RECOVER ERIOPHYOIDS FROM THESE SAMPLES! THESE ARE OTHER MITES!#
###########################################################################################
#glmer model of other mites for all sites
glm_athens <-
  glmer(`Other Mites` ~ Treatment + (1 | Block),
        family = 'poisson',
        data = athns)

summary(glm_athens)

#ANOVA
Anova(glm_athens, type = c("III")) #treatment is significant

#multiple comparisions
summary(glht(glm_athens, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
w <- emmeans(glm_athens, "Treatment")

sink(file = 'data/rrv_ipm_cld_athns.txt')
cld(w)
sink()

#####GRIFFIN SITES####
####################################################################
#NONE OF THE CODE CAN WORK, THERE WAS HARDLY ANY RECOVERY OF MITES!#
####################################################################
# #glmer model of other mites for all sites
# glm_griffin <-
#   glmer(`Other Mites` ~ Treatment + (1 | Block),
#         family = 'poisson',
#         data = grifn)
#
# summary(glm_griffin)
#
# #ANOVA
# Anova(glm_griffin, type = c("III")) #treatment is significant
#
# #multiple comparisions
# summary(glht(glm_griffin, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))
#
# #making a compact letter display for each treatment
# e <- emmeans(glm_griffin, "Treatment")
#
# sink(file = 'data/rrv_ipm_cld_grifn.txt')
# cld(e)
# sink()

####TALLAHASSEE SITES####
##############
#ERIOPHYOIDS!#
##############
glm_talla_erios <-
  glmer(Eriophyoids ~ Treatment + (1 | Block),
        family = 'poisson',
        data = talla)

summary(glm_talla_erios)

#ANOVA
Anova(glm_talla_erios, type = c("III")) #treatment is significant

#making a compact letter display for each treatment
erio <- emmeans(glm_talla_erios, "Treatment")

sink(file = 'data/rrv_ipm_cld_erios_talla.txt')
cld(erio)
sink()

#glmer model of other mites for all sites
glm_talla <-
  glmer(`Other Mites` ~ Treatment + (1 | Block),
        family = 'poisson',
        data = talla)

summary(glm_talla)

#ANOVA
Anova(glm_talla, type = c("III")) #treatment is significant

#multiple comparisions
summary(glht(glm_talla, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
f <- emmeans(glm_talla, "Treatment")

sink(file = 'data/rrv_ipm_cld_other_talla.txt')
cld(f)
sink()

#cleanup
rm(list = ls())