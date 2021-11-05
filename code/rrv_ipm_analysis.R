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
df <- read_csv('data/rrv_ipm_master_datasheet.csv')

#removing untreated plots
df <- df %>% filter(Treatment != 'NoTrt')

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
  glmer(
    Eriophyoids ~ Treatment + Tetranychoids + Phytoseiids + (1 | Field),
    family = 'poisson',
    data = df
  )

summary(glm_erios)

#ANOVA
Anova(glm_erios, type = c("III")) #treatment, tetranychoids and phytoseiids are significant

#making a compact letter display for each treatment
q <- emmeans(glm_erios, "Treatment")

sink(file = 'data/rrv_ipm_cld_all_erios.txt')
cld(q) #compact letter display
summary(glht(glm_erios, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
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
cld(q) #compact letter display
summary(glht(glm_other, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
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

#multiple comparisons
summary(glht(glm_athens, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- emmeans(glm_athens, "Treatment")

sink(file = 'data/rrv_ipm_cld_athns.txt')
cld(q) #compact letter display
summary(glht(glm_athens, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
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
# #multiple comparisons
# summary(glht(glm_griffin, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))
#
# #making a compact letter display for each treatment
# e <- emmeans(glm_griffin, "Treatment")
#
# sink(file = 'data/rrv_ipm_cld_grifn.txt')
# cld(q) #compact letter display
# summary(glht(glm_griffin, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
# sink()

####TALLAHASSEE SITES####
##############
#ERIOPHYOIDS!#
##############
glm_talla_erios <-
  glmer(
    Eriophyoids ~ Treatment + Tetranychoids + Phytoseiids + (1 | Block),
    family = 'poisson',
    data = talla
  )

summary(glm_talla_erios)

#ANOVA
Anova(glm_talla_erios, type = c("III")) #treatment is significant

#making a compact letter display for each treatment
q <- emmeans(glm_talla_erios, "Treatment")

sink(file = 'data/rrv_ipm_cld_erios_talla.txt')
cld(q) #compact letter display
summary(glht(glm_talla_erios, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
sink()

###############
#TETRANYCHOIDS#
###############
glm_tet <-
  glmer(
    Tetranychoids ~ Treatment + Eriophyoids + Phytoseiids + (1 | Block),
    family = 'poisson',
    data = talla
  )

summary(glm_tet)

#ANOVA
Anova(glm_tet, type = c("III")) #treatment, eriophyoids and phytoseiids are significant

#making a compact letter display for each treatment
q <- emmeans(glm_tet, "Treatment")

sink(file = 'data/rrv_ipm_cld_tet_talla.txt')
cld(q) #compact letter display
summary(glht(glm_tet, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
sink()

#############
#PHYTOSEIIDS#
#############
glm_pred <-
  glmer(
    Phytoseiids ~ Treatment + Tetranychoids + Eriophyoids + (1 | ID),
    family = 'poisson',
    data = talla
  )

summary(glm_pred)

#ANOVA
Anova(glm_pred, type = c("III")) #treatment and tetranychoids are significant

#making a compact letter display for each treatment
q <- emmeans(glm_pred, "Treatment")

sink(file = 'data/rrv_ipm_cld_pred_talla.txt')
cld(q) #compact letter display
summary(glht(glm_pred, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
sink()

#############
#OTHER MITES#
#############
#glmer model of other mites for all sites
glm_talla <-
  glmer(
    `Other Mites` ~ Treatment + Tetranychoids + Eriophyoids + (1 |
                                                                 Block),
    family = 'poisson',
    data = talla
  )

summary(glm_talla)

#ANOVA
Anova(glm_talla, type = c("III")) #treatment and tetranychoids are significant

#making a compact letter display for each treatment
q <- emmeans(glm_talla, "Treatment")

sink(file = 'data/rrv_ipm_cld_other_talla.txt')
cld(q) #compact letter display)
summary(glht(glm_talla, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) #multiple comparisons
sink()

#cleanup
rm(list = ls())
