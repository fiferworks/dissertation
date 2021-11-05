####SETUP####
pkgs <-
  c('lme4',
    'pscl',
    'car',
    'boot',
    'multcomp',
    'lubridate',
    'tidyverse')

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####ACTIGARD DATA ANALYSIS####
#reading in the data
df <-
  read_csv('data/rrv_actigard_master_datasheet.csv', col_types = "fcddfffdfdd?")

#only Griffin site had good data
grifn <- df %>% filter(Field == 'Griffin')
grifn$Treatment <- as_factor(grifn$Treatment)

#data is now ready to be analyzed
####ZIP MODEL OF GRIFFIN DATA####
zip_model <-
  zeroinfl(
    `Total P.fructiphilus` ~ Treatment,
    data = grifn,
    dist = "poisson",
    link = 'logit'
  )

summary(zip_model)
Anova(zip_model, type = c("III")) #treatment is significant

#glmer model,  Model is nearly unidentifiable: large eigenvalue ratio
glm_1 <-
  glmer(`Total P.fructiphilus` ~ Treatment + (1 |
                                                Block),
        family = 'poisson',
        data = grifn)

summary(glm_1)

#ANOVA
Anova(glm_1, type = c("III")) #treatment is significant

#multiple comparisons
#glmer model suggests that the low Actigard rate and Kontos treatments are
#significantly different than the water treatment
summary(glht(glm_1, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))


# #compute contrasts "by hand" for ZIP model:
# https://hypatia.math.ethz.ch/pipermail/r-help/2009-March/418679.html
# nr <- length(levels(grifn$Treatment)) + 1
# contr <- matrix(0, nrow = nr, ncol = length(coef(zip_model)))
# colnames(contr) <- names(coef(zip_model))
# rownames(contr) <-
#   paste(levels(grifn$Treatment)[c(2, 2, 3, 3, 5, 5)],
#         levels(grifn$Treatment)[c(1, 3, 5, 1, 2, 1)], sep = " - ")
# contr[, 2:4] <-
#   contrMat(numeric(nrow(contr)), type = "Tukey")[7:12, 2:4]
# glht_zip_model <- glht(zip_model, linfct = contr)
# summary(glht_zip_model)


# data("NMES1988", package = "AER")
# nmes <- NMES1988[, c(1, 6:8)]
#
# ## models
# fm_pois <- glm(visits ~ ., data = nmes, family = poisson)
# fm_nbin <- glm.nb(visits ~ ., data = nmes)
# fm_zinb <- zeroinfl(visits ~ . | ., data = nmes, dist = "negbin")
#
# ## generalized linear hypotheses
# glht_pois <- glht(fm_pois, linfct = mcp(health = "Tukey"))
# glht_nbin <- glht(fm_nbin, linfct = mcp(health = "Tukey"))
#
# ## compute contrasts "by hand" for ZINB
# nr1 <- length(levels(nmes$health))
# contr1 <- matrix(0, nrow = nr1, ncol = length(coef(fm_zinb)))
# colnames(contr1) <- names(coef(fm_zinb))
# rownames(contr1) <- paste(levels(nmes$health)[c(2, 3, 3)],
#                           levels(nmes$health)[c(1, 1, 2)], sep = " - ")
# contr1[, 3:4] <-
#   contrMat(numeric(nrow(contr1)), type = "Tukey")[, -2]
# glht_zinb <- glht(fm_zinb, linfct = contr1)
#
# ## multiple comparisons
# summary(glht_pois)
# summary(glht_nbin)
# summary(glht_zinb)


#making a compact letter display for each treatment
q <- glht(glm_1, linfct = mcp(Treatment = "Tukey"))

sink(file = 'data/rrv_actigard_cld_letters.txt')
cld(q)
sink()

#cleanup
rm(list = ls())
