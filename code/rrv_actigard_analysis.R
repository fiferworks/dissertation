####SETUP####
pkgs <- c('lme4', 'pscl', 'car', 'boot', 'multcomp', 'lubridate', 'tidyverse')

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####ACTIGARD DATA ANALYSIS####
#reading in the data
df <- read_csv('data/rrv_actigard_master_datasheet.csv', col_types = "fdffff?")

#data is now ready to be analyzed
zip_model <- zeroinfl(Mites ~ Treatment, data = df, dist = "poisson", link ='logit')

summary(zip_model)
Anova(zip_model, type = c("III")) #treatment is significant

# dput(coef(zip_model, "count"))
# dput(coef(zip_model, "zero"))
# 
# f <- function(data, i) {
#   require(pscl)
#   m <- zeroinfl(Mites ~ Treatment,
#                 data = data[i,],
#                 start = list(
#                   count = c(
#                     2.7850328037862,
#                     -7.58605319814307,
#                     0.339553706088603,
#                     -0.103990370364982,
#                     -3.86373629052554e-09
#                   ),
#                   zero = c(
#                     2.379547947819,
#                     -7.55230340935205,
#                     0.888141120188275,
#                     0.564874029193537,
#                     15.1891111024181
#                   )
#                 ))
#   as.vector(t(do.call(rbind, coef(summary(
#     m
#   )))[, 1:2]))
# }
# 
# set.seed(5)
# res <- boot(df, f, R = 1200, parallel = "snow", ncpus = 8)
# res
# 
# parms <- t(sapply(c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19), function(i) {
#   out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
#   with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
#               bcaLL = bca[4], bcaLL = bca[5]))
# }))

#glmer model,  Model is nearly unidentifiable: large eigenvalue ratio
glm_1 <- glmer(Mites ~ Treatment + (1 | Block), family = 'poisson', data = df)

summary(glm_1)

#ANOVA
Anova(glm_1, type = c("III")) #treatment is significant

#multiple comparisons
#suggests that both the low Actigard rate and Kontos treatments are significantly different than the water treatment
summary(glht(glm_1, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- glht(glm_1, linfct = mcp(Treatment = "Tukey"))

sink(file = 'data/rrv_actigard_cld_letters.txt')
cld(q)
sink()

#cleanup
rm(list = ls())
