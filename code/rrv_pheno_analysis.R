#####LOADING PACKAGES####
pkgs <-
  c('tidyverse',
    'lubridate')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#reading in the required file
df <- read_csv("data/rrv_pheno_clean_datasheet.csv")

#adding year column
df$year <- year(df$date)

df <- df %>% filter(Study == 'Phenology' | id == 'Pheno 11' |
                      id == 'Pheno 12' |
                      id == 'Pheno 13' | id == 'Pheno 14')

df$id <- as_factor(df$id)
df$Block <- as_factor(df$Block)
df$Treatment <- as_factor(df$Treatment)
df$Study <- as_factor(df$Study)
df$month <- as_factor(df$month)
df$year <- as_factor(df$year)

df$`erios/g` <- replace_na(df$`erios/g`, )

pheno_lm <- lmer(`erios/g` ~ month + year + (1 | id),
                 data = df)

summary(pheno_glm)

#ANOVA
Anova(pheno_glm, type = c("III")) #not significantly different

#multiple comparisons
summary(glht(pheno_glm, linfct = mcp(year = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- glht(pheno_glm, linfct = mcp(year = "Tukey"))

sink(file = 'data/rrv_pheno_cld_letters.txt')
cld(q)
sink()

#cleanup
rm(list = ls())
