#####LOADING PACKAGES####
pkgs <-
  c('tidyverse',
    'readxl',
    'ggthemes',
    'showtext',
    'extrafont',
    'Cairo',
    'writexl')

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

lapply(pkgs, library, character.only = T)

####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths('../fonts')

#imports the font Gill Sans MT as the font family 'gill_sans'
font_add(
  family = 'gill_sans',
  regular = "GIL_____.TTF",
  bold = "GILB____.TTF",
  italic = "GILBI___.TTF",
  bolditalic = "GILI____.TTF"
)

#imports the font Garamond MT as the font family 'garamond'
font_add(
  family = 'garamond',
  regular = "AGaramondPro-Regular.otf",
  bold = "AGaramondPro-Bold.otf",
  italic = "AGaramondPro-BoldItalic.otf",
  bolditalic = "AGaramondPro-Italic.otf"
)

# #uncommment and run to make sure it worked
# #it should list "gill_sans" and "garamond"
# font_families()

showtext_auto()


####READING IN THE REQUIRED FILES####
df <- read_xlsx('all_mites_survey.xlsx')

#making sure certain columns are considered as factors
df$p_fructiphilus <- as.factor(df$p_fructiphilus)
df$month <- as.factor(df$month)

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

####SUMMARY STATS####
df_fl <- df_fl %>% group_by(month) %>% mutate(
  totals = sum(eriophyoids),
  per_plant = mean(eriophyoids),
  sd = sd(eriophyoids),
  se = sd(eriophyoids) / sqrt(n()),
  log_xformed = log(mean(eriophyoids)),
  se_xformed = log(sd(eriophyoids) / sqrt(n())),
) %>%
  ungroup()

#makes a list of totals for each month to display on the graphs
surveys <- df_fl %>% group_by(month) %>% summarize(
  totals = sum(eriophyoids),
  per_plant = mean(eriophyoids),
  sd = sd(eriophyoids),
  se = sd(eriophyoids) / sqrt(n()),
  log_xformed = log(mean(eriophyoids)),
  se_xformed = log(sd(eriophyoids) / sqrt(n())),
  n_samples = n()
) %>%
  ungroup()

#rounding for graph
surveys$log_xformed  <- round(surveys$log_xformed, digits = 2)

####GRAPHS####
#####graphs of the different tests####
#plots the data
ggplot(data = surveys,
       mapping = aes(x = month, y = log_xformed, fill = month)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = log_xformed - se_xformed, ymax = log_xformed + se_xformed),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.9, 10)) +
  geom_text(
    aes(month, log_xformed, label = log_xformed, fill = NULL),
    size = 40,
    position = position_stack(2)
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 3.5,
    size = 40,
    data = df_fl
  ) +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression(
    'Log number of' ~ italic(P. ~ fructiphilus) ~ 'per site in Leon County 2019'
  )) +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(
      size = 100,
      face = "bold",
      hjust = 0.1,
      color = "grey20",
      family = "garamond"
    ),
    axis.text.x = element_text(
      color = "grey20",
      size = 80,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 80,
      angle = 0,
      hjust = 1,
      vjust = 0,
      face = "bold"
    )
  ) +
  annotate(
    geom = "text",
    size = 60,
    x = 2,
    y = 8.7,
    label = "***",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 35,
    x = 2,
    y = 8.2,
    label = "p-value = 0.001",
    color = "black"
  ) +
  scale_fill_manual(values = c("#B1B3B6", "#CB5382")) +
  
  #saving the file
  ggsave(
    '../images/survey_graph.png',
    plot = last_plot(),
    type = "cairo",
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )

# #cleanup
rm(list = ls(all.names = TRUE))