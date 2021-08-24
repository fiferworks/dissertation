#####LOADING PACKAGES####
pkgs <-
  c('tidyverse',
    'ggthemes',
    'showtext',
    'extrafont',
    'Cairo',
    'lubridate',
    'scales')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths('fonts')

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
df <- read_csv('data/rrv_pheno_clean_datasheet.csv')

#making sure months are interpreted correctly
df$month <- month(df$date, label = T, abbr = FALSE)

#adding year column
df$year <- year(df$date)

df1 <- df %>% filter(year == 2020)

# df1 <- df1 %>% filter(id == 'Pheno 11' |
#                         id == 'Pheno 12' |
#                         id == 'Pheno 13' | id == 'Pheno 14')

####SUMMARY STATS####
df1 <- df1 %>% group_by(month) %>% mutate(
  totals = sum(eriophyoids),
  per_plant = mean(eriophyoids),
  sd = sd(eriophyoids),
  se = sd(eriophyoids) / sqrt(n()),
  log_xformed = log(mean(eriophyoids)),
  se_xformed = log(sd(eriophyoids) / sqrt(n())),
) %>%
  ungroup()

#makes a list of totals for each month to display on the graphs
pheno <- df1 %>% group_by(month) %>% summarize(
  totals = sum(eriophyoids),
  tot_per_g = sum(`erios/gram`),
  per_plant = mean(eriophyoids),
  sd = sd(eriophyoids),
  se = sd(eriophyoids) / sqrt(n()),
  sd_per_g = sd(`erios/gram`),
  se_per_g = sd(`erios/gram`) / sqrt(n()),
  log_xformed = log(mean(eriophyoids)),
  se_xformed = log(sd(eriophyoids) / sqrt(n())),
  n_samples = n()
) %>%
  ungroup()

####GRAPHS####
#average pf per plant
ggplot(data = pheno,
       mapping = aes(x = month, y = tot_per_g, fill = month)) +
  geom_bar(stat = 'identity') +
  geom_segment(
    data = pheno,
    mapping = aes(
      x = 2.5,
      y = 930,
      xend = 2.5,
      yend = 0
    ),
    size = 2,
    color = "red"
  ) +
  geom_errorbar(
    aes(ymin = tot_per_g - se_per_g, ymax = tot_per_g + se_per_g),
    width = 0.65,
    size = 1,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.9, 1000)) +
  geom_text(
    aes(month, totals, label = paste0("n = ", totals), fill = NULL),
    size = 30,
    position = position_fill(),
    vjust = 1.3
  ) +
  geom_text(
    aes(
      month,
      tot_per_g,
      label = round(tot_per_g, digits = 3),
      fill = NULL
    ),
    size = 30,
    position = position_stack(),
    vjust = -1.3
  ) +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression(
    'Number of' ~ italic(P. ~ fructiphilus) ~ 'collected per gram of rose dry weight'
  )) +
  ylab("mites/g") +
  theme(axis.title.x = element_blank(), axis.text = element_blank()) +
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
    ),
    axis.title.y = element_text(
      color = "grey20",
      size = 80,
      angle = 0,
      hjust = 0,
      vjust = 0.5,
      face = "bold"
    )
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2.5,
    y = 980,
    label = "Pruned",
    color = "red"
  )

#saving the file
ggsave(
  'figure/rrv_pheno_bargraph.png',
  plot = last_plot(),
  type = "cairo",
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls(all.names = TRUE))
