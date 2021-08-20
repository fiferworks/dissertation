####SETUP####
pkgs <-
  c('tidyverse',
    'viridis',
    'extrafont',
    'showtext',
    'ggthemes',
    'Cairo',
    'eply')

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths('fonts/')

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

####ACTIGARD DATA PREP####
#reading in the data
df  <-
  read_csv('data/rrv_actigard_master_datasheet.csv', col_types = "dddddddfffffDf")

#reading in the difference letters
abc <- read_table("data/rrv_actigard_cld_letters.txt")

#rows to columns
abc <- abc %>% gather(key = 'treat')

#removing extra characters
abc$value <- unquote(abc$value)
abc$value <- as_factor(abc$value)
abc$treat <- as_factor(abc$treat)

#combining letters with dataset
df <- full_join(df, abc, by = 'treat')

#rearranging columns
df <-
  dplyr::select(
    df,
    'mites',
    'totals',
    'per_plant',
    'sd',
    'se',
    'log_xformed',
    'se_xformed',
    'treat',
    'value',
    'id',
    'plant',
    'block',
    'field',
    'date',
    'n_samples'
  )

#getting summary stats for each treatment group
actgrd <- df %>% group_by(treat) %>% summarize(
  per_plant = mean(mites),
  totals = sum(mites),
  sd = sd(mites),
  se = sd(mites) / sqrt(n()),
  log_xformed = log(mean(mites)),
  se_xformed = log(sd(mites) / sqrt(n()))
) %>%
  ungroup()

#rounding for graph
actgrd$per_plant <- round(actgrd$per_plant, digits = 2)

#combining letters with dataset
actgrd <- full_join(actgrd, abc, by = 'treat')

####GRAPHS####
#####graphs of the different tests####
ggplot(data = actgrd,
       mapping = aes(x = treat, y = per_plant, fill = treat)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = per_plant - se, ymax = per_plant + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.1, 1.9)) +
  geom_text(
    mapping = aes(x = treat, label = value),
    data = actgrd,
    stat = "identity",
    position = position_stack(2.1),
    vjust = -.5,
    size = 40
  ) +
  geom_text(
    aes(treat, per_plant, label = per_plant, fill = NULL),
    stat = "identity",
    position = position_stack(1.3),
    vjust = -6,
    size = 40
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 13,
    size = 35,
    data = df
  ) +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression(
    'Number of' ~ italic(P.fructiphilus) ~ 'per plant - Georgia Trials 2018 - 2019'
  )) +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(
      size = 100,
      face = "bold",
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
  ) + scale_fill_manual(values = viridis(
    5,
    begin = 0,
    end = 1,
    option = 'C'
  ))

#saving the file
ggsave(
  'figure/rrv_actigard_graph.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls(all.names = TRUE))
