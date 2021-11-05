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
  read_csv('data/rrv_actigard_master_datasheet.csv')

df <- df %>% filter(Field == 'Griffin')

#reading in the difference letters
abc <- read_table("data/rrv_actigard_cld_letters.txt")
abc <- abc %>%  select('Water', 'High', 'Low', 'Spiro')

#rows to columns
abc <-
  abc %>% pivot_longer(everything(), names_to = "Treatment", values_to = "Letters")

#removing extra characters
abc$Letters <- unquote(abc$Letters)
abc$Letters <- as_factor(abc$Letters)
abc$Treatment <- as_factor(abc$Treatment)

#combining letters with dataset
df <- full_join(df, abc, by = "Treatment")

#rearranging columns
df <-
  df %>% dplyr::select(
    "Sample #",
    "Treatment",
    "Total P.fructiphilus",
    "Other Mites",
    "Plant",
    "Block",
    "Field",
    "H-B Score",
    "RRD",
    "AUDPC",
    "Final disease severity (%)",
    "Date",
    "Letters"
  )

#getting summary stats for each treatment group
actgrd_griffin <-
  df %>% group_by(Treatment) %>% summarize(
    per_plant = mean(`Total P.fructiphilus`, na.rm = TRUE),
    totals = sum(`Total P.fructiphilus`, na.rm = TRUE),
    sd = sd(`Total P.fructiphilus`, na.rm = TRUE),
    se = sd(`Total P.fructiphilus`, na.rm = TRUE) / sqrt(n()),
    log_xformed = log(mean(`Total P.fructiphilus`, na.rm = TRUE)),
    se_xformed = log(sd(`Total P.fructiphilus`, na.rm = TRUE) / sqrt(n()))
  ) %>%
  ungroup()

#rounding for graph
actgrd_griffin$per_plant <-
  round(actgrd_griffin$per_plant, digits = 2)

#combining letters with dataset
actgrd_griffin <- full_join(actgrd_griffin, abc, by = "Treatment")


####GRAPHS####
#####graphs of the different tests####
ggplot(
  data = df,
  mapping = aes(x = Treatment, y = `Total P.fructiphilus`, fill = Treatment)
) +
  geom_boxplot(
    lwd = 2.5,
    notch = TRUE,
    varwidth = TRUE,
    outlier.size = 2.5
  ) +
  coord_cartesian(ylim = c(-0.4, 60)) +
  geom_text(
    mapping = aes(x = Treatment, y = per_plant, label = Letters),
    data = actgrd_griffin,
    stat = "identity",
    position = position_stack(1),
    vjust = -1.5,
    size = 30
  ) +
  geom_text(
    data = actgrd_griffin,
    mapping = aes(
      x = Treatment,
      y = per_plant,
      label = per_plant,
      fill = NULL
    ),
    stat = "identity",
    position = position_stack(1.5),
    vjust = -3,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 2,
    size = 25,
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
      hjust = 0.7,
      vjust = 0,
      face = "bold"
    )
  ) + scale_fill_manual(values = c(
    "#CDC08C",
    "#85D4E3",
    "#9C964A",
    "#C27D38",
    "#F4B5BD",
    "#798E87",
    "#FAD77B"
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
