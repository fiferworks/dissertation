####PACKAGES####
pkgs <- c('tidyverse',
          'viridis',
          'sysfonts',
          'showtext',
          'ggthemes',
          'Cairo',
          'eply')

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# ####GETTING REQUIRED FONTS####
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

####IPM DATA ANALYSIS ACROSS ALL TRIALS####
#reading in the data
df <- read_csv('data/ipm.csv')

#assigning columns as factors
df$Treatment <- as_factor(df$Treatment)
df$ID <- as_factor(df$ID)
df$Block <- as_factor(df$Block)
df$Field <- as_factor(df$Field)
df$Month <- lubridate::month(df$Date, label = TRUE, abbr = FALSE)

#Athens only data
athns <- filter(df, Field == 'Athens')

#Griffin only data
grifn <- filter(df, Field == 'Griffin')

#Tallahassee only data
talla <- filter(df, Field == 'Tallahassee')

####ADDING SIGNIFICANCE LETTERS TO TABLES FOR USE IN GRAPHS####
#reading in the difference letters for all sites
ltrs_erios_all <-
  read_table("data/rrv_ipm_cld_all_erios.txt", n_max = 8)

####SUMMARY STATS OF BOTH SITES####
#getting summary stats for each treatment group
ipm_erios <-
  df %>% group_by(Treatment) %>% summarize(
    'mean_erios/g' = mean(`erios/g`, na.rm = TRUE),
    totals = sum(Eriophyoids, na.rm = TRUE),
    sd = sd(`erios/g`, na.rm = TRUE),
    se = sd(`erios/g`, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

#rounding for graph
ipm_erios$'mean_erios/g' <-
  round(ipm_erios$'mean_erios/g', digits = 2)

#combining letters with dataset
ipm_erios <- left_join(ipm_erios, ltrs_erios_all, by = 'Treatment')


####ALL OTHER MITES####
ltrs_other_all <-
  read_table("data/rrv_ipm_cld_other_mites.txt", n_max = 8)

ipm_other <-
  df %>% group_by(Treatment) %>% summarize(
    'mean_mites/g' = mean(`mites/g`, na.rm = TRUE),
    totals = sum(`Other Mites`, na.rm = TRUE),
    sd = sd(`Other Mites`, na.rm = TRUE),
    se = sd(`Other Mites`, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

#rounding for graph
ipm_other$`mean_mites/g` <-
  round(ipm_other$`mean_mites/g`, digits = 2)

#combining letters with dataset
ipm_other <- left_join(ipm_other, ltrs_other_all, by = 'Treatment')

####ATHENS####
#difference letters for Athens, ignore the warning
ltrs_other_athns <-
  read_table("data/rrv_ipm_cld_athns.txt", n_max = 6)

#getting summary stats for each treatment group
ipm_other_athns <- athns %>% group_by(Treatment) %>% summarize(
  'mean_mites/plant' = mean(`Other Mites`, na.rm = TRUE),
  totals = sum(`Other Mites`, na.rm = TRUE),
  sd = sd(`Other Mites`, na.rm = TRUE),
  se = sd(`Other Mites`, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_other_athns$'mean_mites/plant' <-
  round(ipm_other_athns$'mean_mites/plant', digits = 2)

#combining letters with dataset
ipm_other_athns <-
  left_join(ipm_other_athns, ltrs_other_athns, by = 'Treatment')


####GRIFFIN####
#getting summary stats for each treatment group
ipm_other_grifn <- grifn %>% group_by(Treatment) %>% summarize(
  'mean_mites/plant' = mean(`Other Mites`, na.rm = TRUE),
  totals = sum(`Other Mites`, na.rm = TRUE),
  sd = sd(`Other Mites`, na.rm = TRUE),
  se = sd(`Other Mites`, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_other_grifn$'mean_mites/plant' <-
  round(ipm_other_grifn$'mean_mites/plant', digits = 2)

####TALLAHASSEE####
#getting summary stats for each treatment group
ipm_other_talla <- talla %>% group_by(Treatment) %>% summarize(
  'mean_mites/g' = mean(`mites/g`, na.rm = TRUE),
  totals = sum(`Other Mites`, na.rm = TRUE),
  sd = sd(`mites/g`, na.rm = TRUE),
  se = sd(`mites/g`, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_other_talla$'mean_mites/g' <-
  round(ipm_other_talla$'mean_mites/g', digits = 2)

ltrs_other_talla <-
  read_table("data/rrv_ipm_cld_other_talla.txt", n_max = 6)

#combining letters with dataset
ipm_other_talla <-
  left_join(ipm_other_talla, ltrs_other_talla, by = 'Treatment')

ipm_other_talla$.group <- c('a', 'b', 'b', 'b', 'b', 'b')

#now with eriophyoids
ipm_erio_talla <- talla %>% group_by(Treatment) %>% summarize(
  'mean_erios/g' = mean(`erios/g`, na.rm = TRUE),
  totals = sum(Eriophyoids, na.rm = TRUE),
  sd = sd(`erios/g`, na.rm = TRUE),
  se = sd(`erios/g`, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_erio_talla$'mean_erios/g' <-
  round(ipm_erio_talla$'mean_erios/g', digits = 2)

ltrs_erio_talla <-
  read_table("data/rrv_ipm_cld_erios_talla.txt", n_max = 6)

ltrs_erio_talla$.group <- c('a', 'b', 'c', 'd', 'de', 'e')


#combining letters with dataset
ipm_erio_talla <-
  left_join(ipm_erio_talla, ltrs_erio_talla, by = 'Treatment')

#data is now ready to be graphed
####GRAPHS####
#####GRAPH OF ALL TALLAHASSEE ERIOS####
ggplot(
  data = ipm_erio_talla,
  mapping = aes(x = Treatment, y = `mean_erios/g`, fill = Treatment)
) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = `mean_erios/g` - se, ymax = `mean_erios/g` + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.3, 12.5), clip = "off") +
  theme_tufte(base_size = 70, base_family = "gill_sans") +
  ggtitle(
    expression(
      'Mean of eriophyoid mites per gram dry weight - Tallahassee IPM Trials 2020-2021'
    )
  ) +
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
  ) +
  geom_text(
    mapping = aes(x = Treatment, label = .group),
    stat = "identity",
    position = position_stack(1.1),
    vjust = -3,
    size = 30
  ) +
  geom_text(
    mapping = aes(x = Treatment, label = `mean_erios/g`),
    data = ipm_erio_talla,
    stat = "identity",
    position = position_stack(1.1),
    vjust = -1.1,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 3.5,
    size = 25,
    data = talla
  ) +
  scale_fill_manual(values = viridis(
    6,
    begin = 0,
    end = 1,
    option = 'D'
  ))

#saving the file
ggsave(
  'figure/rrv_ipm_graph_erios_talla.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

####GRAPH OF ALL TALLAHASSEE OTHER MITES####
ggplot(
  data = ipm_other_talla,
  mapping = aes(x = Treatment, y = `mean_mites/g`, fill = Treatment)
) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = `mean_mites/g` - se, ymax = `mean_mites/g` + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.3, 12.5), clip = "off") +
  theme_tufte(base_size = 70, base_family = "gill_sans") +
  ggtitle(expression(
    'Mean of other mites per gram dry weight - Tallahassee IPM Trials 2020-2021'
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
  ) +
  geom_text(
    mapping = aes(x = Treatment, label = `mean_mites/g`),
    stat = "identity",
    position = position_stack(1.1),
    vjust = -1.1,
    size = 30
  ) +
  geom_text(
    mapping = aes(x = Treatment, label = .group),
    stat = "identity",
    position = position_stack(1.1),
    vjust = -3,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 3.5,
    size = 25,
    data = talla
  ) +
  scale_fill_manual(values = viridis(
    6,
    begin = 0,
    end = 1,
    option = 'D'
  ))

#saving the file
ggsave(
  'figure/rrv_ipm_graph_other_talla.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

####TALLAHASSEE BY WEEK####
may_week <-
  talla %>% filter(Month == 'May') %>% group_by(Treatment) %>% summarize(
    'mean_erios/g' = mean(`erios/g`, na.rm = TRUE),
    totals = sum(Eriophyoids, na.rm = TRUE),
    sd = sd(`erios/g`, na.rm = TRUE),
    se = sd(`erios/g`, na.rm = TRUE) / sqrt(n()),
    Treatment = Treatment,
    Month = Month
  ) %>% distinct()

jun_week <-
  talla %>% filter(Month == 'June') %>% group_by(Treatment) %>% summarize(
    'mean_erios/g' = mean(`erios/g`, na.rm = TRUE),
    totals = sum(Eriophyoids, na.rm = TRUE),
    sd = sd(`erios/g`, na.rm = TRUE),
    se = sd(`erios/g`, na.rm = TRUE) / sqrt(n()),
    Treatment = Treatment,
    Month = Month
  ) %>% distinct()

jul_week <-
  talla %>% filter(Month == 'July') %>% group_by(Treatment) %>% summarize(
    'mean_erios/g' = mean(`erios/g`, na.rm = TRUE),
    totals = sum(Eriophyoids, na.rm = TRUE),
    sd = sd(`erios/g`, na.rm = TRUE),
    se = sd(`erios/g`, na.rm = TRUE) / sqrt(n()),
    Treatment = Treatment,
    Month = Month
  ) %>% distinct()

aug_week <-
  talla %>% filter(Month == 'August') %>% group_by(Treatment) %>% summarize(
    'mean_erios/g' = mean(`erios/g`, na.rm = TRUE),
    totals = sum(Eriophyoids, na.rm = TRUE),
    sd = sd(`erios/g`, na.rm = TRUE),
    se = sd(`erios/g`, na.rm = TRUE) / sqrt(n()),
    Treatment = Treatment,
    Month = Month
  ) %>% distinct()

talla_week <- bind_rows(may_week, jun_week, jul_week, aug_week)
talla_week$`mean_erios/g` <-
  round(talla_week$`mean_erios/g`, digits = 1)
talla_week$Treatment <-
  gsub('Mites \\+ Actigard', 'MA', talla_week$Treatment)

# plots of Eriophyoids per week
ggplot(
  data = talla_week,
  mapping = aes(y = `mean_erios/g`, x = Treatment, fill = Treatment)
) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = `mean_erios/g` - se, ymax = `mean_erios/g` + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  facet_wrap(~ Month, strip.position = 'top') +
  coord_cartesian(ylim = c(-3, 17), clip = "off") +
  theme_tufte(base_size = 70, base_family = "gill_sans") +
  ggtitle(expression(
    'Mean Number of' ~ italic(P. ~ fructiphilus) ~ 'per gram of rose dry weight - IPM Tallahassee'
  )) +
  theme(axis.title = element_blank(), axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(
      size = 80,
      face = "bold",
      family = "garamond"
    ),
    axis.text.x = element_text(
      color = "grey20",
      size = 70,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 70,
      angle = 0,
      hjust = 1,
      vjust = 0,
      face = "bold"
    ),
    strip.text = element_text(size = 70)
  ) +
  geom_text(
    mapping = aes(x = Treatment, label = `mean_erios/g`),
    stat = "identity",
    position = position_stack(1.2),
    vjust = -1.1,
    size = 20
  ) +
  geom_text(
    mapping = aes(x = Treatment, label = paste0("n = ", totals)),
    stat = "identity",
    position = position_fill(-1),
    vjust = 1,
    size = 20
  ) +
  scale_fill_manual(values = viridis(
    6,
    begin = 0,
    end = 1,
    option = 'D'
  ))

#saving the file
ggsave(
  'figure/rrv_ipm_graph_erios_talla_week.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls(all.names = TRUE))