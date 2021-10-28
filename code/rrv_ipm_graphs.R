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
df <- read_csv('data/rrv_ipm_master_datasheet.csv')

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
ltrs_tet_all <-
  read_table("data/rrv_ipm_cld_other_mites.txt", n_max = 8)

####SUMMARY STATS FOR TETRANYCHOIDS AT ALL SITES####
ipm_tet <-
  df %>% group_by(Treatment) %>% summarize(
    'tetranychoids/plant' = mean(`Other Mites`, na.rm = TRUE),
    totals = sum(`Other Mites`, na.rm = TRUE),
    sd = sd(`Other Mites`, na.rm = TRUE),
    se = sd(`Other Mites`, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

#rounding for graph
ipm_tet$'tetranychoids/plant' <-
  round(ipm_tet$'tetranychoids/plant', digits = 2)

#helper function to convert the cld numbers to letters
convert_cld <- function(x) {
  chartr("123456789", "abcdefghi", x)
}

#combining letters with dataset
ipm_tet <- left_join(ipm_tet, ltrs_tet_all, by = 'Treatment')

#converting letters
ipm_tet$.group <- convert_cld(ipm_tet$.group)

####ATHENS####
#difference letters for Athens, ignore the warning
ltrs_tet_athns <-
  read_table("data/rrv_ipm_cld_athns.txt", n_max = 6)

#getting summary stats for each treatment group
ipm_tet_athns <- athns %>% group_by(Treatment) %>% summarize(
  'tetranychoids/plant' = mean(`Other Mites`, na.rm = TRUE),
  totals = sum(`Other Mites`, na.rm = TRUE),
  sd = sd(`Other Mites`, na.rm = TRUE),
  se = sd(`Other Mites`, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_tet_athns$'tetranychoids/plant' <-
  round(ipm_tet_athns$'tetranychoids/plant', digits = 2)

#combining letters with dataset
ipm_tet_athns <-
  left_join(ipm_tet_athns, ltrs_tet_athns, by = 'Treatment')

ipm_tet_athns$.group <- convert_cld(ipm_tet_athns$.group)


####TALLAHASSEE####
#getting summary stats for tetranychoids
ipm_tet_talla <- talla %>% group_by(Treatment) %>% summarize(
  'mean_tets/g' = mean(Tetranychoids / grams_dry_weight, na.rm = TRUE),
  totals = sum(Tetranychoids, na.rm = TRUE),
  sd = sd(Tetranychoids, na.rm = TRUE),
  se = sd(Tetranychoids, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_tet_talla$`mean_tets/g` <-
  round(ipm_tet_talla$`mean_tets/g`, digits = 2)

ltrs_tet_talla <-
  read_table("data/rrv_ipm_cld_tet_talla.txt", n_max = 6)

#combining letters with dataset
ipm_tet_talla <-
  left_join(ipm_tet_talla, ltrs_tet_talla, by = 'Treatment')

#converting the numbers to letters
ipm_tet_talla$.group <- convert_cld(ipm_tet_talla$.group)


#now with eriophyoids
ipm_erio_talla <- talla %>% group_by(Treatment) %>% summarize(
  'mean_erios/g' = mean(Eriophyoids / grams_dry_weight, na.rm = TRUE),
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

ltrs_erio_talla$.group <- convert_cld(ltrs_erio_talla$.group)

#combining letters with dataset
ipm_erio_talla <-
  left_join(ipm_erio_talla, ltrs_erio_talla, by = 'Treatment')


#now with phytoseiids
ipm_preds_talla <- talla %>% group_by(Treatment) %>% summarize(
  `mean_phytos/g` = mean(Phytoseiids / grams_dry_weight, na.rm = TRUE),
  totals = sum(Phytoseiids, na.rm = TRUE),
  sd = sd(Phytoseiids / grams_dry_weight, na.rm = TRUE),
  se = sd(Phytoseiids / grams_dry_weight, na.rm = TRUE) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_preds_talla$`mean_phytos/g` <-
  round(ipm_preds_talla$`mean_phytos/g`, digits = 2)

ltrs_preds_talla <-
  read_table("data/rrv_ipm_cld_pred_talla.txt", n_max = 6)

ltrs_preds_talla$.group <- convert_cld(ltrs_preds_talla$.group)

#combining letters with dataset
ipm_preds_talla <-
  left_join(ipm_preds_talla, ltrs_preds_talla, by = 'Treatment')

#data is now ready to be graphed
####GRAPHS####
#####GRAPH OF ALL TALLAHASSEE ERIOS####
ggplot(data = talla,
       mapping = aes(x = Treatment, y = `erios/g`, fill = Treatment)) +
  geom_boxplot(
    lwd = 2.5,
    notch = TRUE,
    varwidth = TRUE,
    outlier.size = 2.5
  ) +
  theme_tufte(base_size = 70, base_family = "gill_sans") +
  coord_cartesian(ylim = c(-0.4, 40), clip = "off") +
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
  scale_fill_manual(values = c(
    "#CDC08C",
    "#85D4E3",
    "#9C964A",
    "#C27D38",
    "#F4B5BD",
    "#798E87",
    "#FAD77B"
  )) +
  geom_text(
    data = ipm_erio_talla,
    mapping = aes(x = Treatment, y = `mean_erios/g`, label = .group),
    stat = "identity",
    position = position_stack(1),
    vjust = -3,
    hjust = 2,
    size = 30
  ) +
geom_text(
    data = ipm_erio_talla,
    mapping = aes(x = Treatment, y = `mean_erios/g`, label = `mean_erios/g`),
    stat = "identity",
    position = position_stack(1),
    vjust = -3,
    hjust = -0.3,
    size = 30
  ) +
geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 2,
    size = 25,
    data = talla
  )


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

####GRAPH OF ALL TALLAHASSEE TETRANYCHOIDS####
ggplot(data = talla,
       mapping = aes(x = Treatment, y = Tetranychoids/grams_dry_weight, fill = Treatment)) +
  geom_boxplot(
    lwd = 2.5,
    notch = TRUE,
    varwidth = TRUE,
    outlier.size = 2.5
  ) +
  theme_tufte(base_size = 70, base_family = "gill_sans") +
  coord_cartesian(ylim = c(-0.4, 10), clip = "off") +
  ggtitle(
    expression(
      'Mean of tetranychoid mites per gram dry weight - Tallahassee IPM Trials 2020-2021'
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
  scale_fill_manual(values = c(
    "#CDC08C",
    "#85D4E3",
    "#9C964A",
    "#C27D38",
    "#F4B5BD",
    "#798E87",
    "#FAD77B"
  )) +
  geom_text(
    data = ipm_tet_talla,
    mapping = aes(x = Treatment, y = `mean_tets/g`, label = .group),
    stat = "identity",
    position = position_stack(1),
    vjust = -1,
    hjust = 2,
    size = 30
  ) +
  geom_text(
    data = ipm_tet_talla,
    mapping = aes(x = Treatment, y = `mean_tets/g`, label = `mean_tets/g`),
    stat = "identity",
    position = position_stack(1),
    vjust = -1,
    hjust = -0.3,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 5,
    size = 25,
    data = talla
  )


#saving the file
ggsave(
  'figure/rrv_ipm_graph_tets_talla.png',
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
  facet_wrap( ~ Month, strip.position = 'top') +
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