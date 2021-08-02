####PACKAGES####
pkgs <- c('tidyverse',
    'viridis',
    'sysfonts',
    'showtext',
    'ggthemes',
    'Cairo',
    'eply')

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

lapply(pkgs, library, character.only = T)

rm(pkgs)

# ####GETTING REQUIRED FONTS####
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

###########################################################################################
#IMPORTANT NOTE! WE DID NOT RECOVER ERIOPHYOIDS FROM THESE SAMPLES! THESE ARE OTHER MITES!#
###########################################################################################

####IPM DATA ANALYSIS ACROSS ALL TRIALS####
#reading in the data
df <- read_csv('ipm.csv')

#assigning columns as factors
df$treat <- as_factor(df$treat)
df$id <- as_factor(df$id)
df$plant <- as_factor(df$plant)
df$block <- as_factor(df$block)
df$field <- as_factor(df$field)

#Athens only data
athns <- filter(df, field == 'Athens')

#Griffin only data
grifn <- filter(df, field == 'Griffin')

####ADDING SIGNIFICANCE LETTERS TO TABLES FOR USE IN GRAPHS####
#reading in the difference letters for both sites, ignore the warning
abc <- read_table("ipm_cld_letters.txt")

#dropping the misread columns and renaming the 'ninja + mites' treatment
abc <-
  dplyr::select(abc,
         everything(), -'mites_1', -'+',
         'mites + ninja' = 'ninja_1')

#rows to columns
abc <- abc %>% gather(key = 'treat')

#removing extra characters
abc$value <- unquote(abc$value)
abc$value <- as_factor(abc$value)
abc$treat <- as_factor(abc$treat)

#combining letters with dataset
df <- full_join(df, abc, by = 'treat')

####SUMMARY STATS OF BOTH SITES####
#getting summary stats for each treatment group
ipm <- df %>% group_by(treat) %>% summarize(
  per_plant = mean(other),
  totals = sum(other),
  sd = sd(other),
  se = sd(other) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm$per_plant <- round(ipm$per_plant, digits = 2)

#combining letters with dataset
ipm <- full_join(ipm, abc, by = 'treat')

####ATHENS####
#difference letters for Athens, ignore the warning
abc <- read_table("ipm_cld_letters_athns.txt")

#dropping the misread columns and renaming the 'ninja + mites' treatment
abc <-
  dplyr::select(abc,
         everything(), -'mites_1', -'+',
         'mites + ninja' = 'ninja_1')

#rows to columns
abc <- abc %>% gather(key = 'treat')

#removing extra characters
abc$value <- unquote(abc$value)
abc$value <- as_factor(abc$value)
abc$treat <- as_factor(abc$treat)

#combining letters with dataset
athns <- full_join(athns, abc, by = 'treat')

####SUMMARY STATS ATHENS####
#getting summary stats for each treatment group
ipm_2 <- athns %>% group_by(treat) %>% summarize(
  per_plant = mean(other),
  totals = sum(other),
  sd = sd(other),
  se = sd(other) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_2$per_plant <- round(ipm_2$per_plant, digits = 2)

#combining letters with dataset
ipm_2 <- full_join(ipm_2, abc, by = 'treat')


####GRIFFIN####
#difference letters for Athens, ignore the warning
abc <- read_table("ipm_cld_letters_grifn.txt")

#dropping the misread columns and renaming the 'ninja + mites' treatment
abc <-
  dplyr::select(abc,
                everything(), -'mites_1', -'+',
                'mites + ninja' = 'ninja_1')

#rows to columns
abc <- abc %>% gather(key = 'treat')

#removing extra characters
abc$value <- unquote(abc$value)
abc$value <- as_factor(abc$value)
abc$treat <- as_factor(abc$treat)

#combining letters with dataset
grifn <- full_join(grifn, abc, by = 'treat')

####SUMMARY STATS GRIFFIN####
#getting summary stats for each treatment group
ipm_3 <- grifn %>% group_by(treat) %>% summarize(
  per_plant = mean(other),
  totals = sum(other),
  sd = sd(other),
  se = sd(other) / sqrt(n())
) %>%
  ungroup()

#rounding for graph
ipm_3$per_plant <- round(ipm_3$per_plant, digits = 2)

#combining letters with dataset
ipm_3 <- full_join(ipm_3, abc, by = 'treat')

#data is now ready to be graphed


####GRAPHS####
#####GRAPH OF BOTH SITES####
ggplot(data = ipm,
       mapping = aes(x = treat, y = per_plant, fill = treat)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = per_plant - se, ymax = per_plant + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.3, 8)) +
  geom_text(
    mapping = aes(x = treat, label = value),
    data = ipm,
    stat = "identity",
    position = position_stack(1.7),
    vjust = -1.7,
    size = 50
  ) +
  geom_text(
    aes(treat, per_plant, label = per_plant, fill = NULL),
    stat = "identity",
    position = position_stack(1.6),
    vjust = -1,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 6,
    size = 25,
    data = df
  ) +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression('Number of herbivorous mites per plant - IPM Trials 2019')) +
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
    6,
    begin = 1,
    end = 0,
    option = 'C'
  )) +
  
  #saving the file
  ggsave(
    '../images/ipm_graph.png',
    plot = last_plot(),
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )

#####GRAPH OF ATHENS####
ggplot(data = ipm_2,
       mapping = aes(x = treat, y = per_plant, fill = treat)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = per_plant - se, ymax = per_plant + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.3, 8)) +
  geom_text(
    mapping = aes(x = treat, label = value),
    data = ipm_2,
    stat = "identity",
    position = position_stack(1.7),
    vjust = -1.7,
    size = 50
  ) +
  geom_text(
    aes(treat, per_plant, label = per_plant, fill = NULL),
    stat = "identity",
    position = position_stack(1.6),
    vjust = -1,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 6,
    size = 25,
    data = athns
  ) +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression('Number of \'Other Mites\' per plant - IPM Trials - Athens 2019')) +
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
    6,
    begin = 1,
    end = 0,
    option = 'C'
  )) +
  
  #saving the file
  ggsave(
    '../images/ipm_graph_athens.png',
    plot = last_plot(),
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )


#####GRAPH OF GRIFFIN####
ggplot(data = ipm_3,
       mapping = aes(x = treat, y = per_plant, fill = treat)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = per_plant - se, ymax = per_plant + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.3, 8)) +
  geom_text(
    mapping = aes(x = treat, label = value),
    data = ipm_3,
    stat = "identity",
    position = 'fill',
    vjust = -0.5,
    size = 50
  ) +
  geom_text(
    aes(treat, per_plant, label = per_plant, fill = NULL),
    stat = "identity",
    position = position_stack(1.6),
    vjust = -1,
    size = 30
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    position = 'fill',
    vjust = 6,
    size = 25,
    data = grifn
  ) +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression('Number of \'Other Mites\' per plant - IPM Trials - Griffin 2019')) +
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
    6,
    begin = 1,
    end = 0,
    option = 'C'
  )) +
  
  #saving the file
  ggsave(
    '../images/ipm_graph_griffin.png',
    plot = last_plot(),
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )

#cleanup
rm(list = ls(all.names = TRUE))