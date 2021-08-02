#####LOADING PACKAGES####
pkgs <-
  c('tidyverse', 'ggthemes', 'showtext', 'extrafont', 'Cairo')
lapply(pkgs, library, character.only = T)

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

####GRAPHS OF OLFACTOMETER DATA####
#reading in olfactometer data, making sure R knows df are factors
df <-
  read_csv('data/rrv_all_olfactometer_flat.csv', col_types = cols(choice = col_factor(c(
    'no choice', 'control', 'experiment'
  ))))

df$trial <- sub('rose', 'Healthy Rose', df$trial)
df$trial <- sub('MeSA', 'Methyl Salicylate', df$trial)
df$trial <- sub('limonene', 'Limonene', df$trial)
df$trial <- sub('rrv', 'RRV-Infected', df$trial)

#filtering out no choice from the original dataset for graphs
df <- filter(df, choice != 'no choice')
rose_df <-
  filter(df, trial == 'Healthy Rose' | trial == 'RRV-Infected')
tests_df <-
  filter(df, trial == 'Methyl Salicylate' | trial == 'Limonene')

#reading in chisquared tests
pvals <- read_csv("data/rrv_olfact_chisq_tests.csv")

#####graphs of the rose comparisons####
ggplot(data = rose_df,
       mapping = aes(x = trial, y = chs, fill = choice)) +
  geom_bar(position = 'stack', stat = 'identity') +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  ggtitle(expression(italic(A. ~ swirskii) ~ 'attraction to RRV-infected roses')) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  labs(fill = 'Choice:') +
  theme(
    plot.title = element_text(
      size = 100,
      face = "bold",
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
  theme(axis.title = element_blank()) +
  geom_hline(yintercept = seq(-100, 0, 100),
             col = 'white',
             lwd = 1) +
  coord_flip(ylim = c(-110, 120)) +
  scale_fill_manual(values = c("#B1B3B6", "#CB5382")) +
  annotate(
    geom = "text",
    size = 30,
    x = 2,
    y = 50,
    label = pvals$observed.experiment[2],
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 30,
    x = 2,
    y = -50,
    label = pvals$observed.control[2],
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 30,
    x = 1,
    y = 50,
    label = "90",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 30,
    x = 1,
    y = -50,
    label = "81",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2,
    y = -100,
    label = paste0("n = ", sum(pvals$`observed.no choice`[2] + pvals$observed.control[2] + pvals$observed.experiment[2])),
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1,
    y = -100,
    label = "n = 200",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 0.8,
    y = 110,
    label = "p = 0.49",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1.8,
    y = 110,
    label = paste0("n = ", signif(pvals$p.value[2], digits = 3)),
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1,
    y = 110,
    label = "N.S.",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 50,
    x = 2,
    y = 110,
    label = "***",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2.53,
    y = -40,
    label = 'Healthy Rose',
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2.53,
    y = 40,
    label = 'Infected Rose',
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1.5,
    y = -35,
    label = 'Filtered Air',
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1.5,
    y = 40,
    label = 'Healthy Rose',
    color = "black"
  ) +
  scale_y_reverse()

#saving the file
ggsave(
  'figure/rose_graph.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)


#####graphs of the different tests####
ggplot(data = tests_df,
       mapping = aes(x = trial, y = chs, fill = choice)) +
  geom_bar(position = 'stack', stat = 'identity') +
  theme_tufte(base_size = 20, base_family = "gill_sans") +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(
    plot.title = element_text(
      size = 100,
      face = "bold",
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
  ggtitle(expression(italic(A. ~ swirskii) ~ 'attraction to VOCs From roses')) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  labs(fill = 'Choice:') +
  geom_hline(yintercept = seq(-50, 0, 50),
             col = 'white',
             lwd = 1) +
  coord_flip(ylim = c(-50, 50)) +
  scale_fill_manual(values = c("#B1B3B6", "#E28F41")) +
  annotate(
    geom = "text",
    size = 30,
    x = 2,
    y = -20,
    label = "34",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 30,
    x = 1,
    y = -20,
    label = "22",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 30,
    x = 2,
    y = 20,
    label = "40",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 30,
    x = 1,
    y = 20,
    label = "16",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2,
    y = -45,
    label = "n = 100",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1,
    y = -45,
    label = "n = 40",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = .8,
    y = 42,
    label = "p = 0.33",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1.8,
    y = 42,
    label = "p = 0.48",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 1,
    y = 42,
    label = "N.S.",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2,
    y = 42,
    label = "N.S.",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2.53,
    y = -15,
    label = "Filtered Air",
    color = "black"
  ) +
  annotate(
    geom = "text",
    size = 25,
    x = 2.53,
    y = 20,
    label = '100 μm of 1 g/ml',
    color = "black"
  ) +
  scale_y_reverse()


#saving the file
ggsave(
  'figure/tests_graph.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls(all.names = TRUE))