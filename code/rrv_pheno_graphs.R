#####LOADING PACKAGES####
pkgs <-
  c('tidyverse',
    'ggthemes',
    'showtext',
    'extrafont',
    'Cairo',
    'lubridate')

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
df$month <- month(df$date, label = T, abbr = TRUE)

#adding year column
df$year <- year(df$date)

df <- df %>% filter(Study == 'Phenology' | id == 'Pheno 11' |
                      id == 'Pheno 12' |
                      id == 'Pheno 13' | id == 'Pheno 14')

####SUMMARY STATS####
#makes a list of totals for each month to display on the graphs
pheno <- df %>% group_by(month, year) %>% summarize(
  totals = sum(eriophyoids, na.rm = TRUE),
  tot_per_g = mean(`erios/g`, na.rm = TRUE),
  per_plant = mean(eriophyoids, na.rm = TRUE),
  sd = sd(eriophyoids, na.rm = TRUE),
  se = sd(eriophyoids, na.rm = TRUE) / sqrt(n()),
  sd_per_g = sd(`erios/g`, na.rm = TRUE),
  se_per_g = sd(`erios/g`, na.rm = TRUE) / sqrt(n()),
  log_xformed = log(mean(eriophyoids, na.rm = TRUE)),
  se_xformed = log(sd(eriophyoids, na.rm = TRUE) / sqrt(n())),
  n_samples = n()
) %>% arrange(year) %>%
  ungroup()


####GRAPHS####
#average pf per plant
pheno_graph <- ggplot(data = pheno,
                      mapping = aes(x = month, y = tot_per_g, fill = month)) +
  geom_bar(stat = 'identity') +
  facet_grid(. ~ year) +
  geom_errorbar(
    aes(ymin = tot_per_g - se_per_g, ymax = tot_per_g + se_per_g),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-2, 60), clip = "off") +
  geom_text(
    aes(
      month,
      tot_per_g,
      label = round(tot_per_g, digits = 1),
      fill = NULL
    ),
    size = 30,
    position = position_stack(1.5),
    vjust = -.7
  ) +
  theme_tufte(base_size = 70, base_family = "gill_sans") +
  ggtitle(expression(
    'Mean Number of' ~ italic(P. ~ fructiphilus) ~ 'collected per gram of rose dry weight'
  )) +
  ylab("mites/g") +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    strip.text.x = element_text(size = 80, face = "bold")
  ) +
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
  )


dat_text <- data.frame(
  label = c("Pruned"),
  month   = c("Jul"),
  year = c(2020)
)

pheno_graph <- pheno_graph + geom_text(
  data    = dat_text,
  mapping = aes(x = 4.2, y = 50, label = label),
  color = "red",
  size = 25,
  hjust   = -0.1,
  vjust   = -1
) +
  geom_segment(
    data = dat_text,
    mapping = aes(
      x = 5,
      y = 50,
      xend = 5,
      yend = 0
    ),
    size = 2,
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
