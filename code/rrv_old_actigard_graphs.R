####PACKAGES####
pkgs <-
  c('tidyverse', 'viridis', 'extrafont', 'ggthemes')
lapply(pkgs, library, character.only = T)
rm(pkgs)

####ACTIGARD DATA PREP####
#reading in the data
df <- read_csv('actigrd.csv')

####GRIFFIN TRIALS####
#selecting Griffin trials
df_griff <- filter(df, field == 'griffin')

#makes a column with totals for each treatment to display on the graph
totals <-
  df_griff %>% group_by(treat) %>% summarize(total = sum(mites))

#assigning columns as factors
df_griff$treat <- as_factor(df_griff$treat)
df_griff$id <- as_factor(df_griff$id)
df_griff$plant <- as_factor(df_griff$plant)
df_griff$block <- as_factor(df_griff$block)
df_griff$field <- as_factor(df_griff$field)


####GRIFFIN GRAPHS####
#####graphs of the different tests####
ggplot(data = df_griff,
       mapping = aes(x = treat, y = mites, fill = treat)) +
  geom_bar(position = 'stack', stat = 'identity') +
  coord_cartesian(ylim = c(0, 65)) +
  geom_text(stat = "count",
            aes(label = paste0("n = ", ..count..), y = ..count..),
            position = 'fill',
            vjust = 2,
            size = 8) +
  geom_text(
    aes(treat, total, label = total, fill = NULL),
    data = totals,
    vjust = -0.5,
    size = 15
  ) +
  theme_tufte() +
  ggtitle(expression('Number of' ~italic(P.fructiphilus)~ '- Griffin, GA, 2018')) +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    axis.text.x = element_text(
      color = "grey20",
      size = 40,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 40,
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
  '../images/actigard_graph_griffin.png',
  plot = last_plot(),
  device = png(),
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

####ATHENS TRIALS####
#selecting Athens trials
df_athns <- filter(df, field == 'athens')

#makes a column with totals for each treatment to display on the graph
totals <-
  df_athns %>% group_by(treat) %>% summarize(total = sum(mites))

#assigning columns as factors
df_athns$treat <- as_factor(df_athns$treat)
df_athns$id <- as_factor(df_athns$id)
df_athns$plant <- as_factor(df_athns$plant)
df_athns$block <- as_factor(df_athns$block)
df_athns$field <- as_factor(df_athns$field)


####ATHENS GRAPHS####
#####graphs of the different tests####
ggplot(data = df_athns,
       mapping = aes(x = treat, y = mites, fill = treat)) +
  geom_bar(position = 'stack', stat = 'identity') +
  coord_cartesian(ylim = c(0, 65)) +
  geom_text(stat = "count",
            aes(label = paste0("n = ", ..count..), y = ..count..),
            position = 'fill',
            vjust = 2,
            size = 8) +
  geom_text(
    aes(treat, total, label = total, fill = NULL),
    data = totals,
    vjust = -0.5,
    size = 15
  ) +
  theme_tufte() +
  ggtitle(expression('Number of' ~italic(P.fructiphilus)~ '- Athens, GA, 2018')) +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    axis.text.x = element_text(
      color = "grey20",
      size = 40,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 40,
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
  '../images/actigard_graph_athens.png',
  plot = last_plot(),
  device = png(),
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls(all.names = TRUE))