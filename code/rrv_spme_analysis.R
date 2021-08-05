####SETUP####
pkgs <-
  c("tidyverse",
    "factoextra",
    "ggplot2",
    "Cairo",
    "readxl",
    "writexl")

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# reading in master datasheet
df <-
  read_excel("data/ofv_pca_table.xlsx", col_types = "guess")


####PRINCIPAL COMPONENT ANALYSIS####
pca <- df %>% select(-Sample,-Treatment) %>%
  prcomp(center = TRUE, scale = FALSE)

# summary(pca)
#
# # can be interpreted as Pearson correlation coefficients
# map_dfc(1:27, ~pca$rotation[,.]*sqrt(pca$sdev^2)[.])
# # the following code does the same things with the factoextra package
# pca_dat$coord

pca_dat <- get_pca(pca)

####PLOTTING THE PCA VALUES####
fviz_pca_biplot(pca, label = 'var')

#saving the file
ggsave(
  'figure/spme_biplot_graph.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

# fviz_pca_var(pca)
# fviz_screeplot(pca, addlabels = TRUE, choice = "eigenvalue")

fviz_screeplot(pca, addlabels = TRUE, choice = "variance")

#saving the file
ggsave(
  'figure/spme_screeplot_graph.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)


spme_pca <- df %>%
  mutate(PCA1 = pca$x[, 1], PCA2 = pca$x[, 2])

ggplot(spme_pca, aes(PCA1, PCA2, col = Treatment)) +
  geom_point() +
  theme_bw() +
  stat_ellipse(level = 0.95)

#saving the file
ggsave(
  'figure/spme_pca_graph.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls())