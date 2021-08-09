####SETUP####
pkgs <-
  c("tidyverse",
    "umap",
    "Rcpp",
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


####UNIFORM MANIFOLD APPROXIMATION AND PROJECTION####

spme_umap <- select(df,-Sample,-Treatment) %>%
  as.matrix() %>%
  umap(
    n_neighbors = 7,
    min_dist = 0.1,
    metric = "manhattan",
    n_epochs = 200,
    verbose = TRUE
  )

# #saving the file
# ggsave(
#   'figure/spme_screeplot_graph.png',
#   plot = last_plot(),
#   type = 'cairo',
#   width = 16,
#   height = 9,
#   scale = 1,
#   dpi = 300
# )
# 
# # plotting the PCAs against one another
# spme_pca <- df %>%
#   mutate(PCA1 = pca$x[, 1], PCA2 = pca$x[, 2])
# 
# ggplot(spme_pca,
#        aes(
#          PCA1,
#          PCA2,
#          color = Treatment,
#          shape = Treatment,
#          label = Treatment
#        )) +
#   geom_point(size = 2, alpha = 0.6) +
#   theme_bw() +
#   stat_ellipse(level = 0.95)
# 
# #saving the file
# ggsave(
#   'figure/spme_pca_graph.png',
#   plot = last_plot(),
#   type = 'cairo',
#   width = 16,
#   height = 9,
#   scale = 1,
#   dpi = 300
# )
# 
# 
# #cleanup
# rm(list = ls())