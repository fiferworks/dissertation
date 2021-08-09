####SETUP####
pkgs <-
  c("tidyverse",
    "umap",
    "Rcpp",
    "ggplot2",
    "GGally",
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
spme_umap <- select(df, -Sample, -Treatment) %>%
  as.matrix() %>%
  umap(
    n_neighbors = 7,
    min_dist = 0.1,
    metric = "manhattan",
    n_epochs = 2000,
    verbose = TRUE
  )

tib_spme_umap <- df %>%
  mutate_if(.funs = scale,
            .predicate = is.numeric,
            scale = FALSE) %>%
  mutate(UMAP1 = spme_umap$layout[, 1], UMAP2 = spme_umap$layout[, 2]) %>%
  pivot_longer(c(-UMAP1, -UMAP2, -Sample, -Treatment),
               names_to = 'Variable',
               values_to = 'Value')
# gather(key = 'Variable', value = 'Value', c(-UMAP1, -UMAP2, -Sample, -Treatment))

# ggpairs(as.data.frame(spme_umap$layout), mapping = aes(col = df$Treatment))

####2D UMAP PLOTS####
ggplot(tib_spme_umap, aes(UMAP1, UMAP2, col = Value, shape = Treatment)) +
  facet_wrap(~ Variable) +
  geom_point(size = 3) +
  scale_color_viridis_b(begin = 1,
                        end = 0,
                        option = "D") +
  theme_bw()

#saving the file
ggsave(
  'figure/spme_umap_graph_2d.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

# ####3D UMAP PLOTS####
# spme_umap_3d <- select(df, -Sample, -Treatment) %>%
#   as.matrix() %>%
#   umap(
#     n_neighbors = 7,
#     min_dist = 0.1,
#     n_components = 3,
#     metric = "manhattan",
#     n_epochs = 2000,
#     verbose = TRUE
#   )
#
# ggpairs(as.data.frame(spme_umap_3d$layout), mapping = aes(col = df$Treatment))
#
# #saving the file
# ggsave(
#   'figure/spme_umap_graph_3d.png',
#   plot = last_plot(),
#   type = 'cairo',
#   width = 16,
#   height = 9,
#   scale = 1,
#   dpi = 300
# )


#cleanup
rm(list = ls())