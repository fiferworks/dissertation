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
  read_excel("data/rrv_volatiles_pca_table.xlsx", col_types = "guess")

# making df with SPME chems only
rrd_spme <-
  df %>%  select_if(~ any(. > 0)) %>% filter(`Injection Method` == 'rrd_spme')

df <- df %>% select(colnames(rrd_spme))

# qsep only
rrd_qsep <-
  df %>%  filter(`Injection Method` == 'rrd_qsep') %>% select_if(~ any(. > 0))

####UNIFORM MANIFOLD APPROXIMATION AND PROJECTION####
volatiles_umap <-
  select(rrd_spme,-Sample,-Treatment,-`Injection Method`) %>%
  as.matrix() %>%
  umap(
    n_neighbors = 7,
    min_dist = 0.5,
    metric = "manhattan",
    n_epochs = 4000,
    verbose = TRUE
  )

tib_vols_umap <- rrd_spme %>%
  mutate_if(.funs = scale,
            .predicate = is.numeric,
            scale = FALSE) %>%
  mutate(UMAP1 = volatiles_umap$layout[, 1], UMAP2 = volatiles_umap$layout[, 2]) %>%
  pivot_longer(
    c(-UMAP1,-UMAP2,-Sample,-Treatment,-`Injection Method`),
    names_to = 'Variable',
    values_to = 'Value'
  )
# gather(key = 'Variable', value = 'Value', c(-UMAP1, -UMAP2, -Sample, -Treatment))

# ggpairs(as.data.frame(volatiles_umap$layout), mapping = aes(col = df$Treatment))

####2D UMAP PLOTS####
ggplot(tib_vols_umap, aes(UMAP1, UMAP2, col = Value, shape = Treatment)) +
  facet_wrap( ~ Variable) +
  geom_point(size = 3) +
  scale_color_viridis_b(begin = 1,
                        end = 0,
                        option = "D") +
  theme_bw()

#saving the file
ggsave(
  'figure/rrv_volatiles_umap_graph_2d.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

####3D UMAP PLOTS####
volatiles_umap_3d <- select(df, -Sample, -Treatment) %>%
  as.matrix() %>%
  umap(
    n_neighbors = 7,
    min_dist = 0.1,
    n_components = 3,
    metric = "manhattan",
    n_epochs = 2000,
    verbose = TRUE
  )

ggpairs(as.data.frame(volatiles_umap_3d$layout),
        mapping = aes(col = df$Treatment))

#saving the file
ggsave(
  'figure/rrv_volatiles_umap_graph_3d.png',
  plot = last_plot(),
  type = 'cairo',
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

#cleanup
rm(list = ls())