####SETUP####
pkgs <-
  c("tidyverse",
    "umap",
    "Rcpp",
    "ggplot2",
    "GGally",
    "Cairo")

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# reading in master datasheet
df <- read_csv("data/rrv_volatiles_pca_table.csv")

# making three different tables: one with just spme samples, another with just qsep samples and one with all combined (df)
rrd_spme <-
  df %>%  filter(`Injection Method` == 'rrd_spme') %>% select_if( ~ any(. > 0))

# qsep only
rrd_qsep <-
  df %>%  filter(`Injection Method` == 'rrd_qsep') %>% select_if( ~ any(. > 0))

df <- df %>% select(
  Sample,
  Treatment,
  `Injection Method`,
  `Pinene <1R-alpha->`,
  `Pinene <alpha->`,
  `Pinene <beta->`,
  `Carene, <3->`,
  `Carene <delta-3>`,
  `Phellandrene <beta->`,
  `p-Cymene`,
  `D-Limonene`,
  `Limonene oxide, trans-`,
  `Copaene <alpha->`,
  `Copaene <beta->`,
  `Bourbonene <beta->`,
  `Bergamotene <alpha-, cis->`,
  Caryophyllene,
  `Caryophyllene oxide`,
  `Caryophyllene <9-epi-(E)->`,
  `Murrolene <alpha->`,
  `Murrolene <gamma->`,
  `Muurrolene <gamma->`,
  `Farnesene <(E,E)-, alpha->`,
  `Farnesene <(E)-, beta->`
)

df <- df %>%
  filter(!str_detect(Sample, "rose_clean_greenhouse*"))

# setting up parameters for UMAP
custom.config = umap.defaults
custom.config$random_state = 123
custom.config$n_neighbors = 7
custom.config$min_dist = 0.15
custom.config$n_components = 2
custom.config$metric = "euclidean"
custom.config$n_epochs = 200
custom.config$verbose = TRUE

####UNIFORM MANIFOLD APPROXIMATION AND PROJECTION####
make_main_umap <- function(y) {
  filename <-
    paste("figure/rrv_volatiles_umap_",
          deparse(substitute(y)),
          ".png",
          sep = "")
  umapd <-
    y %>% select(-Sample,-Treatment,-`Injection Method`) %>%
    as.matrix() %>%
    umap(config = custom.config)
  
  tib_vols_4_umap <- y %>%
    mutate_if(.funs = scale,
              .predicate = is.numeric,
              scale = FALSE) %>%
    mutate(UMAP1 = umapd$layout[, 1], UMAP2 = umapd$layout[, 2]) %>%
    pivot_longer(
      c(-UMAP1, -UMAP2, -Sample, -Treatment, -`Injection Method`),
      names_to = 'Variable',
      values_to = 'Value'
    )
  
  ggplot(tib_vols_4_umap,
         aes(UMAP1, UMAP2, col = Treatment, shape = Treatment)) +
    ggtitle(paste(deparse(substitute(y)))) +
    geom_point(size = 3) +
    scale_color_viridis_d(begin = 0,
                          end = 1,
                          option = "D") +
    theme_bw()
  ggsave(
    file = filename,
    plot = last_plot(),
    device = png,
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )
}

make_main_umap(df)
make_main_umap(rrd_spme)
make_main_umap(rrd_qsep)

# in the future, refactor the function to make this work:
# list_of_tables <- list(df, rrd_spme, rrd_qsep)
# walk(list_of_tables, make_main_umap)


####UMAP functions makes facet wrapped graphs for the different chems####
umap_n_save_graph <- function(x) {
  filename <-
    paste("figure/rrv_volatiles_umap_chems_",
          deparse(substitute(x)),
          ".png",
          sep = "")
  umapped <-
    x %>% select(-Sample,-Treatment,-`Injection Method`) %>%
    as.matrix() %>%
    umap(config = custom.config)
  
  tib_vols_umap <- x %>%
    mutate_if(.funs = scale,
              .predicate = is.numeric,
              scale = FALSE) %>%
    mutate(UMAP1 = umapped$layout[, 1], UMAP2 = umapped$layout[, 2]) %>%
    pivot_longer(
      c(-UMAP1, -UMAP2, -Sample, -Treatment, -`Injection Method`),
      names_to = 'Variable',
      values_to = 'Value'
    )
  
  ggplot(tib_vols_umap, aes(UMAP1, UMAP2, col = Value, shape = Treatment)) +
    ggtitle(paste(deparse(substitute(x)))) +
    facet_wrap(~ Variable) +
    geom_point(size = 3) +
    scale_color_viridis_b(begin = 1,
                          end = 0,
                          option = "D") +
    theme_bw()
  ggsave(
    file = filename,
    plot = last_plot(),
    device = png,
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )
}

umap_n_save_graph(df)
umap_n_save_graph(rrd_spme)
umap_n_save_graph(rrd_qsep)

# ####3D UMAP PLOTS####
# volatiles_umap_3d <- select(df, -Sample, -Treatment) %>%
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
# ggpairs(as.data.frame(volatiles_umap_3d$layout),
#         mapping = aes(col = df$Treatment))
#
# #saving the file
# ggsave(
#   'figure/rrv_volatiles_umap_graph_3d.png',
#   plot = last_plot(),
#   type = 'cairo',
#   width = 16,
#   height = 9,
#   scale = 1,
#   dpi = 300
# )

#cleanup
rm(list = ls())