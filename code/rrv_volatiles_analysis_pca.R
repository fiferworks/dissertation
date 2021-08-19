####SETUP####
pkgs <-
  c("tidyverse",
    "factoextra",
    "tibble",
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
  read_excel("data/rrv_volatiles_pca_table.xlsx", col_types = "guess")

# picking out the chemistry we want to compare
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


# making df with SPME chems only
rrd_spme <-
  df %>%  select_if( ~ any(. > 0)) %>% filter(`Injection Method` == 'rrd_spme')

df <- df %>% select(colnames(rrd_spme))

# qsep only
rrd_qsep <-
  df %>%  filter(`Injection Method` == 'rrd_qsep') %>% select_if( ~ any(. > 0))

####PRINCIPAL COMPONENT ANALYSIS####

# quick helper function to run for each dataframe
run_pca <- function(data) {
  data %>% select(-Sample, -Treatment,-`Injection Method`) %>%
    prcomp(center = TRUE, scale = FALSE)
}

pca_df <- run_pca(df)
pca_spme <- run_pca(rrd_spme)
pca_qsep <- run_pca(rrd_qsep)

####PLOTTING SCREEPLOTS AND PCA####
# fviz_screeplot(pca_df, addlabels = TRUE, choice = "eigenvalue")
plot_n_save <- function(graph) {
  fviz_screeplot(graph, addlabels = TRUE, choice = "variance")
  filename <-
    paste("figure/rrv_volatiles_screeplot_",
          deparse(substitute(graph)),
          ".png",
          sep = "")
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
  fviz_pca_biplot(graph, label = "var")
  filename2 <-
    paste("figure/rrv_volatiles_biplot_var",
          deparse(substitute(graph)),
          ".png",
          sep = "")
  ggsave(
    file = filename2,
    plot = last_plot(),
    device = png,
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )
  
  fviz_pca_biplot(graph, label = "ind")
  filename3 <-
    paste("figure/rrv_volatiles_biplot_ind",
          deparse(substitute(graph)),
          ".png",
          sep = "")
  ggsave(
    file = filename3,
    plot = last_plot(),
    device = png,
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )
}

# plotting and savin'
plot_n_save(pca_df)
plot_n_save(pca_spme)
plot_n_save(pca_qsep)

####COMPONENT TABLES####
pca_dat <- get_pca(pca_df)
pca_dat_spme <- get_pca(pca_spme)
pca_dat_qsep <- get_pca(pca_qsep)

# summary(pca_df)
#
# # can be interpreted as Pearson correlation coefficients
# map_dfc(1:27, ~pca_df$rotation[,.]*sqrt(pca_df$sdev^2)[.])
# # the following code does the same things with the factoextra package

save_table <- function(tab) {
  x <- data.frame(tab$cor, check.names = FALSE)
  x <- tibble::rownames_to_column(x)
  x <- as_tibble(x)
  x <- x %>% select(rowname, Dim.1, Dim.2, Dim.3) %>%
    rename(
      Chemical = rowname,
      PCA1 = Dim.1,
      PCA2 = Dim.2,
      PCA3 = Dim.3
    )
  filename <-
    paste(
      "data/rrv_volatiles_pca_correlation_table_",
      deparse(substitute(tab)),
      ".xlsx",
      sep = ""
    )
  write_xlsx(x, path = filename)
  
  x <- data.frame(tab$contrib, check.names = FALSE)
  x <- tibble::rownames_to_column(x)
  x <- as_tibble(x)
  x <- x %>% select(rowname, Dim.1, Dim.2, Dim.3) %>%
    rename(
      Chemical = rowname,
      PCA1 = Dim.1,
      PCA2 = Dim.2,
      PCA3 = Dim.3
    )
  filename2 <-
    paste(
      "data/rrv_volatiles_pca_contribution_table_",
      deparse(substitute(tab)),
      ".xlsx",
      sep = ""
    )
  write_xlsx(x, path = filename2)
}

save_table(pca_dat)
save_table(pca_dat_spme)
save_table(pca_dat_qsep)

####PLOTTING PCAS AGAINST ONE ANOTHER####
pca_df <- run_pca(df)
pca_spme <- run_pca(rrd_spme)
pca_qsep <- run_pca(rrd_qsep)

# plotting the PCAs against one another
pca_comp_df <- df %>%
  mutate(PCA1 = pca_df$x[, 1], PCA2 = pca_df$x[, 2])

pca_comp_spme <- rrd_spme %>%
  mutate(PCA1 = pca_spme$x[, 1], PCA2 = pca_spme$x[, 2])

pca_comp_qsep <- rrd_qsep %>%
  mutate(PCA1 = pca_qsep$x[, 1], PCA2 = pca_qsep$x[, 2])

# function fer savin'
save_pca_comparisons <- function (graph) {
  ggplot(graph,
         aes(
           PCA1,
           PCA2,
           color = Treatment,
           shape = Treatment,
           label = Treatment
         )) +
    geom_point(size = 2, alpha = 0.6) +
    theme_bw() +
    stat_ellipse(level = 0.95)
  
  filename <-
    paste("figure/rrv_volatiles_pca_comparison_",
          deparse(substitute(graph)),
          ".png",
          sep = "")
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
  
  ggplot(graph,
         aes(
           PCA1,
           PCA2,
           color = Treatment,
           shape = Treatment,
           label = Treatment
         )) +
    geom_point(size = 2, alpha = 0.6) +
    theme_bw() +
    stat_ellipse(level = 0.95) +
    geom_text(aes(label = Sample), hjust = -0.05, vjust = 0)
  
  filename2 <-
    paste(
      "figure/rrv_volatiles_pca_comparison_labeled_",
      deparse(substitute(graph)),
      ".png",
      sep = ""
    )
  ggsave(
    file = filename2,
    plot = last_plot(),
    device = png,
    type = 'cairo',
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )
}

#savin'
save_pca_comparisons(pca_comp_df)
save_pca_comparisons(pca_comp_spme)
save_pca_comparisons(pca_comp_qsep)


#cleanup
rm(list = ls())
