####SETUP####
pkgs <-
  c("tidyverse",
    "factoextra",
    "tibble",
    "ggplot2",
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

# removing contaminant
df <- df %>% select(-Styrene)

# making three different tables: one with just spme samples, another with just qsep samples and one with all combined (df)
rrd_spme <-
  df %>%  filter(`Injection Method` == 'rrd_spme') %>% select_if(~ any(. > 0))

# qsep only
rrd_qsep <-
  df %>%  filter(`Injection Method` == 'rrd_qsep') %>% select_if(~ any(. > 0))

# averaging the baseline plants from the data,
# they overwhelm components from rrv plants
avg_clean_rose_qsep <- df %>%
  filter(str_detect(Sample, "rose_clean_greenhouse*"))

avg_clean_rose_qsep <-
  avg_clean_rose_qsep %>% summarise(across(is.numeric, mean))

# there is no injection method, it is an average of the baseline roses
avg_clean_rose_qsep <-
  avg_clean_rose_qsep %>% add_column('Sample' = 'avg_clean_rose_qsep', 'Treatment' = 'untreated')

df <- bind_rows(df, avg_clean_rose_qsep)

df <- df %>%
  filter(!str_detect(Sample, "rose_clean_greenhouse*"))

####PRINCIPAL COMPONENT ANALYSIS####
# prcomp uses singular value decomposition (SVD)
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
  fviz_screeplot(
    graph,
    addlabels = TRUE,
    choice = "variance",
    title = paste(deparse(substitute(graph)))
  )
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
  fviz_pca_biplot(
    graph,
    col.var = "contrib",
    select.var = list(contrib = 5),
    addEllipses = TRUE,
    label = "var",
    repel = TRUE,
    title = paste(deparse(substitute(graph)))
  )
  filename2 <-
    paste("figure/rrv_volatiles_biplot_var_",
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
  
  fviz_pca_biplot(
    graph,
    addEllipses = TRUE,
    label = "ind",
    repel = TRUE,
    select.ind = list(contrib = 10),
    title = paste(deparse(substitute(graph)))
  )
  filename3 <-
    paste("figure/rrv_volatiles_biplot_ind_",
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
  x <- x %>% arrange(desc(PCA1))
  filename <-
    paste("data/rrv_volatiles_correlation_table_",
          deparse(substitute(tab)),
          ".csv",
          sep = "")
  
  write_csv(x, filename)
  
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
  x <- x %>% arrange(desc(PCA1))
  filename2 <-
    paste("data/rrv_volatiles_contribution_table_",
          deparse(substitute(tab)),
          ".csv",
          sep = "")
  write_csv(x, filename2)
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
         aes(PCA1,
             PCA2,
             color = Treatment)) +
    ggtitle(paste(deparse(substitute(graph)))) +
    geom_point(size = 2, alpha = 0.6) +
    theme_bw() +
    stat_ellipse(level = 0.95)
  
  filename <-
    paste("figure/rrv_volatiles_comparison_",
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
         aes(PCA1,
             PCA2,
             color = Treatment)) +
    ggtitle(paste(deparse(substitute(graph)))) +
    geom_point(size = 2, alpha = 0.6) +
    theme_bw() +
    stat_ellipse(level = 0.95) +
    geom_text(aes(label = Sample), hjust = -0.05, vjust = 0)
  
  filename2 <-
    paste("figure/rrv_volatiles_comparison_labeled_",
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
}

#savin' graphs
save_pca_comparisons(pca_comp_df)
save_pca_comparisons(pca_comp_spme)
save_pca_comparisons(pca_comp_qsep)

#cleanup
rm(list = ls())
