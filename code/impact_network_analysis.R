####IMPACT THROUGH NETWORKS####
# a list of packages used
pkgs <- c('dplyr', 'igraph')

# checks to see if you have the packages installed,
# if you don't, it will try to install them
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading packages from pkgs
lapply(pkgs, library, character.only = TRUE)


mat1 <-
  matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3, ncol = 3) # matrix function

mat2 <- graph_from_adjacency_matrix(mat1)

plot(mat2, edge.arrow.size = 1)

mat3 <-
  graph(edge = c(1, 3, 3, 2, 2, 1),
        n = 3,
        directed = TRUE) # use graph function and list edges
plot(
  mat3,
  edge.arrow.size = 1,
  vertex.color = 'purple',
  vertex.size = 20
)

mtgraph <- make_empty_graph(50)
plot(
  mtgraph,
  vertex.size = 8,
  vertex.label = NA,
  vertex.color = "plum"
)

fulgraph <- make_full_graph(50)
plot(
  fulgraph,
  vertex.size = 8,
  vertex.label = NA,
  vertex.color = "plum"
)

tregraph <- make_tree(50)
plot(
  tregraph,
  vertex.size = 8,
  vertex.label = NA,
  vertex.color = "plum"
)

erdograph <- sample_gnm(n = 100, m = 40)
plot(
  erdograph,
  vertex.size = 8,
  vertex.label = NA,
  vertex.color = "plum"
)
