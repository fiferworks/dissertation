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

# invasion network modelling
dt1 <- c(1, 0, 0, 0, 0, 1, 1, 1, 1, 0)
Amat <- matrix(rbinom(n = 100, s = 1, p = 0.7), ncol = 10)
diag(Amat) <- 1
temp <- dt1 %*% Amat
dt2 <- as.numeric((temp > 0))
cbind(dt1, dt2)

temp <- runif(20)
own <- temp <= 0.61
neighbor <- temp > 0.61 & temp <= 0.77
market <- temp > 0.77 & temp <= 0.96
dealer <- temp > 0.96

Amat <- matrix(0, ncol = 30, nrow = 30)
diag(Amat[1:20, 1:20])[own] <- 1

for (j in 1:20) {
  if (neighbor[j])Amat[sample(1:20[-j], 1), j] <- 1
  else if (market[j]) Amat[sample(21:25, 1), j] <- 1
  else if (dealer[j]) Amat[sample(26:30, 1), j] <- 1
}

Amat[1:4, 21] <- 1
Amat[5:8, 22] <- 1
Amat[9:12, 23] <- 1
Amat[13:16, 24] <- 1
Amat[17:20, 25] <- 1

MA <- graph.adjacency(Amat)
V(MA)$color <- c(rep('red', 20), rep('blue', 5), rep('green', 5))
plot(MA, edge.arrow.size = 0.3)
layout.keep <- layout.kamada.kawai(MA)
plot(
  MA,
  edge.arrow.size = 0.3,
  layout = layout.keep,
  vertex.label.cex = 0.5
)
