library(dplyr)
library(igraph)
library(assertthat)
library(parallel)
source("R/netStats.R")
load("data/POS_numbers_ASD.Rdata")
load("data/POS_numbers_TD.Rdata")
load("data/CoxHae_child_network.RData")
load("data/vertices_POS_child.Rdata")
graph_CoxHae_toddler <- graph_from_adjacency_matrix(CoxHae_child_network)

# Define the cluster, using all by two cores on the system. This should give
# you 30 "workers". Before you do this, start "top" in another terminal window
# to see what happens.
cl <- makeCluster(detectCores()-2)

# Setting the Random Number Generator Stream for each worker will make sure
# that random processes behave differently on each.
clusterSetRNGStream(cl)

# Define functions for doing the random acquisition simulation
multiSample <- function(n, x, fct, simplify2vec = FALSE) {
  y <- mapply(sample, split(x, fct), n, SIMPLIFY = FALSE)
  return(if(simplify2vec) unname(do.call(c, y)) else y)
}

balanced_RAN_network <- function(vocab_size, G, POS) {
  env_size <- igraph::vcount(G)
  assertthat::are_equal(env_size, length(POS))
  assertthat::are_equal(nlevels(POS), length(vocab_size))
  ix <- multiSample(vocab_size, seq_len(env_size), POS, simplify2vec = TRUE)
  return(igraph::induced_subgraph(G, ix))
}

network_stats <- function(g) {
    return(c(indegree_median=median(indegree_igraph(g)), 
             indegree_mean=mean(indegree_igraph(g)),
             clustcoef=igraph::transitivity(g, type = "global"),
             meandist=igraph::mean_distance(g)))
}

# If we run this function, each random graph will be discarded after computing
# statistics. This eliminates the memory consumption issue.
balanced_RAN_stats <- function(vocab_size, G, POS) {
    return(network_stats(balanced_RAN_network(vocab_size, G, POS)))
}

# Export everything needed to run the operation, including any data or
# functions you have defined. If a function you have written relies on a
# package, make sure to use the package::function referencing.
invisible(clusterExport(cl, c("POS_numbers_ASD", "POS_numbers_TD", "graph_CoxHae_toddler", "vertices_POS_child",
                              "balanced_RAN_network", "network_stats", "balanced_RAN_stats", "multiSample", "indegree_igraph")))

# Finally, run with parLapply. Running 1000 replications takes less than two minutes.
starttime <- proc.time()
stats_ASD_RAN <- parLapply(cl, POS_numbers_ASD,
                        function(n,G,POS) replicate(1000, balanced_RAN_stats(n,G,POS)),
                        G = graph_CoxHae_toddler, POS = as.factor(vertices_POS_child$POS))

save(stats_ASD_RAN, file = "data/Child_Stats_ASD_balRAN_1000.Rdata")

stats_TD_RAN <- parLapply(cl, POS_numbers_TD,
                           function(n,G,POS) replicate(1000, balanced_RAN_stats(n,G,POS)),
                           G = graph_CoxHae_toddler, POS = as.factor(vertices_POS_child$POS))
save(stats_TD_RAN, file = "data/Child_Stats_TD_balRAN_1000.Rdata")

stoptime <- proc.time()
print(stoptime - starttime)

# All done, so release the workers
stopCluster(cl)
