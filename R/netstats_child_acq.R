netStats_child_acq <- function(G) {
  netStats <- function(N) {
    y <- c("indegree" = mean(indegree_igraph(N)),
           "outdegree" = mean(outdegree_igraph(N)),
           "clustering_coefficient" = transitivity(N, type = "global"),
           "mean_distance" = mean_distance(N),
           "median_indegree" = median(indegree_igraph(N)))

    return(y)
  }
  return(netStats(G))
}
