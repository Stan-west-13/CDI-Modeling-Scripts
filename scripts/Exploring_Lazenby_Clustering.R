load("data/CoxHae_child_network.RData")
load("data/significant_binBinom_words.Rdata")
load("data/logit_diff_all.Rdata")
load("data/cdi-metadata.Rdata")
library(dplyr)
library(igraph)
library(purrr)
library(tidyr)
source("R/netStats.R")

vertex_names = function (g) {
    vert = V(g)
    if (! is.null(names(vert))) names(vert) else as.vector(vert)
}

netStats_Laz <- function(G) {
  netStats <- function(N) {
    y <- c("outdegree" = outdegree_igraph(N),
           "mean_distance" = mean(distances((N), mode = "in")),
           "median_indegree" = indegree_igraph(N))

    return(y)
  }
  return(netStats(G))
}


joined_logit_binned <- q %>%
    filter(num_item_id %in% sig_binBinom_words$num_item_id) %>%
    left_join(.,sig_binBinom_words) %>%
    left_join(.,select(cdi_metadata, num_item_id, word))

words_outside_prediction <- joined_logit_binned %>%
    filter(abs(diff_logit) > 1 &
    non_asd_native < 20 |
    non_asd_native > 600 |
    asd_native < 20 |
    asd_native > 600)

words_within_prediction <- joined_logit_binned %>%
    filter(abs(diff_logit) > 1 &
    non_asd_native >= 20 &
    non_asd_native <= 600 &
    asd_native >= 20 &
    asd_native <= 600)


child_net_graph <- CoxHae_child_network %>%
    graph_from_adjacency_matrix()

laz_stats <- child_net_graph %>%
    netStats_Laz(.) %>%
    as.data.frame() %>%
    data.frame(stat = rownames(.), value = .$.) %>%
    select(-.) %>%
    data.frame(do.call('rbind', strsplit(as.character(.$stat),'.',fixed=TRUE))) %>%
    select(-stat) %>%
    rename("stat" = "X1", "word" = "X2")
rownames(laz_stats) <- 1:nrow(laz_stats)


clust_laz <- data.frame(word = vertex_names(child_net_graph), 
                        stat = "clustering_coefficient", 
                        value = transitivity(child_net_graph, type = "local"))

CoxHae_childNodeWise_stats <- rbind(laz_stats, clust_laz)

Lazenby_Nodewise_stats <- CoxHae_childNodeWise_stats %>%
    left_join(., select(cdi_metadata, word, num_item_id)) %>%
    filter(num_item_id %in% words_within_prediction$num_item_id ) %>%
    arrange(word)


