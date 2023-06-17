load("data/CoxHae_child_network.RData")
load("data/significant_binBinom_words.Rdata")
load("data/logit_diff_all.Rdata")
load("data/cdi-metadata.Rdata")
load("data/ASD_NA_metadataNetstats_2023_RAN.Rdata")
source("R/netStats.R")
library(dplyr)
library(igraph)
library(purrr)
library(tidyr)
library(ggplot2)

## Function defining vertex names for obtaining network statistics 
vertex_names <- function (g) {
    vert <- V(g)
    if (! is.null(names(vert))) names(vert) else as.vector(vert)
}

## Function defining local network statistics
netStats_Laz <- function(G) {
  netStats <- function(N) {
    y <- c("outdegree" = outdegree_igraph(N),
           "mean_distance" = mean(distances((N), mode = "in")),
           "median_indegree" = indegree_igraph(N))

    return(y)
  }
  return(netStats(G))
}

## Joining Lazenby results with Binned Binomial results
joined_logit_binned <- q %>%
    filter(num_item_id %in% sig_binBinom_words$num_item_id) %>%
    left_join(.,sig_binBinom_words) %>%
    left_join(.,select(cdi_metadata, num_item_id, word))

words_within_prediction <- joined_logit_binned %>%
    filter(abs(diff_logit) > 1 &
    non_asd_native >= 20 &
    non_asd_native <= 600 &
    asd_native >= 20 &
    asd_native <= 600)

## Graph for the child-oriented word association matrix
child_net_graph <- CoxHae_child_network %>%
    graph_from_adjacency_matrix()

## Computing network statistics and formatting dataframe
laz_stats <- child_net_graph %>%
    netStats_Laz(.) %>%
    as.data.frame() %>%
    data.frame(stat = rownames(.), value = .$.) %>%
    select(-.) %>%
    data.frame(do.call('rbind', strsplit(as.character(.$stat),'.',fixed=TRUE))) %>%
    select(-stat) %>%
    rename("stat" = "X1", "word" = "X2")
rownames(laz_stats) <- 1:nrow(laz_stats)

## Computing clustering coefficient. This was not retaining vertex names in the function above.
clust_laz <- data.frame(word = vertex_names(child_net_graph), 
                        stat = "clustering_coefficient", 
                        value = transitivity(child_net_graph, type = "local"))

CoxHae_childNodeWise_stats <- rbind(laz_stats, clust_laz)

CoxHae_childNodeWise_stats[CoxHae_childNodeWise_stats$word == "soda",]$word <- "soda/pop"
CoxHae_childNodeWise_stats[CoxHae_childNodeWise_stats$word == "grandma",]$word <- "grandma*" 
CoxHae_childNodeWise_stats[CoxHae_childNodeWise_stats$word == "turn",]$word <- "turn around" 
## Network statistics for significantly different words 
Lazenby_Nodewise_stats <- CoxHae_childNodeWise_stats %>%
    left_join(., select(cdi_metadata, word, num_item_id)) %>%
    filter(num_item_id %in% words_within_prediction$num_item_id ) %>%
    arrange(word) %>%
    pivot_wider(names_from = stat,
                values_from = value) %>%
    left_join(., select(words_within_prediction, -word), by = "num_item_id") %>%
    mutate(ASD_more = ifelse(asd_native > non_asd_native, TRUE, FALSE))

t.test(clustering_coefficient~ASD_more, data = Lazenby_Nodewise_stats %>% filter(asd_native >= 300))

t.test(clustering_coefficient~ASD_more, data = Lazenby_Nodewise_stats)
t.test(clustering_coefficient~ASD_more, data = Lazenby_Nodewise_stats %>% filter(asd_native <= 200))


ggplot(aes(x = non_asd_native, y = clustering_coefficient), data = Lazenby_Nodewise_stats)+
    geom_point(aes(color = ASD_more))+
    labs(x = "Vocabulary Size", y = "Clustering Coefficient", color = "ASD more likely to produce")
    
x <- lm(clustering_coefficient ~ asd_native * ASD_more, data = Lazenby_Nodewise_stats)
summary(x)
plot(x)



### "Cooks distance" analysis
vix <- V(child_net_graph)

x <- map_dfr(vix, function(v, g) {
    transitivity(delete_vertices(g, v), type = "global")
}, g = child_net_graph, .id = .id) %>%
    t() %>%
    as.data.frame() %>%
    mutate(word = rownames(.), diff = transitivity(child_net_graph, type = "global") - V1)
