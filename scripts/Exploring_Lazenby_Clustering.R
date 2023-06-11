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
vertex_names = function (g) {
    vert = V(g)
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


## Network statistics for significantly different words 
Lazenby_Nodewise_stats <- CoxHae_childNodeWise_stats %>%
    left_join(., select(cdi_metadata, word, num_item_id)) %>%
    filter(num_item_id %in% words_within_prediction$num_item_id ) %>%
    arrange(word)


## Obtaining counts per bin for each word to plot with clustering coefficient 
participants <- ASD_NA_metadataNetstats_2023_RAN %>%
    select(group, subjectkey, vocab_size = nProduced, interview_age, sex) %>%
    unique() %>%
    mutate(vocab_bin = cut(vocab_size, c(20, seq(50, 600, by = 50)), include.lowest = TRUE))

items <- cdi_metadata %>%
    select(num_item_id, category, lexical_class, word) %>%
    arrange(num_item_id) %>%
    distinct()

d <- ASD_NA_metadataNetstats_2023_RAN %>%
    filter(Produces) %>%
    drop_na() %>%
    select(group, subjectkey, interview_age, vocab_size = nProduced, num_item_id) %>%
    distinct() %>%
    mutate(vocab_bin = cut(vocab_size, c(20, seq(50, 600, by = 50)), include.lowest = TRUE))


bin_size <- participants %>%
    group_by(vocab_bin, group) %>%
    tally( name = "bin_size") %>%
    pivot_wider(
        id_cols = "vocab_bin",
        names_from = "group",
        names_prefix = "bin_size_",
        values_from = "bin_size"
    )


x <- d %>%
    group_by(vocab_bin, group, num_item_id) %>%
    tally() %>%
    pivot_wider(
        id_cols = c(vocab_bin, num_item_id),
        names_from = group,
        values_from = n,
        names_prefix = "n_",
        values_fill = 0
    ) %>%
    pivot_longer(.,
        cols = c(n_ASD,n_NA),
        names_to = c("n","group"),
        names_sep = "_",
        values_to = "n",
        names_repair = "unique") %>%
    left_join(bin_size) %>%
    ungroup() %>%
    filter(num_item_id %in% Lazenby_Nodewise_stats$num_item_id) %>%
    left_join(., Lazenby_Nodewise_stats)
summary(x)

ggplot(x, aes(x = vocab_bin, y = ))


y <- d %>%
    group_by(vocab_bin, group, num_item_id) %>%
    tally() %>%
    pivot_wider(
        id_cols = c(vocab_bin, num_item_id),
        names_from = group,
        values_from = n,
        names_prefix = "n_",
        values_fill = 0
    ) %>%
    filter(num_item_id %in% Lazenby_Nodewise_stats$num_item_id) %>%
    left_join(., Lazenby_Nodewise_stats)%>%
    pivot_wider(c(vocab_bin, num_item_id),
    names_from = stat,
    values_from = value) %>%
    left_join(.,)




logit_greaterOne <- q %>%
    filter(abs(diff_logit) > 1 &
    abs(non_asd_native) >= 20 &
    abs(non_asd_native) <= 600 &
    abs(asd_native) >= 20 &
    abs(asd_native) <= 600) 


sig_binBinom_words_log <- sig_binBinom_words %>%
    left_join(., logit_greaterOne) %>%
    arrange(desc(abs(diff_logit))) %>%
    left_join(.,select(cdi_metadata, num_item_id, word))

sig_binBinom_words_ASD_more <- sig_binBinom_words_log %>%
    filter(diff_logit > 0) %>%
    arrange(desc(abs(diff_native))) 

sig_binBinom_words_ASD_less <- sig_binBinom_words_log %>%
    filter(diff_logit < 0) %>%
    arrange(desc(abs(diff_native))) 
