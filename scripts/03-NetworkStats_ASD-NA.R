load("data/ASD_NA_metadata_2023_match.Rdata")
load("data/CoxHae_child_network.RData")
load("data/cdi-metadata.Rdata")
source("R/netstats_child_acq.R")
source("R/assocNetwork.R")
source("R/netStats.R")
source("R/Random_Networks.R")
library(purrr)
library(igraph)
library(dplyr)
row_SD <- function(d){
  return(apply(d, 1, sd, na.rm = TRUE))
}

z_score <- function(true, RAN_mean, RAN_sd){
  return((true - RAN_mean)/RAN_sd)
}

ASD_NA_split_netstats <- ASD_NA_metadata_2023_match %>%
  filter(Produces == TRUE) %>%
  mutate(subjectkey = as.character(subjectkey))%>%
  select(subjectkey, word) %>%
  split(x = .$word, f = .$subjectkey) %>%
  lapply(., child_acquisition_network, network_of_words = CoxHae_child_network) %>%
  lapply(., graph_from_adjacency_matrix) %>%
  imap_dfr(~netStats_child_acq(.), .id = "subjectkey")

ASD_NA_metadataNetstats_2023 <- left_join(ASD_NA_metadataNetstats_2023_match, ASD_NA_split_netstats, by = "subjectkey")



##Prep for RAN analysis##############################3
### POS
ASD_NA_metadataNetstats_2023 <- ASD_NA_metadataNetstats_2023 %>%
  mutate(POS = ifelse(lexical_class == "nouns"|lexical_class == "verbs", lexical_class, "other"))
save(ASD_NA_metadataNetstats_2023, file = "data/ASD_NA_metadataNetstats_2023.Rdata")

POS_numbers_ASD <- ASD_NA_metadataNetstats_2023 %>%
  filter(group == "ASD", Produces == TRUE) %>%
  droplevels()

POS_numbers_ASD <- tapply(as.factor(POS_numbers_ASD$POS), POS_numbers_ASD$subjectkey, table)

POS_numbers_TD <- ASD_NA_metadataNetstats_2023 %>%
  filter(group == "NA", Produces == TRUE) %>%
  droplevels()

POS_numbers_TD <- tapply(as.factor(POS_numbers_TD$POS), POS_numbers_TD$subjectkey, table)

save(POS_numbers_ASD, file = "data/POS_numbers_ASD.Rdata")
save(POS_numbers_TD, file = "data/POS_numbers_TD.Rdata")


## Vertices
cdi_metadata <- cdi_metadata %>%
  mutate(POS = ifelse(lexical_class == "nouns"  | lexical_class == "verbs", lexical_class, "other"))
graph_Child <- graph_from_adjacency_matrix(CoxHae_child_network)


vertices_CoxHae_toddler <- data.frame(vId_CoxHae_toddler = seq_len(vcount(graph_Child)),
                                      cue_CoxHae = names(V(graph_Child)))

vertices_POS_child <- merge(vertices_CoxHae_toddler, select(cdi_metadata, c("cue_CoxHae", "POS")))

cdi_metadata <- merge(cdi_metadata, vertices_CoxHae_toddler, by = "cue_CoxHae", all.x = TRUE)


subset_CDI_NAs <- function(POS, v_ID, num_item_id){
  assertthat::are_equal(length(POS), length(v_ID))
  assertthat::assert_that(is.numeric(v_ID))
  assertthat::are_equal(length(POS), length(num_item_id))
  z <- num_item_id[!is.na(v_ID)]
  y <- POS[!is.na(v_ID)]
  x <- v_ID[!is.na(v_ID)]
  return(data.frame("v_ID" = x[order(x)], "POS" = y[order(x)], "num_item_id" = z[order(x)]))
}
vertices_CoxHae_toddler$vId_CoxHae_toddler <- as.numeric(vertices_CoxHae_toddler$vId_CoxHae_toddler)
vertices_POS_child <- subset_CDI_NAs(cdi_metadata$POS, cdi_metadata$vId_CoxHae_toddler, cdi_metadata$num_item_id)
save(vertices_POS_child, file = "data/vertices_POS_child.Rdata")








