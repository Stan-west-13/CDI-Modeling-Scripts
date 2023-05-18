load("data/Child_Stats_ASD_balRAN_1000.Rdata")
load("data/Child_Stats_TD_balRAN_1000.Rdata")
load("data/ASD_NA_metadataNetstats_2023.Rdata")
library(dplyr)
library(purrr)
row_SD <- function(d){
  return(apply(d, 1, sd, na.rm = TRUE))
}

z_score <- function(true, RAN_mean, RAN_sd){
  return((true - RAN_mean)/RAN_sd)
}

ASD_ran_descrip_mean <- stats_ASD_RAN %>%
  map_dfr(., .f = rowMeans, na.rm = TRUE, .id = "subjectkey") %>%
  rename(indegree_mean_RAN_mean = indegree_mean,
         indegree_median_RAN_mean = indegree_median,
         clustcoef_RAN_mean = clustcoef,
         meandist_RAN_mean = meandist)

ASD_ran_descrip_sd <- stats_ASD_RAN %>%
  map_dfr(., .f = row_SD, .id = "subjectkey") %>%
  rename(indegree_mean_RAN_sd = indegree_mean,
         indegree_median_RAN_sd = indegree_median,
         clustcoef_RAN_sd = clustcoef,
         meandist_RAN_sd = meandist)

ASD_stats_RAN <- cbind(ASD_ran_descrip_mean, ASD_ran_descrip_sd[,-1])




TD_ran_descrip_mean <- stats_TD_RAN %>%
  map_dfr(., .f = rowMeans, na.rm = TRUE, .id = "subjectkey") %>%
  rename(indegree_mean_RAN_mean = indegree_mean,
         indegree_median_RAN_mean = indegree_median,
         clustcoef_RAN_mean = clustcoef,
         meandist_RAN_mean = meandist)

TD_ran_descrip_sd <- stats_TD_RAN %>%
  map_dfr(., .f = row_SD, .id = "subjectkey") %>%
  rename(indegree_mean_RAN_sd = indegree_mean,
         indegree_median_RAN_sd = indegree_median,
         clustcoef_RAN_sd = clustcoef,
         meandist_RAN_sd = meandist)
TD_stats_RAN <- cbind(TD_ran_descrip_mean, TD_ran_descrip_sd[,-1])

RAN_stats <- rbind(ASD_stats_RAN, TD_stats_RAN)

ASD_NA_metadataNetstats_2023_RAN <- ASD_NA_metadataNetstats_2023 %>%
  left_join(., RAN_stats)





z_table <- ASD_NA_metadataNetstats_2023_RAN %>%
  select(subjectkey, indegree, median_indegree, indegree_mean_RAN_mean,indegree_median_RAN_mean,
         indegree_median_RAN_sd, indegree_mean_RAN_sd, mean_distance, meandist_RAN_mean, meandist_RAN_sd,
         clustering_coefficient, clustcoef_RAN_mean,clustcoef_RAN_sd) %>%
  unique()

z_table <- z_table %>%
  droplevels() %>%
  group_by(subjectkey) %>%
  mutate(z_indegree_mean = z_score(indegree, indegree_mean_RAN_mean, indegree_mean_RAN_sd),
         z_indegree_med = z_score(median_indegree, indegree_median_RAN_mean, indegree_median_RAN_sd ),
         z_clust = z_score(clustering_coefficient, clustcoef_RAN_mean, clustcoef_RAN_sd ),
         z_dist = z_score(mean_distance, meandist_RAN_mean, meandist_RAN_sd ))


ASD_NA_metadataNetstats_2023_RAN <- ASD_NA_metadataNetstats_2023_RAN %>%
  left_join(.,select(z_table, subjectkey, z_indegree_mean, z_indegree_med, z_clust, z_dist))

save(ASD_NA_metadataNetstats_2023_RAN, file = "data/ASD_NA_metadataNetstats_2023_RAN.Rdata")
