library(dplyr)
library(effectsize)
library(ggplot2)
library(svglite)
load("data/ASD_NA_metadataNetstats_2023_RAN.Rdata")

ASD_NA_metadataNetstats_2023_RAN_match_unique <- ASD_NA_metadataNetstats_2023_RAN %>%
  select(subjectkey, indegree, median_indegree, indegree_mean_RAN_mean,indegree_median_RAN_mean,
         indegree_median_RAN_sd, indegree_mean_RAN_sd, mean_distance, meandist_RAN_mean, meandist_RAN_sd,
         clustering_coefficient, clustcoef_RAN_mean,clustcoef_RAN_sd, z_indegree_mean, z_indegree_med, z_clust, z_dist, nProduced,group) %>%
  unique()


## OrthPoly child plots and models
orth_poly <- poly(ASD_NA_metadataNetstats_2023_RAN_match_unique$nProduced, 3)
orth_poly <- orth_poly %>%
  as.data.frame()%>%
  mutate(nProduced = ASD_NA_metadataNetstats_2023_RAN_match_unique$nProduced)

names <- c("linear", "quadratic", "cubic", "nProduced")
colnames(orth_poly) <- names

ASD_TD_data_graph <- cbind(ASD_NA_metadataNetstats_2023_RAN_match_unique, select(orth_poly, c("linear", "quadratic", "cubic")))
ASD_TD_data_graph$npg <- cut(ASD_TD_data_graph$nProduced, breaks = seq(0, max(ASD_TD_data_graph$nProduced), by = 20))
ASD_TD_data_graph$group <- as.factor(ASD_TD_data_graph$group)
contrasts(ASD_TD_data_graph$group) <- c(-0.5, 0.5)

g <- levels(as.factor(ASD_TD_data_graph$group))
nProduced <- unique(ASD_TD_data_graph$nProduced)


## CoxHae child Linear Models
lin_orth_CoxHae_child_clust <- lm(z_clust ~ (linear+quadratic+cubic) * group, data = ASD_TD_data_graph)
summary(lin_orth_CoxHae_child_clust)
lin_orth_CoxHae_child_indegree <- lm(z_indegree_med ~ (linear+quadratic+cubic) * group, data = ASD_TD_data_graph)
summary(lin_orth_CoxHae_child_indegree)
lin_orth_CoxHae_child_distance <- lm(z_dist ~ (linear+quadratic+cubic) * group, data = ASD_TD_data_graph)
summary(lin_orth_CoxHae_child_distance)


dNew_CoxHae_child <- subset(ASD_TD_data_graph, select = c(subjectkey,nProduced, linear, quadratic, cubic, group,z_clust, z_indegree_med, z_dist))


dNew_CoxHae_child$predictions_clust <- predict(lin_orth_CoxHae_child_clust, dNew_CoxHae_child)

ggplot(ASD_TD_data_graph, aes(x = nProduced, y = z_clust)) +
  #geom_point(alpha = 0.5, aes(shape = group))+
  theme_classic(base_size = 12)+
  theme(axis.title = element_text(size = 12),
        legend.position = c(.5, .3),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  #geom_line(dNew_CoxHae_child, mapping = aes(x = nProduced, y = predictions_clust, linetype = group), size = 3)+
  geom_smooth(dNew_CoxHae_child,method = "lm",
              formula = y ~ poly(x,3),
              mapping = aes(x = nProduced, y = z_clust, linetype = group),
              color = "black",size = 2)+
  scale_linetype_manual(labels = c("Non-Austistic", "Autistic"), values = c("dotted", "solid"))+
  labs(x = "Number of Words Produced", y = "Z-Score")+
  ggtitle("Clustering Coefficient Trajectory")





dNew_CoxHae_child$predictions_indegree <- predict(lin_orth_CoxHae_child_indegree, dNew_CoxHae_child)

ggplot(ASD_TD_data_graph, aes(x = nProduced, y = z_indegree_med)) +
  #geom_point(alpha = 0.5, aes(shape = group))+
  theme_classic(base_size = 12)+
  theme(axis.title = element_text(size = 12),
        legend.position = c(.5, .3),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  #geom_line(dNew_CoxHae_child, mapping = aes(x = nProduced, y = predictions_clust, linetype = group), size = 3)+
  geom_smooth(dNew_CoxHae_child,method = "lm",
              formula = y ~ poly(x,3),
              mapping = aes(x = nProduced, y = z_indegree_med, linetype = group),
              color = "black",size = 2)+
  scale_linetype_manual(labels = c("Non-Austistic", "Autistic"), values = c("dotted", "solid"))+
  labs(x = "Number of Words Produced", y = "Z-Score")+
  ggtitle("Median Indegree Trajectory")


dNew_CoxHae_child$predictions_dist <- predict(lin_orth_CoxHae_child_distance, dNew_CoxHae_child)

ggplot(ASD_TD_data_graph, aes(x = nProduced, y = z_dist)) +
  #geom_point(alpha = 0.5, aes(shape = group))+
  theme_classic(base_size = 12)+
  theme(axis.title = element_text(size = 12),
        legend.position = c(.5, .3),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  #geom_line(dNew_CoxHae_child, mapping = aes(x = nProduced, y = predictions_clust, linetype = group), size = 3)+
  geom_smooth(dNew_CoxHae_child,method = "lm",
              formula = y ~ poly(x,3),
              mapping = aes(x = nProduced, y = z_dist, linetype = group),
              color = "black",size = 2)+
  scale_linetype_manual(labels = c("Non-Austistic", "Autistic"), values = c("dotted", "solid"))+
  labs(x = "Number of Words Produced", y = "Z-Score")+
  ggtitle("Median Indegree Trajectory")



############## SRCLD plot
ggplot(ASD_TD_data_graph, aes(x = nProduced, y = z_clust)) +
  #geom_point(alpha = 0.5, aes(shape = group))+
  theme_classic(base_size = 24)+
  theme(axis.title = element_text(size = 24),
        legend.position = c(.5, .3),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 24))+
  #geom_line(dNew_CoxHae_child, mapping = aes(x = nProduced, y = predictions_clust, linetype = group), size = 3)+
  geom_smooth(dNew_CoxHae_child,method = "lm",
              formula = y ~ poly(x,3),
              mapping = aes(x = nProduced, y = z_clust, fill = group),
              color = "black",size = 2)+
  scale_fill_manual(labels = c("Non-Austistic", "Autistic"), values = c("#FDD023", '#461D7C'))+
  labs(x = "Number of Words Produced", y = "Z-Score")
ggsave("Figures/clust_coefSRCLD.pdf", height =18, width = 18, units = "cm" ) 


