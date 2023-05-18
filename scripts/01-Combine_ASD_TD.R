load("data/ASD_long_data.Rdata")
load("data/NA_all_long.Rdata")
load("data/cdi-metadata.Rdata")
library(dplyr)

ASD_prep <- ASD_all_long %>%
  select(subjectkey, interview_age, sex, Produces,
         nProduced, num_item_id,group) %>%
  mutate(subjectkey = as.factor(subjectkey))

NA_prep <- mci_all%>%
  select(subjectkey, interview_age, sex, Produces = produced,
         nProduced, num_item_id,group) %>%
  mutate(subjectkey = as.factor(subjectkey)) %>%
  mutate(sex = ifelse(sex == "Male" | sex == "M","M","F" ))


ASD_NA_metadata_2023 <- rbind(ASD_prep, NA_prep) %>%
  left_join(., cdi_metadata, by = "num_item_id")

save(ASD_NA_metadata_2023, file = 'data/ASD_NA_metadata2023.Rdata')

ASD_NA_metadata_2023 %>%
  select(subjectkey,group,sex) %>%
  unique() %>%
  group_by(group) %>%
  count(sex)
