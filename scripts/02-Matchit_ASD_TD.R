load("data/ASD_NA_metadata2023.Rdata")
library(MatchIt)


## Separate data to unique Male and Female subject IDs.########################
ASD_NA_metadata_2023$group <- as.factor(ASD_NA_metadata_2023$group)
ASD_NA_metadata_2023$group <- factor(ASD_NA_metadata_2023$group, levels = c("NA","ASD"), labels = c("NA", "ASD"))

ASD_NA_metadata_2023_uniq <- ASD_NA_metadata_2023 %>%
  select(subjectkey, sex, group, interview_age, nProduced) %>%
  unique()

ASD_NA_metadata_2023_uniq_M <- ASD_NA_metadata_2023_uniq %>%
  filter(sex == "M")

ASD_NA_metadata_2023_uniq_F <- ASD_NA_metadata_2023_uniq %>%
  filter(sex == "F")
#########################################################################

## Fit models to match nProduced within Males########################################
m.outmale <- matchit(group ~ nProduced , data = ASD_NA_metadata_2023_uniq_M,
                  method = "optimal", distance = "glm", ratio = 5)
summary(m.outmale)

plot(m.outmale, type = "density", interactive = TRUE,
     which.xs = ~nProduced )

subs_M <- match.data(m.outmale)
################################################################

## Fit models to match nProduced within Females######################################
m.outfemale <- matchit(group ~ nProduced , data = ASD_NA_metadata_2023_uniq_F,
                  method = "optimal", distance = "glm", ratio = 5)
summary(m.outfemale)

plot(m.outfemale, type = "density", interactive = TRUE,
     which.xs = ~nProduced )

subs_F <- match.data(m.outfemale)
##############################################################

## Combine matched subs
subs_all <- rbind(subs_F, subs_M)

sub_all_chi <- subs_all %>%
  ungroup() %>%
  select(subjectkey,sex,group) %>%
  mutate(sex = as.factor(sex)) 

chisq.test(sub_all_chi$group, sub_all_chi$sex)
t.test(nProduced~group, data = subs_all)



ASD_NA_metadata_2023_match<- ASD_NA_metadata_2023 %>%
  filter(subjectkey %in% subs_all$subjectkey)

save(ASD_NA_metadata_2023_match, file = "data/ASD_NA_metadata_2023_match.Rdata")
