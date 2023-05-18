library(dplyr)

source('R/assign_percentiles.R')

# Load normative data ----
# downloaded on 12/02/18 from Wordbank and renamed.#########
# http://wordbank.stanford.edu/analyses?name=instrument_data
mci_TDC_gestures <-read.csv(file="data/instrument_data_GESTURES.csv",header=TRUE,sep=",") # downloaded on 12/02/18 from Wordbank and renamed.  ##############
mci_TDC_sentences <-read.csv(file="data/instrument_data_SENTENCES.csv",header=TRUE,sep=",")

## Filter to only words and adding logical produced column ###################
mci_TDC_sentences <- mci_TDC_sentences %>%
  filter(type == 'word') %>%
  mutate(produced = (value == "produces")) %>%
  arrange(data_id)

mci_TDC_gestures <- mci_TDC_gestures%>%
  filter(type == 'word') %>%
  mutate(produced = (value == "produces")) %>%
  arrange(data_id)
############################################################
## Loading productive vocabulary norms.
vocabulary_norms_table_WG_CDI <- read.csv("data/words_produced_norms_table_WG_use.csv")
vocabulary_norms_table_WS_CDI <- read.csv("data/words_produced_norms_table_WS_use.csv")
############################################################
## Grouping NA into Typical and Late talkers above the 10th percentile
percent_thresh <- 10

lang_ability_WS <- assign_percentile_produces(mci_TDC_sentences, vocabulary_norms_table_WS_CDI, percent_thresh)
lang_ability_WG <- assign_percentile_produces(mci_TDC_gestures, vocabulary_norms_table_WG_CDI, percent_thresh)
## Counting TT vs LT
lang_ability_WS %>% group_by(group) %>% count()
lang_ability_WG %>% group_by(group) %>% count()



## Remove LT and add nProduced and add num_item_id and lemmas #########################33
merged_lemmas_WS <- read.csv("data/merged_lemma_WS.csv")
merged_lemmas_WG <- read.csv("data/merged_lemma_WG.csv")

mci_TDC_gestures_TD <- mci_TDC_gestures %>%
  filter(data_id %in% lang_ability_WG[lang_ability_WG$group == "TD",]$data_id) %>%
  group_by(data_id, age) %>%
  mutate(nProduced = sum(produced)) %>%
  rename(
    subjectkey = data_id,
    interview_age = age,
    word = definition) %>%
  left_join(., merged_lemmas_WG, by = c("word" = "definition"))

mci_TDC_sentences_TD <- mci_TDC_sentences %>%
  filter(data_id %in% lang_ability_WS[lang_ability_WS$group == "TD",]$data_id) %>%
  group_by(data_id, age) %>%
  mutate(nProduced = sum(produced)) %>%
  rename(
    subjectkey = data_id,
    interview_age = age,
    word = definition) %>%
  left_join(., merged_lemmas_WS, by = c("word" = "definition"))
#######################################################################
## No repeats across lists and no kid with multiple entries.
sum(unique(mci_TDC_gestures_TD$subjectkey) %in% unique(mci_TDC_sentences_TD$subjectkey))

mci_TDC_gestures_TD %>%
  group_by(subjectkey) %>%
  mutate(rep_age = n_distinct(interview_age)>1) %>%
  filter(rep_age == TRUE)

mci_TDC_sentences_TD %>%
  group_by(subjectkey) %>%
  mutate(rep_age = n_distinct(interview_age)>1) %>%
  filter(rep_age == TRUE)
#########################################################
## Combine forms (no repeated subjectkeys between or within forms so I can just
## rbind.) and filter vocab size to between 20 and 600 words
mci_all <- rbind(mci_TDC_gestures_TD, mci_TDC_sentences_TD) %>%
  select(subjectkey, interview_age, sex, word, produced, nProduced, lemma, num_item_id) %>%
  filter(nProduced >= 20 & nProduced <= 600,
         !is.na(sex)) %>%
  mutate(group = "NA",
         sex = ifelse(sex == "Male" | sex == "M", "M", "F"))




save(mci_all, file = "data/NA_all_long.Rdata")







