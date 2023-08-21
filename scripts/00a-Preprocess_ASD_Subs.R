library(readr)
library(dplyr)
library(tidyr)
library(readxl)
source("R/Proportion_missing_on_checklist.R")
source('R/select_vocabulary_checklist_wg.R')
source('R/select_vocabulary_checklist_ws.R')
source('R/recode_NA_as_zero.R')
source('R/merge_split_records.R')
source('R/select_by_n_produced.R')
source("R/Identify_empty_checklist.R")
source("R/Proportion_missing_on_checklist.R")
source('R/filter_by_n_produced.R')
source('R/filter_missing_data_and_words_produced.R')
source('R/adds_nProduced_column.R')
source('R/keep_best_assessments.R')
source('R/makeDataFrame_out_of_best_record.R')
## 1. Load data ###################################################
load("data/cdi-metadata.Rdata")
load("data/words_gestures_fix_codes.Rdata")
## Latest CDI pull from NDAR
mci_sentences02 <- read.csv("data/mci_sentences02_April2023.csv", na.strings = "")[-1,]
mci_words_gestures <- read.csv("data/mci_words_gestures01.csv")[-1,]

## Current list of GUID's that we are confident have ASD April 2023
ASD_subjects <- read.csv('data/ASD_CDI_list_April20_2023.csv')
##########################################################################
# Select columns that correspond to words and useful metadata
# Same for words and gestures.
word_dict_S <- read.csv("data/word_dict_S_April2023.csv")
names(word_dict_S) <- c('num_item_id', 'word_code', 'word1', 'word2', 'WordBank1', 'WordBank2', 'category', 'class', 'classForm', 'classEH', 'classRS','classEHse','classVerbs', 'CDI_Metadata_compatible')
word_dict_WG <- read.csv("data/word_dict_WG.csv")
names(word_dict_WG) <- c('num_item_id','word_code', 'word1', 'word2', 'WordBank1', 'WordBank2', 'category', 'class', 'classForm', 'classEH', 'classRS', 'classEHse','classVerbs', 'CDI_Metadata_compatible')





## 2. Selecting vocabulary checkists and other demographic info. ########################################
select_columns <- c('subjectkey', 'interview_age', 'sex','interview_date','mcs_vc_total','mci_sentences02_id', as.character(word_dict_S$word_code))
mci_sentences02 <- mci_sentences02[select_columns] %>%
  filter(!duplicated(.)) %>%
  mutate(form = "WS",
         interview_age = as.integer(interview_age))

select_columns <- c('subjectkey', 'interview_age', 'sex','interview_date','mcg_vc_totpr','mci_words_gestures01_id', as.character(word_dict_WG$word_code))
mci_words_gestures <- mci_words_gestures[select_columns] %>%
  filter(!duplicated(.))%>%
  mutate(form = "WG",
         interview_age = as.integer(interview_age))
####################################################################################################



## 3. Fix split data ###################################################3
## Fix split data for Words and Gestures
WG_half <- data.frame(
  subjectkey=c(
    'NDARZV952HB6',
    'NDARJE882YXF',
    'NDARJL403NHA',
    'NDARPE246ADU',
    'NDARTB675VKB',
    'NDARTB675VKB',
    'NDARYH239CVZ'),
  interview_age = c(88,91, 92, 118, 49, 62, 69)
)
for (i in 1:nrow(WG_half)) {
  s <- as.character(WG_half$subjectkey)[i]
  a <- WG_half$interview_age[i]
  print(c(subjectkey = s, interview_age = a))
  mci_words_gestures<- merge_split_records(mci_words_gestures, subject = s, age = a)
}

## Fix data split for Words and sentences
WS_half <- data.frame(
  subjectkey=c(
    'NDAR_INVSD261LFJ',
    'NDAREK314FNC',
    'NDARFZ559LLV',
    'NDARJG616TFG'),
  interview_age = c(59, 83, 107, 52)
)
for (i in 1:nrow(WS_half)) {
  s <- as.character(WS_half$subjectkey)[i]
  a <- WS_half$interview_age[i]
  print(c(subjectkey = s, interview_age = a))
  mci_sentences02 <- merge_split_records(mci_sentences02, subject = s, age = a)
}

# Subject NDAREK314FNC needs to be merged across two ages, so we will fix them manually.
row_ind <- which(
  (mci_sentences02$subjectkey == "NDAREK314FNC") &
    ((mci_sentences02$interview_age == 83) | (mci_sentences02$interview_age == 84))
)
z <- as.vector(is.na(mci_sentences02[row_ind[1],]))
mci_sentences02[row_ind[1],z] <- mci_sentences02[row_ind[2],z]
mci_sentences02 <- mci_sentences02[-row_ind[2],]
########################################################################

## 4. Filter out kids younger than 11 months and missing data #################

ASD_wg <- mci_words_gestures %>%
  filter(subjectkey %in% ASD_subjects$subjectkey & interview_age >= 11) %>%
  mutate(empty = Identify_empty_checklist(.[,-c(1:6)], 0.9)) %>%
  filter(empty == FALSE ) %>%
  mutate(interview_date = as.Date(interview_date, form = "%m/%d/%Y"))

ASD_ws <- mci_sentences02 %>%
  filter(subjectkey %in% ASD_subjects$subjectkey & interview_age >= 11) %>%
  mutate(empty = Identify_empty_checklist(.[,-c(1:6)], 0.9)) %>%
  filter(empty == FALSE | subjectkey == "NDARMK278CK2")

############################################################################

## 5. Fix incorrect codes #################################################

ASD_wg_fix <- ASD_wg %>%
  right_join(., wg_switch_codes, by = c("subjectkey", "interview_age", "interview_date")) %>%
  filter(!is.na(sex))


for(i in 1:nrow(ASD_wg_fix)){
  for(j in 7:402){
    if(ASD_wg_fix[i,j] == 1){
      ASD_wg_fix[i,j] <- 2
    }else if(ASD_wg_fix[i,j] == 2){
      ASD_wg_fix[i,j]<- 1
    }else if(ASD_wg_fix[i,j] == 0){
      ASD_wg_fix[i,j] <- 0
    }
  }

}

ASD_wg_correct <- ASD_wg %>%
  full_join(.,wg_switch_codes, by  = c("subjectkey", "interview_age","interview_date") ) %>%
  filter(is.na(fix))

ASD_wg <- rbind(ASD_wg_fix, ASD_wg_correct)
#####################################################################################

## 6. Long format data and filter to between 20 and 600 words produced #####################################
ASD_wg_long <- ASD_wg %>%
  pivot_longer(.,
               cols = matches("^mcg_vc?[0-9]"),
               values_to = "response_code",
               names_to = "word_code") %>%
  mutate(response_code = as.numeric(response_code),
         interview_age = as.numeric(interview_age),
        mci_words_gestures01_id = as.numeric(mci_words_gestures01_id)) %>%
  mutate_at(vars("response_code"), ~replace_na(.,0)) %>%
  mutate(Produces = ifelse(response_code == 2, TRUE, FALSE)) %>%
  group_by(subjectkey, interview_age, interview_date, mci_words_gestures01_id) %>%
  mutate(nProduced = sum(Produces)) %>%
  filter(nProduced >= 20 & nProduced <= 600) %>%
  left_join(.,word_dict_WG, by = "word_code") %>%
  ungroup() %>%
  select(-mci_words_gestures01_id, -fix, -mcg_vc_totpr)

ASD_ws_long <- ASD_ws %>%
  pivot_longer(.,
               cols = matches("^mcs_vc?[0-9]"),
               values_to = "response_code",
               names_to = "word_code") %>%
  mutate(response_code = as.numeric(response_code),
         interview_age = as.numeric(interview_age),
         interview_date = as.Date(interview_date, form = "%m/%d/%Y")) %>%
  mutate_at(vars("response_code"), ~replace_na(.,0)) %>%
  mutate(Produces = ifelse(response_code == 1 | response_code == 3, TRUE, FALSE)) %>%
  group_by(subjectkey, interview_age, interview_date, mci_sentences02_id) %>%
  mutate(nProduced = sum(Produces)) %>%
  filter(nProduced >= 20 & nProduced <= 600) %>%
  left_join(.,word_dict_S, by = "word_code") %>%
  ungroup() %>%
  select(-mci_sentences02_id, -mcs_vc_total)


ASD_all_long <- rbind(ASD_wg_long, ASD_ws_long) %>%
  group_by(subjectkey) %>%
  mutate(best = nProduced == max(nProduced),
         group = "ASD") %>%
  filter(best == TRUE)%>%
  ungroup() %>%
  group_by(subjectkey) %>%
  mutate(newest_rec = interview_date == max(interview_date)) %>%
  group_by(subjectkey, interview_age) %>%
  filter(newest_rec == TRUE) %>%
  ungroup() %>%
  unique()


save(ASD_all_long, file = "data/ASD_long_data.Rdata")
###########################################################################

x <- ASD_all_long %>%
  group_by(subjectkey,form) %>%
  mutate(rep_age = n_distinct(interview_age)>1) %>%
  filter(rep_age == TRUE)




