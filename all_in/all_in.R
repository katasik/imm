library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)
library(dplyr)
install.packages("psych")
library(psych)

#defining important blocks
important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")
crit_pos <- c("crit_pos")
crit_neg <- c("crit_neg")
chall_pos <- c("chall_pos")
chall_neg <- c("chall_neg")
important_crit_words <- c("visszajelzés", "értékelés", "vélemény", "destruktív", "konstruktív", "bírálat")

# read data -------------------------------------------------------------------------

#implicit data
gnat_raw <- 
  vroom(file = list.files(path = "/Users/katasik/Downloads/thesisdata_all/final/", pattern = ".*.txt",
                          full.names = TRUE), 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target"), 
        id = "id")

gnat_important<- gnat_raw %>%  
  filter(target %in% important_targets) %>% 
  filter(trial_type=="go_trial")

correct_rate <-
  gnat_important %>%  
  group_by(id, target) %>% 
  summarise(correct = 1 - mean(error))
  
##must delete D12 and D130, as block error rate is above 40%, final_data df is created later
final_data <- final_data[-c(24, 36), ]

#deleting rows with too quick and too slow response windows (too quick is 300 and too slow is 3 SD above averagert ==631+126*3)
gnat_final %>% 
  summarise(pers_a = mean(rt))
gnat_final %>% 
  summarise(pers_sd = sd(rt))

gnat_final<- 
  gnat_important%>% filter(rt %in% (300:1009))


#explicit data
explicit_raw <- read.xlsx("//Users/katasik/Downloads/thesisdata_all/explicit.xlsx", sheetIndex = 1)

##cleaning explicit data
#reversing items
explicit_reversed <- explicit_raw %>% 
  mutate_at(vars(IQ1.1, IQ2.1, FMS3.1, FMS4.1, CrMS3.1, CrMS4.1, ChMS3.1, ChMS4.1), 
            ~recode(., `1` = 6,
                    `2` = 5,
                    `3` = 4,
                    `4` = 3,
                    `5` = 2,
                    `6` = 1,
            ))

explicit_reversed <- explicit_reversed %>% 
  mutate_at(vars(Failurescenario.1, Criticismscenario.1), 
            ~recode(., `1` = 5,
                    `2` = 4,
                    `4` = 2,
                    `5` = 1,
            ))

#creating means of explicit measures
explicit_coded<-explicit_reversed %>%
  mutate(IQMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "IQ"))))
explicit_coded<-explicit_coded %>%
  mutate(CRMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "CrMS"))))
explicit_coded<-explicit_coded %>%
  mutate(CHMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "ChMS"))))
explicit_coded<-explicit_coded %>%
  mutate(FMS_M = rowMeans(x = select(.data = .,
                                     starts_with(match = "FMS"))))
explicit_coded<-explicit_coded %>%
  mutate(FSC_M = rowMeans(x = select(.data = .,
                                     starts_with(match = "Failure"))))
explicit_coded<-explicit_coded %>%
  mutate(CrSC_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "Criticism"))))
#final explicit data
explicit <-
  explicit_coded %>% 
  select(id, Challengescenario.1, gender.1, age.1, IQMS_M, CRMS_M, CHMS_M, FMS_M, FSC_M, CrSC_M)

##cleaning implicit data

gnat_final <- gnat_final %>% 
  mutate(id = str_remove(id, ".txt")) %>% 
  mutate(id = str_remove(id, "/Users/katasik/Downloads/thesisdata_all/final//"))

dscores <-
  gnat_final %>% 
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  separate(target, c("target1", "target2")) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  unite(targets, c("target1", "target2"), sep = "_") %>% 
  spread(targets, d) %>% 
  transmute(challange_d = chall_neg - chall_pos,
            crit_d = crit_neg - crit_pos)



#calculating d scores for separate blocks
d_spread<-
  gnat_final %>%  
  filter(target %in% important_targets) %>%
  filter(trial_type=="go_trial") %>% 
  filter(error=="0") %>% 
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  spread(target, d)

#calculating correct rate
correct_rate <-
  gnat_final %>%  
  filter(target %in% important_targets) %>%
  group_by(id) %>% 
  summarise(correct = 1 - mean(error))

personal_average <-
  gnat_final %>%  
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  ungroup()

personal_average<-personal_average %>% 
  spread(target, pers_block_avg)

#collecting all data in one dataframe
final_data <- correct_rate %>% 
  left_join(explicit, by = "id")
final_data <- dscores %>% 
  left_join(final_data, by = "id")
final_data <- d_spread %>% 
  left_join(final_data, by = "id")
final_data <- personal_average %>% 
  left_join(final_data, by="id")


#selecing cases with correct rate 90% and above

final_data <- 
  final_data %>% 
  filter(correct>="0.8")

#selecting random numbers
set.seed(1)
randomly<-
  final_data [sample(nrow(final_data ), 86), ] 

single_words <- crit_neg_final %>% 
  left_join(crit_pos_final, by = "file") %>% 
  left_join(chall_neg_final, by = "file") %>% 
  left_join(chall_pos_final, by = "file")

crit_pos <- gnat_raw %>% 
  filter(target %in% crit_pos) %>% 
  filter(trial_type=="go_trial") %>% 
  group_by(file, word) %>%
    summarise(word_rt = mean(rt)) %>% 
  ungroup()

crit_pos_tidy<- crit_pos %>% 
  mutate(file=str_remove(file, "/Users/katasik/Downloads/thesisdata_all/final//D")) %>% 
  mutate(file=str_remove(file, ".txt"))

crit_pos_final<- crit_pos_tidy %>% 
  spread(word, word_rt)

crit_neg <- gnat_raw %>% 
  filter(target %in% crit_neg) %>% 
  filter(trial_type=="go_trial") %>% 
  group_by(file, word) %>%
  summarise(word_rt = mean(rt)) %>% 
  ungroup()

crit_neg_tidy<- crit_neg %>% 
  mutate(file=str_remove(file, "/Users/katasik/Downloads/thesisdata_all/final//D")) %>% 
  mutate(file=str_remove(file, ".txt"))

crit_neg_final<- crit_neg_tidy %>% 
  spread(word, word_rt)

chall_neg <- gnat_raw %>% 
  filter(target %in% chall_neg) %>% 
  filter(trial_type=="go_trial") %>% 
  group_by(file, word) %>%
  summarise(word_rt = mean(rt)) %>% 
  ungroup()

chall_neg_tidy<- chall_neg %>% 
  mutate(file=str_remove(file, "/Users/katasik/Downloads/thesisdata_all/final//D")) %>% 
  mutate(file=str_remove(file, ".txt"))

chall_neg_final<- chall_neg_tidy %>% 
  spread(word, word_rt)


chall_pos <- gnat_raw %>% 
  filter(target %in% chall_pos) %>% 
  filter(trial_type=="go_trial") %>% 
  group_by(file, word) %>%
  summarise(word_rt = mean(rt)) %>% 
  ungroup()

chall_pos_tidy<- chall_pos %>% 
  mutate(file=str_remove(file, "/Users/katasik/Downloads/thesisdata_all/final//D")) %>% 
  mutate(file=str_remove(file, ".txt"))

chall_pos_final<- chall_pos_tidy %>% 
spread(word, word_rt)
  

feedback_neg <- gnat_raw %>% 
  filter(target %in% crit_neg) %>%
  filter(word %in% feedback) %>%
  filter(trial_type=="go_trial") %>%
  group_by(file) %>% 
  mutate(feedback_rt = mean(rt))

evaluation <- gnat_raw %>% 
  filter(target %in% crit_pos) %>%
  filter(word %in% evaluation) %>%
  filter(trial_type=="go_trial") %>%
  group_by(file) %>% 
  summarize(evaluation_rt = mean(rt))

opinion <- gnat_raw %>% 
  filter(target %in% crit_pos) %>%
  filter(word %in% opinion) %>%
  filter(trial_type=="go_trial") %>%
  group_by(file) %>% 
  mutate(opinion_rt = mean(rt))
  
correct_rate2<-
correct_rate %>% mutate(file = str_remove(file, "/Users/katasik/Downloads/thesisdata_all//")) %>% 
  mutate(file = str_remove(file, ".txt"))

as.numeric(correct_rate2$file)
  

 
  filter(target %in% important_targets) %>% 
  group_by(file)

dscores_errorminus2 <- dscores_errorminus %>% 
  mutate(id = str_remove(id, "/Users/katasik/Downloads/thesisdata_all/final//D"))

### included no_go_trials
dscores_wrong <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  group_by(file) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(file, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  ungroup() %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  separate(target, c("target1", "target2")) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  unite(targets, c("target1", "target2"), sep = "_") %>% 
  spread(targets, d) %>% 
  transmute(id = str_remove_all(file, "data/pilot_2/|.txt"),
            challange_d = chall_neg - chall_pos,
            crit_d = crit_neg - crit_pos)

### with only go_trials
dscores <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  filter(trial_type=="go_trial") %>% 
  filter(error=="0") %>% 
  group_by(file) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(file, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  ungroup() %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  separate(target, c("target1", "target2")) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  unite(targets, c("target1", "target2"), sep = "_") %>% 
  spread(targets, d) %>% 
  transmute(id = str_remove_all(file, "data/pilot_2/|.txt"),
            challange_d = chall_neg - chall_pos,
            crit_d = crit_neg - crit_pos)

dscores_error <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  group_by(file, target) %>% 
  summarise(pers_error = mean(error)) %>% 
  ungroup() %>% 
  spread(target, pers_error)

dscores_correct <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  group_by(file, target) %>% 
  summarise(pers_correct = mean(error)) %>% 
  ungroup() %>% 
  mutate(file = str_remove(file, ".txt")) %>% 
  mutate(file = str_remove(file, "/Users/katasik/Downloads/thesisdata_all/final//")) %>% 
  spread(target, pers_error)

dscores_error1 <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  filter(trial_type=="go_trial") %>% 
  group_by(file, target, error) %>% 
  ungroup()
  
personal_average <-
  gnat_final %>%  
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  ungroup()

personal_average<-personal_average %>% 
spread(target, pers_block_avg)

raw_singlewords<-
  raw_singlewords %>% 
  mutate(file = str_remove(file, "/Users/katasik/Downloads/thesisdata_all/final//")) %>% 
  mutate(file = str_remove(file, ".txt")) %>% 
  mutate(file = str_remove(file, "D"))
  
dscores<-
  dscores %>% 
  mutate(id = str_remove(id, "/Users/katasik/Downloads/thesisdata_all//")) %>% 
  mutate(id = str_remove(id, ".txt")) %>% 
  mutate(id = str_remove(id, "D"))

dscores %>% 
  left_join(explicit, by = "id") %>% 
  select(-id) %>%
  psych::cor.plot(numbers = TRUE, cuts = FALSE, pval = FALSE)

dscores %>% 
  left_join(explicit, by = "id") %>% 
  write.xlsx("dscoresexplicit.xlsx")

write.xlsx(dscores, "dscores_stat.xlsx")
write.xlsx(correct_rate, "correct_rate_stat.xlsx")

explicit_reversed <- explicit_raw %>% 
  mutate_at(vars(IQ1.1, IQ2.1, FMS3.1, FMS4.1, CrMS3.1, CrMS4.1, ChMS3.1, ChMS4.1), 
            ~recode(., `1` = 6,
                    `4` = 3,
                    `3` = 4,
                    `2` = 5,
                    `5` = 2,
                    `6` = 1,
            ))

explicit_reversed <- explicit_reversed %>% 
  mutate_at(vars(Failurescenario.1, Criticismscenario.1), 
            ~recode(., `1` = 5,
                    `2` = 4,
                    `4` = 2,
                    `5` = 1,
            ))

explicit_coded<-explicit_reversed %>%
  mutate(IQMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "IQ"))))
explicit_coded<-explicit_coded %>%
  mutate(CRMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "CrMS"))))
explicit_coded<-explicit_coded %>%
  mutate(CHMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "ChMS"))))
explicit_coded<-explicit_coded %>%
  mutate(FMS_M = rowMeans(x = select(.data = .,
                                     starts_with(match = "FMS"))))
explicit_coded<-explicit_coded %>%
  mutate(FSC_M = rowMeans(x = select(.data = .,
                                     starts_with(match = "Failure"))))
explicit_coded<-explicit_coded %>%
  mutate(CrSC_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "Criticism"))))


write.csv(randomly, "randomly_new.csv")
openxlsx::write.xlsx(final_data, file = "final.xlsx")

set.seed(1)
d_spread[sample(nrow(final_data ), 86), ] 

randomly<-dscores[sample(nrow(dscores), 86), ] %>% 
  mutate(id = str_remove(id, "/Users/katasik/Downloads/thesisdata_all/final//")) %>% 
  mutate(id = str_remove(id, ".txt")) %>% 
  mutate(id = str_remove(id, "D"))
