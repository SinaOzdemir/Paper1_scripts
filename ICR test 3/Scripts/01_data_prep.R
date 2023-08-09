# PR+ manual coding inter coder reliablity test data prep #
# Author: Sina Özdemir                                    #
#         sina.ozdemir@ntnu.no                            #
#         Department of sociology and political science   #
# Date: 21/03/2022                                        #
###########################################################

# setup -------------------------------------------------------------------

library(pacman)
packs<- c("tidyverse","xlsx","here")

p_load(char = packs)

test_sample<- read.xlsx(file = here("Data","icr_test_data_3.xlsx"),sheetIndex = 1) %>% 
  mutate(tweet_id = str_extract_all(string = status_link,pattern = "/[:digit:]+"),
         tweet_id = str_remove_all(string = tweet_id, pattern = "/")) %>% 
  pull(tweet_id)

coded_sample<- read.xlsx(file = here("Data","data_PR_plus_2022-03-21_17-20.xlsx"),sheetIndex = 1) %>% 
  filter(V101_01 %in% test_sample) %>% 
  filter(CI01 == 1 | CI01 == 2)


# extract recoded tweets --------------------------------------------------

recoded_extractor<- function(data, coder_id_col, coder_id, post_id_col){
  coder_data <- data %>%
    filter({{coder_id_col}} == coder_id)
  
  coder_data_no_dupes<- coder_data %>%
    filter(!duplicated({{post_id_col}}))
  
  coder_data_dupes<- coder_data %>%
    filter(duplicated({{post_id_col}})) %>%
    group_by({{post_id_col}}) %>% 
    filter(STARTED == which.max(STARTED))
  
  clean_data<- rbind(coder_data_no_dupes, coder_data_dupes)
  
  return(clean_data)
}



kg<- recoded_extractor(data = coded_sample,coder_id_col = CI01,coder_id = 2,post_id_col = V101_01)

sfo<- recoded_extractor(data = coded_sample,coder_id_col = CI01,coder_id = 1,post_id_col = V101_01)

clean_coded_sample<- rbind(kg,sfo)

xlsx::write.xlsx(x = clean_coded_sample,file = here("Data","ICR_test3_coded_data.xlsx"),row.names = F)

# variable_groups ---------------------------------------------------------

clean_coded_sample <- xlsx::read.xlsx(file = here("Data","ICR_test3_coded_data.xlsx"),sheetIndex = 1)

sop_test_data<- clean_coded_sample %>% select(CI01,V101_01,V201)

oop_test_data<- clean_coded_sample %>% select(CI01,V101_01,matches(match = "V30*"))

pa_test_data<- clean_coded_sample %>% select(CI01,V101_01,matches(match= "V40*"))

saveRDS(object = sop_test_data, file = here("Data","sop_test_data.rds"))
saveRDS(object = oop_test_data, file = here("Data","oop_test_data.rds"))
saveRDS(object = pa_test_data, file = here("Data","pa_test_data.rds"))

rm(list = ls())

