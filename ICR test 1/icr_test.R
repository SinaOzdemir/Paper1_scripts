# Preliminary inter-coder agreement analysis


# setup -------------------------------------------------------------------

library(pacman)
packs<- c("tidyverse", "jaccard", "irr","xlsx","here")
p_load(char = packs, update = T)

data_path<- here::here("Analysis","Tests","Codebook tests","Test 4")

data<- xlsx::read.xlsx(file = file.path(data_path,"data_PR_test_2021-12-06_12-12.xlsx"),
                       sheetIndex = 1,header = T,StringAsFactor = F)

var_details<- data[1,] %>% as.vector(mode = "character")
# coder_efficiency --------------------------------------------------------

coding_time_data<- data[-1,] %>% 
  select(CI01, TIME_SUM,V101_01) 

coding_time_data<- coding_time_data[!duplicated(coding_time_data[c("CI01", "V101_01")]), ] %>%
  drop_na() %>%rename(tweet_id = V101_01,
                      coder_id = CI01,
                      coding_time = TIME_SUM)



coder_efficiency_plot<- coding_time_data %>%
  mutate(coding_time = as.numeric(coding_time),
         coding_time = round((coding_time/60),1)) %>%
  ggplot(aes( x = coder_id,y = coding_time))+geom_boxplot()+
  theme_minimal()+
  labs(x = "Coder ID",y = "Coding time (mins)")

ggsave(filename = "coder_times.jpeg",plot = coder_efficiency_plot,path = data_path)



# percentage_agreement ----------------------------------------------------

data("video")

#test

sop<- data[-1,] %>% select(CI01,V101_01,V201) %>%
  drop_na() %>%
  pivot_wider(id_cols = V101_01,
              names_from = CI01,
              values_from = V201,
              names_prefix = "coder_") %>%
  tibble::column_to_rownames(.,var = "V101_01") 

sop_pagree<- irr::agree(sop,tolerance = 0)

sop_pagree
