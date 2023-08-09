# PR+ manual coding test 2 closer examination             #
# Author: Sina Özdemir                                    #
#         sina.ozdemir@ntnu.no                            #
#         Department of sociology and political science   #
# Date: 22/02/2022                                        #
###########################################################



# setup -------------------------------------------------------------------


library(pacman)
packs<- c("tidyverse","irr","here","xlsx")

p_load(char = packs)

sop_data<- readRDS(file = here("sop_test_data.rds")) %>%
  mutate(CI01 = recode(CI01, `1` = "sfo",`2`= "kg",`3` = "pdw"))

oop_data<- readRDS(file = here("oop_test_data.rds")) %>% 
  mutate(across(V301_01:V301_06,~as.factor(.x)))%>%
  mutate(CI01 = recode(CI01, `1` = "sfo",`2`= "kg",`3` = "pdw"))


pa_data<<- readRDS(file = here("pa_test_data.rds"))%>%
  mutate(CI01 = recode(CI01, `1` = "sfo",`2`= "kg",`3` = "pdw"))


graph_path<- here("graphs")

twt_data<- read.xlsx(file = "data_PR_plus_2022-02-19_13-19.xlsx",sheetIndex = 1) %>% 
  select(V101_01,V101_02) %>% filter(!duplicated(V101_01))


# difference in sop_coding ------------------------------------------------


sop_diff_dt<- sop_data %>%
  pivot_wider(names_from = CI01,values_from = V201) %>% 
  drop_na() %>% 
  mutate(sfo_kg_diff = ifelse(sfo == kg, 1,0),
         sfo_pdw_diff = ifelse(sfo == pdw, 1,0),
         kg_pdw_diff = ifelse(kg == pdw,1,0),
         all_coder_diff = ifelse(sfo == kg & sfo == pdw & kg == pdw, 1,0))


sop_diff_dt %>% 
  filter(sfo_kg_diff == 0) %>% 
  select(V101_01,sfo,kg) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","sop_sfo_kg_diff.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



sop_diff_dt %>% 
  filter(sfo_pdw_diff == 0) %>% 
  select(V101_01,sfo,pdw) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","sop_sfo_pdw_diff.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)


sop_diff_dt %>% 
  filter(kg_pdw_diff == 0) %>% 
  select(V101_01,kg,pdw) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","sop_kg_pdw_diff.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)

# difference in oop coding ------------------------------------------------



oop_coding<-  oop_data %>% 
  filter(CI01 %in% c("sfo","kg")) %>% 
  pivot_wider(names_from = CI01,values_from = V301_01:V301_06)


oop_coding<- oop_coding %>% 
  mutate(V301_01_diff = ifelse(V301_01_kg == V301_01_sfo,1,0),
         V301_02_diff = ifelse(V301_02_kg == V301_02_sfo,1,0),
         V301_03_diff = ifelse(V301_03_kg == V301_03_sfo,1,0),
         V301_04_diff = ifelse(V301_04_kg == V301_04_sfo,1,0),
         V301_05_diff = ifelse(V301_05_kg == V301_05_sfo,1,0),
         V301_06_diff = ifelse(V301_06_kg == V301_06_sfo,1,0)) %>% 
  drop_na()

oop_coding %>% 
  filter(V301_01_diff == 0) %>% 
  select(V101_01,V301_01_kg,V301_01_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","301_01_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



oop_coding %>% 
  filter(V301_02_diff == 0) %>% 
  select(V101_01,V301_02_kg,V301_02_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","301_02_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



oop_coding %>% 
  filter(V301_03_diff == 0) %>% 
  select(V101_01,V301_03_kg,V301_03_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","301_03_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)


oop_coding %>% 
  filter(V301_04_diff == 0) %>% 
  select(V101_01,V301_04_kg,V301_04_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","301_04_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



oop_coding %>% 
  filter(V301_05_diff == 0) %>% 
  select(V101_01,V301_05_kg,V301_05_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","301_05_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



oop_coding %>% 
  filter(V301_06_diff == 0) %>% 
  select(V101_01,V301_06_kg,V301_06_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","301_06_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



# Policy area -------------------------------------------------------------


# Categories:
    # Agriculture and fisheries
    # Economic and financial affairs
    # General affairs
    # None
    # Other


# Data prep:

pa_data_w<- pa_data %>%
  filter(CI01 %in% c("sfo","kg")) %>% 
  drop_na() %>%
  select(CI01,V101_01,V401_01,V401_03,V401_07,V401_11,V401_12) %>%
  pivot_wider(id_cols = V101_01,
              names_from = CI01,
              names_sep = "_",
              values_from = V401_01:V401_12,
              values_fill = "2")

pa_data_w<- pa_data_w %>% 
  mutate(V401_01_diff = ifelse(V401_01_kg == V401_01_sfo,1,0),
         V401_03_diff = ifelse(V401_03_kg == V401_03_sfo,1,0),
         V401_07_diff = ifelse(V401_07_kg == V401_07_sfo,1,0),
         V401_11_diff = ifelse(V401_11_kg == V401_11_sfo,1,0),
         V401_12_diff = ifelse(V401_12_kg == V401_12_sfo,1,0))



pa_data_w %>% 
  filter(V401_01_diff == 0) %>% 
  select(V101_01,V401_01_kg,V401_01_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","401_01_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)


pa_data_w %>% 
  filter(V401_03_diff == 0) %>% 
  select(V101_01,V401_03_kg,V401_03_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","401_03_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)


pa_data_w %>% 
  filter(V401_07_diff == 0) %>% 
  select(V101_01,V401_07_kg,V401_07_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","401_07_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)



pa_data_w %>% 
  filter(V401_11_diff == 0) %>% 
  select(V101_01,V401_11_kg,V401_11_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","401_11_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)




pa_data_w %>% 
  filter(V401_12_diff == 0) %>% 
  select(V101_01,V401_12_kg,V401_12_sfo) %>%
  left_join(.,twt_data, by = "V101_01") %>%
  mutate(V101_01 = paste0("x",V101_01)) %>% 
  write.table(x = .,
              file = here("qualitative analysis","401_12_dif_analysis.csv"),
              sep = ",",fileEncoding = "UTF-8",row.names = F,col.names = T)

