# PR+ manual coding percentage agreement test             #
# Author: Sina Özdemir                                    #
#         sina.ozdemir@ntnu.no                            #
#         Department of sociology and political science   #
# Date: 19/02/2022                                        #
###########################################################


# setup -------------------------------------------------------------------


library(pacman)
packs<- c("tidyverse","irr","here")

p_load(char = packs)

sop_data<- readRDS(file = here("Data","sop_test_data.rds"))

oop_data<- readRDS(file = here("Data","oop_test_data.rds")) %>% 
  mutate(across(V301_01:V301_06,~as.factor(.x)))

pa_data<- readRDS(file = here("Data","pa_test_data.rds"))

graph_path<- here("graphs")

variable_names<- read.xlsx(file = here("Data","variable_names.xlsx"),sheetIndex = 1) %>% 
  filter(grepl(pattern = "^CI|V201|V30*|V40*",x = VAR))

tick_names<- variable_names %>% pull(LABEL) %>% set_names(variable_names$VAR)

## functions

p_agree_calculator<- function(data, test_variable, obs_id_col, coder_id_col){
  test_data<- data %>% 
    select({{obs_id_col}},{{coder_id_col}},{{test_variable}})
  
  test_data_w<- test_data %>%
    pivot_wider(id_cols = {{obs_id_col}},
                names_from = {{coder_id_col}},
                values_from = {{test_variable}}) %>%
    column_to_rownames(var = "V101_01")
  
  p_agreement<- irr::agree(test_data_w)
  
  return(p_agreement)
}



# SOP percentage agreement ------------------------------------------------


##All three coders
SOP_agree<- p_agree_calculator(data = sop_data,
                               test_variable = V201,
                               obs_id_col = V101_01,
                               coder_id_col = CI01)


SOP_sfo_kg <- sop_data %>%
  filter(CI01 == 1 | CI01 == 2) %>% 
  p_agree_calculator(data = .,
                     test_variable = V201,
                     obs_id_col = V101_01,
                     coder_id_col = CI01)


pagree_results<- tibble(all_coders = SOP_agree[["value"]],
                        sfo_kg = SOP_sfo_kg[["value"]]) %>% 
  pivot_longer(cols = everything(),names_to = "coders",values_to = "p_agreement") %>% 
  ggplot(aes(x = coders, y = p_agreement))+
  geom_bar(aes(fill = p_agreement),stat = "identity",position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Coders",y = "percentage agreement", title = "Percentage agreement for Subject of Publicity")

ggsave(filename = "sop_pagree.jpeg",plot = pagree_results,path = graph_path,bg = "white")


remove(list = ls(pattern = "sop_*"))
# OOP pagrees -------------------------------------------------------------


oop_vars<- grep(pattern = "V30*",x = colnames(oop_data),value = T)

oop_pagree_results<- list()

for (i in 1:length(oop_vars)) {
  
  oop_res<- p_agree_calculator(data = oop_data,
                               test_variable = sym(oop_vars[i]),
                               obs_id_col = V101_01,
                               coder_id_col = CI01)
  
  oop_pagree_results[[i]]<- oop_res
  
  names(oop_pagree_results)[i]<- oop_vars[i]
}

oop_collective_agreement<- tibble(variables = names(unlist(oop_pagree_results)),
                                  values = unlist(oop_pagree_results),
                                  coders = rep("all_coders",times = length(unlist(oop_pagree_results)))) %>% 
  filter(grepl(pattern = ".value",x = variables))


#---------

#----

oop_full_results<- oop_collective_agreement %>% 
  mutate(variables = str_remove_all(string = variables, pattern = ".value")) %>% 
  mutate(variables = recode(variables, !!!tick_names)) %>% 
  mutate(variables = str_remove_all(string= variables, pattern = "object_of_publicity: "))

oop_results_graph<- oop_full_results %>%
  mutate(values = as.numeric(values)) %>% 
  ggplot(aes(x = coders, y = values))+
  geom_bar(aes(fill = values),stat = "identity",position = "dodge")+
  geom_hline(yintercept = 80,color = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~variables)
  
ggsave(oop_results_graph,filename = "oop_pagree.jpeg",path = graph_path,bg = "white")

rm(list = ls(pattern = "oop_*"))
# PA percentage agreement -------------------------------------------------

pa_vars<- grep(pattern = "V40*",x = colnames(pa_data),value = T)


pa_agree_all<- list()

for (i in 1:length(pa_vars)) {
  
  pa_agreement<- p_agree_calculator(data = pa_data,
                                    test_variable = sym(pa_vars[i]),
                                    obs_id_col = V101_01,
                                    coder_id_col = CI01)
  
  pa_agree_all[[i]]<- pa_agreement
  
  names(pa_agree_all)[i]<- pa_vars[i]
 
  
    
}


pa_agreement_df_ls<- ls(pattern = "pa_agree_")

pa_agreement_df<- tibble()

for (i in 1:length(pa_agreement_df_ls)) {
  
  ls<- get(pa_agreement_df_ls[i])
  
  ls_unlist<- unlist(ls)
  
  ls_df<- tibble(variables = names(ls_unlist),
                 values = ls_unlist,
                 coders = rep(gsub(pattern = "pa_agree_",replacement = "",pa_agreement_df_ls[i]),times = length(ls_unlist)))
  
  pa_agreement_df<- rbind(pa_agreement_df,ls_df)
}


pa_agree_graph<- pa_agreement_df %>% 
  filter(grepl(pattern =".value",x = variables)) %>%
  mutate(variables = str_remove_all(string = variables,pattern = ".value")) %>% 
  mutate(variables = recode(variables, !!!tick_names)) %>% 
  mutate(variables = str_remove_all(string = variables, pattern = "policy_area: ")) %>% 
  mutate(values = as.numeric(values)) %>% 
  ggplot(aes(x= coders,y = values))+
  geom_bar(aes(fill = values),stat= "identity",position = "dodge")+
  geom_hline(yintercept = 80,color = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "coders",y = "percentage agreement",title = "Policy area percentage agreement")+
  facet_wrap(~variables)

ggsave(pa_agree_graph,filename = "policy_area_agreement.jpeg",path = graph_path,bg = "white")

rm(list = ls())
