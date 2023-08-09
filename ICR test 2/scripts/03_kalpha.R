# PR+ manual coding krippendorf alpha test                #
# Author: Sina Özdemir                                    #
#         sina.ozdemir@ntnu.no                            #
#         Department of sociology and political science   #
# Date: 19/02/2022                                        #
###########################################################


library(pacman)
packs<- c("tidyverse","irr","here")

p_load(char = packs)

sop_data<- readRDS(file = here("data","sop_test_data.rds")) %>% 
  mutate(CI01 = recode(CI01, `1` = "sfo",`2`= "kg",`3` = "pdw"))


oop_data<- readRDS(file = here("data","oop_test_data.rds")) %>% 
  mutate(across(V301_01:V301_06,~as.factor(.x))) %>% 
  mutate(CI01 = recode(CI01, `1` = "sfo",`2`= "kg",`3` = "pdw"))


pa_data<<- readRDS(file = here("data","pa_test_data.rds")) %>% 
  mutate(CI01 = recode(CI01, `1` = "sfo",`2`= "kg",`3` = "pdw"))


graph_path<- here("graphs")

variable_names<- readxl::read_excel(path = here("data","variable_names.xlsx"),sheet = 1) %>% 
  filter(grepl(pattern = "^CI|V201|V30*|V40*",x = VAR))

tick_names<- variable_names %>% pull(LABEL) %>% set_names(variable_names$VAR)


## functions


kalpha_calculator<- function(data, test_variable, obs_id_col, coder_id_col){
  
  test_data<- data %>% 
    select({{obs_id_col}},{{coder_id_col}},{{test_variable}})
  
  kalpha_mtrx<- test_data %>%
    pivot_wider(names_from = {{obs_id_col}},
                values_from = {{test_variable}}) %>% 
    column_to_rownames(var = "CI01") %>% 
    mutate(across(everything(),~as.numeric(.x))) %>% 
    as.matrix()
  
  kalpha<-irr::kripp.alpha(x = kalpha_mtrx,method = "nominal")[["value"]]
  
  names(kalpha)<-paste(rownames(kalpha_mtrx),collapse = "_")
  
  return(kalpha)
}


# SOP krippendorf alpha ---------------------------------------------------

sop_ka_all<- kalpha_calculator(data = sop_data,
                               test_variable = V201,
                               obs_id_col = V101_01,
                               coder_id_col = CI01)

sop_ka_sfo_kg<- sop_data %>%
  filter(CI01 == "sfo" | CI01 == "kg") %>%
  kalpha_calculator(data = .,
                    test_variable = V201,
                    obs_id_col = V101_01,
                    coder_id_col = CI01)

sop_ka_sfo_pdw<- sop_data %>%
  filter(CI01 == "sfo" | CI01 == "pdw") %>%
  kalpha_calculator(data = .,
                    test_variable = V201,
                    obs_id_col = V101_01,
                    coder_id_col = CI01)

sop_ka_kg_pdw<- sop_data %>%
  filter(CI01 == "kg" | CI01 == "pdw") %>%
  kalpha_calculator(data = .,
                    test_variable = V201,
                    obs_id_col = V101_01,
                    coder_id_col = CI01)

sop_ka_results<- c(sop_ka_all,sop_ka_sfo_kg,sop_ka_sfo_pdw,sop_ka_kg_pdw) %>%
  as_tibble(.,rownames = "coders") %>% 
  mutate(value = as.numeric(value))


sop_ka_graph<-sop_ka_results %>% 
  mutate(coders = recode(coders, "pdw_kg_sfo" = "all coders","kg_sfo" = "coder 1 & 2","pdw_sfo" = "coder 1 & 3", "pdw_kg" = "coder 2 & 3")) %>% 
  ggplot(aes(x = coders,y = value))+
  geom_bar(aes(fill = value),stat = "identity",position = "dodge")+
  geom_hline(yintercept = .67, color = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "coders",y = "krippendorf's alpha", title = "Subject of publicity krippendorf alpha",subtitle = "the red line is at .67")

ggsave(filename = "sop_kalpha.jpeg",plot = sop_ka_graph,path = graph_path,bg = "white")
# OOP krippendorf alpha ---------------------------------------------------


oop_vars<- grep(pattern = "V30*",x = colnames(oop_data),value = T)

oop_ka_all_coder<- list()
for (i in 1:length(oop_vars)) {
  
  oop_ka_all_coder[[i]]<- kalpha_calculator(data = oop_data,
                                            test_variable = sym(oop_vars[i]),
                                            obs_id_col = V101_01,
                                            coder_id_col = CI01)
  names(oop_ka_all_coder)[i]<- oop_vars[i]
  
  
}



oop_ka_sfo_kg<- list()
for (i in 1:length(oop_vars)) {
  
  oop_ka_sfo_kg[[i]]<- oop_data %>%
    filter(CI01 == "sfo"| CI01 == "kg") %>%
    kalpha_calculator(data = .,
                      test_variable = sym(oop_vars[i]),
                      obs_id_col = V101_01,
                      coder_id_col = CI01)
  
  names(oop_ka_sfo_kg)[i]<- oop_vars[i]
  
  
}



oop_ka_sfo_pdw<- list()
for (i in 1:length(oop_vars)) {
  
  oop_ka_sfo_pdw[[i]]<- oop_data %>%
    filter(CI01 == "sfo"| CI01 == "pdw") %>%
    kalpha_calculator(data = .,
                      test_variable = sym(oop_vars[i]),
                      obs_id_col = V101_01,
                      coder_id_col = CI01)
  
  names(oop_ka_sfo_pdw)[i]<- oop_vars[i]
  
  
}



oop_ka_kg_pdw<- list()
for (i in 1:length(oop_vars)) {
  
  oop_ka_kg_pdw[[i]]<- oop_data %>%
    filter(CI01 == "kg"| CI01 == "pdw") %>%
    kalpha_calculator(data = .,
                      test_variable = sym(oop_vars[i]),
                      obs_id_col = V101_01,
                      coder_id_col = CI01)
  
  names(oop_ka_kg_pdw)[i]<- oop_vars[i]
  
  
}


oop_results<- tibble()

oop_results_obj<- ls(pattern = "oop_ka_")

for (i in 1:length(oop_results_obj)) {
  
  oop_rs<- get(oop_results_obj[i]) %>%
    unlist()
  
  oop_rs_df<- tibble(variables = names(oop_rs),
                     values = oop_rs)
  
  oop_results<- rbind(oop_results,oop_rs_df)
  
}

oop_results_graph<- oop_results %>% 
  mutate(coders = str_remove_all(string = variables, pattern = "^V.+\\.")) %>% 
  mutate(variables = str_remove_all(string = variables, pattern = "\\..+")) %>% 
  mutate(variables = recode(variables, !!!tick_names)) %>% 
  mutate(variables = str_remove_all(variables,"object_of_publicity: "))


oop_graph<- oop_results_graph %>% 
  mutate(coders = recode(coders, "pdw_kg_sfo" = "all coders","kg_sfo" = "coder 1 & 2","pdw_sfo" = "coder 1 & 3", "pdw_kg" = "coder 2 & 3")) %>% 
  ggplot(aes(x = coders, y = values))+
  geom_bar(aes(fill = values),stat = "identity",position = "dodge")+
  geom_hline(yintercept = .67,color = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Coders", y= "Krippendorf alpha",title = "Object of publicity krippendorf alpha",subtitle = "Red line is at .67")+
  facet_wrap(~variables)

ggsave(filename = "oop_kalpha.jpeg",plot = oop_graph,path = graph_path,bg = "white")


# PA krippendorf alpha ----------------------------------------------------



pa_vars<- grep(pattern = "V40*",x = colnames(pa_data),value = T)

pa_ka_all_coder<- list()

for (i in 1:length(pa_vars)) {
  
  pa_ka_all_coder[[i]]<- kalpha_calculator(data = pa_data,
                                           test_variable = sym(pa_vars[i]),
                                           obs_id_col = V101_01,
                                           coder_id_col = CI01)
  names(pa_ka_all_coder)[i]<- pa_vars[i]
  
  
}



pa_ka_sfo_kg<- list()
for (i in 1:length(pa_vars)) {
  
  pa_ka_sfo_kg[[i]]<- pa_data %>%
    filter(CI01 == "sfo"| CI01 == "kg") %>%
    kalpha_calculator(data = .,
                      test_variable = sym(pa_vars[i]),
                      obs_id_col = V101_01,
                      coder_id_col = CI01)
  
  names(pa_ka_sfo_kg)[i]<- pa_vars[i]
  
  
}



pa_ka_sfo_pdw<- list()
for (i in 1:length(pa_vars)) {
  
  pa_ka_sfo_pdw[[i]]<- pa_data %>%
    filter(CI01 == "sfo"| CI01 == "pdw") %>%
    kalpha_calculator(data = .,
                      test_variable = sym(pa_vars[i]),
                      obs_id_col = V101_01,
                      coder_id_col = CI01)
  
  names(pa_ka_sfo_pdw)[i]<- pa_vars[i]
  
  
}



pa_ka_kg_pdw<- list()
for (i in 1:length(pa_vars)) {
  
  pa_ka_kg_pdw[[i]]<- pa_data %>%
    filter(CI01 == "kg"| CI01 == "pdw") %>%
    kalpha_calculator(data = .,
                      test_variable = sym(pa_vars[i]),
                      obs_id_col = V101_01,
                      coder_id_col = CI01)
  
  names(pa_ka_kg_pdw)[i]<- pa_vars[i]
  
  
}


pa_results<- tibble()

pa_results_obj<- ls(pattern = "pa_ka_")

for (i in 1:length(pa_results_obj)) {
  
  pa_rs<- get(pa_results_obj[i]) %>%
    unlist()
  
  pa_rs_df<- tibble(variables = names(pa_rs),
                    values = pa_rs)
  
  pa_results<- rbind(pa_results,pa_rs_df)
  
}

pa_results_graph<- pa_results %>% 
  mutate(coders = str_remove_all(string = variables, pattern = "^V.+\\.")) %>% 
  mutate(variables = str_remove_all(string = variables, pattern = "\\..+")) %>% 
  mutate(variables = recode(variables, !!!tick_names)) %>% 
  mutate(variables = str_remove_all(variables, "policy_area: "))


pa_graph<- pa_results_graph %>% 
  ggplot(aes(x = coders, y = values))+
  geom_bar(aes(fill = values),stat = "identity",position = "dodge")+
  geom_hline(yintercept = .67,color = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Coders", y= "Krippendorf alpha",title = "Policy area krippendorf alpha",subtitle = "Red line is at .67")+
  facet_wrap(~variables)

ggsave(filename = "pa_kalpha.jpeg",plot = pa_graph,path = graph_path,bg = "white")


rm(list = ls())
