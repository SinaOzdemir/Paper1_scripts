### ML performance reporting


# setup -------------------------------------------------------------------

library(pacman)

packs<-c("here","tidyverse")

models_list<- list.files(path = here("final version","models"),pattern = "*.Rdata",full.names = T)
variables<- list.files(path = here("final version","models"),pattern = "*.Rdata",full.names = F) %>% gsub("_models.Rdata","",.)



# model performances ------------------------------------------------------


model_performances<- data.frame()

for (i in 1:length(models_list)) {
  load(models_list[i])
  model_names<- names(ml_performance)
  ml_perf<- ml_performance %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "metrics") %>% 
    pivot_longer(cols = all_of(model_names),names_to = "models",values_to = "scores") %>% 
    mutate(variable = variables[i])
  
  model_performances<- rbind(model_performances,ml_perf)
}


# v201 other actors -------------------------------------------------------

ml_perf<- readRDS(file = here("final version","models","V201_otheract_performance.RDS")) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "metrics") %>% 
  pivot_longer(cols = all_of(model_names),names_to = "models",values_to = "scores") %>% 
  mutate(variable = "V201_other_actors")


model_performances<- rbind(model_performances,ml_perf)

performance_table<- model_performances %>% 
  filter(metrics %in%c("Precision","Recall","F1")) %>%
  pivot_wider(names_from = "metrics",values_from = "scores") %>% 
  group_by(variable) %>% 
  filter(F1 == max(F1,na.rm = T)) %>% 
  ungroup()

xlsx::write.xlsx(x = performance_table,file = here("final version","model_performances.xlsx"))
