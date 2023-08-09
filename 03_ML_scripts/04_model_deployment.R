#########################################################
# Title:  Model deployment for paper 2                  #
# Author: Sina Özdemir                                  #
#         PhD candidate                                 #
#         Department of sociology and political science #
#         sina.ozdemir@ntnu.no                          #
#########################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","caret")

p_load(char = packs)

prediction_data<- readRDS(file = here("final version","data","na_dropped_embedded_prediction_data.RDS"))

model_performances<- readxl::read_excel(path = here("final version","model_performances.xlsx"),sheet = 1)

variable<- model_performances %>% pull(variable)

model<- model_performances %>% pull(models)

model_path<- list.files(path = here("final version","models"),pattern = "*.Rdata",full.names = T)
# prediction --------------------------------------------------------------

predicted_data<-prediction_data %>% select(screen_name,status_id)

for (i in 1:length(variable)) {
  model_to_load<- grep(pattern = variable[i],x = model_path,value = T)
  
  load(model_to_load)
  
  model_to_use<- ml_models[[model[i]]]
  
  predicted_data[[variable[i]]]<- predict(model_to_use,prediction_data[,3:102],type = "raw")
}

other_actors_model<- readRDS(file=here("final version","models","V201_otheract_models.RDS"))[["xgbTree"]]

predicted_data[["V201_other_actors"]]<-predict(other_actors_model,prediction_data[,3:102],type = "raw")

predicted_data[["V201_Compound"]]<- ifelse(predicted_data$V201_none == "no" & predicted_data$V201_Self == "no" & predicted_data$V201_other_actors == "no" ,"yes","no")

problematic_observations_a<- predicted_data %>% 
  filter(V201_Compound == "no" & V201_Self == "yes" & V201_other_actors == "yes" & V201_none == "yes") %>% 
  pull(status_id)

problematic_observations_b<- predicted_data %>% 
  filter(V201_Compound == "no" & V201_Self == "no" & V201_other_actors == "yes" & V201_none == "yes")%>% 
  pull(status_id)

problematic_observations_c<- predicted_data %>% 
  filter(V201_Compound == "no" & V201_Self == "yes" & V201_other_actors == "no" & V201_none == "yes")%>% 
  pull(status_id)


problematic_observations_d<- predicted_data %>% 
  filter(V201_Compound == "no" & V201_Self == "yes" & V201_other_actors == "yes" & V201_none == "no")%>% 
  pull(status_id)

problematic_observations<- unique(c(problematic_observations_a,problematic_observations_b,problematic_observations_c,problematic_observations_d))

clean_predicted_data<- predicted_data %>% 
  filter(!(status_id%in%problematic_observations))


# manual coding alingment -------------------------------------------------


manually_coded_data<- readRDS(file = here("final version","data","analysis_data_11042022.rds")) %>% 
  select(screen_name,status_id,V201_subject_of_publicity,V301_01_Identity_and_mandate:V301_06_Other,is_reply,Actor_type) %>% 
  mutate(screen_name = tolower(screen_name))

actor_type<- manually_coded_data %>% select(screen_name,Actor_type) %>% 
  mutate(Actor_type = recode(Actor_type,"High representative and vice president" = "Commissioner")) %>% 
  distinct(screen_name,.keep_all = T)

V201_dummy<- as.data.frame(psych::dummy.code(manually_coded_data$V201_subject_of_publicity)) %>% 
  mutate(across(everything(),~as.factor(recode(.x,`0` = "no",`1`="yes"))))

colnames(V201_dummy)<-c("V201_other_actors","V201_Self", "V201_Compound"   ,"V201_none")

manually_coded_data<-manually_coded_data %>% select(-V201_subject_of_publicity,-Actor_type) %>% cbind(.,V201_dummy) 

manually_coded_data<- manually_coded_data %>% 
  mutate(across(V301_01_Identity_and_mandate:V301_06_Other,~as.factor(recode(.x,`0` = "no", `1` = "yes"))))%>% 
  mutate(is_reply = as.factor(ifelse(is_reply == T, "yes","no")))

full_data <- readRDS(file = here("final version","data","eu_data_011219_310720.RDS")) %>% 
  filter(status_id %in%clean_predicted_data$status_id) %>% 
  mutate(is_reply = as.factor(ifelse(!is.na(reply_to_screen_name),"yes","no"))) %>% 
  select(screen_name,status_id,is_reply) %>% 
  mutate(screen_name = tolower(screen_name))

clean_predicted_data_plus<-left_join(clean_predicted_data,full_data, by = c("screen_name","status_id"))  

clean_predicted_data_plus_plus<- rbind(clean_predicted_data_plus,manually_coded_data)

clean_predicted_data_plus_plus_plus<- left_join(clean_predicted_data_plus_plus,actor_type, by = "screen_name")

clean_predicted_data_plus_plus_plus<-
  clean_predicted_data_plus_plus_plus %>% 
  mutate(V201_subject_of_publicity = ifelse(V201_Self == "yes","Self",
                                            ifelse(V201_Compound == "yes","Compound",
                                                   ifelse(V201_other_actors == "yes","Other actors",
                                                          ifelse(V201_none == "yes","None","problem")))))

final_clean_predicted_data<- clean_predicted_data_plus_plus_plus %>% select(-V201_none,-V201_Self,-V201_other_actors,-V201_Compound)


saveRDS(final_clean_predicted_data, file = here("final version","data","ml_analysis_data.RDS"))
