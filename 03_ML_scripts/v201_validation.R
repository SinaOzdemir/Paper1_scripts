# V201 validation:
library(pacman)

packs<- c("tidyverse","here","caret")

p_load(char = packs)


# other actors deployment -------------------------------------------------

data<- readRDS(file = here("final_dataset.RDS"))
model_performance<- readRDS(file = here("final_models","initial attempt","otheract_na_dropped_embedding_models_performance.RDS"))
models<- readRDS(file = here("final_models", "initial attempt","otheract_na_dropped_embedding_models.RDS"))
prediction_data<- readRDS(file =here("data","na_dropped_embedded_prediction_data.RDS"))

## model selection ----

model_performance<- as.data.frame(model_performance) %>%
  rownames_to_column(var = "metric") %>% 
  pivot_longer(cols = glmnet:xgbTree, names_to  = "model", values_to = "score")

model_performance %>%
  filter(metric %in% c("Precision","Recall","F1")) %>% 
  ggplot(aes(x = metric, y = score))+
  geom_bar(aes(fill = metric),stat = "identity",position= "dodge")+
  geom_hline(yintercept = .7, color = "red")+
  facet_wrap(~model)

#Recall: when the model says positive, how many of them are true positives
#Precision: how many of the true positives are identified as true positives.
## when other actor model classifies something as positive they tend to be positive.
## However, models have hard time identifying all positives. That is 
## Some of the positive outcomes are classified as negative by models.

# model deploy ------------------------------------------------------------


best_model<- model_performance %>% 
  filter(metric  == "F1") %>%
  slice_max(order_by = score, n = 1) %>% 
  pull(model)

model<- models[[best_model]]

other_actors<- prediction_data %>% 
  select(screen_name,status_id) %>% 
  mutate(V201_other_actors = predict(model, prediction_data[,3:102],type = "raw"))

data_pl<- left_join(data,other_actors,
                    by = c("screen_name_l" = "screen_name", "status_id")) %>% 
  rename(screen_name = screen_name_l) %>% 
  drop_na()

a<- data_pl %>%
  filter(V201_Self == "no") %>% 
  filter(V201_none == "no") %>% 
  filter(V201_other_actors == "no") %>% 
  mutate(V201_Compound = "yes") %>% 
  select(screen_name,status_id, V201_Compound)

data_pl<- left_join(data_pl, a, by = c("screen_name","status_id")) %>% 
  mutate(V201_Compound = replace_na(V201_Compound,"no")) %>% 
  mutate(V201_Compound = as.factor(V201_Compound))

summary<- data_pl %>% 
  group_by(V201_none,V201_Self,V201_other_actors, V201_Compound) %>% 
  tally() %>% 
  mutate(perc = n/nrow(data_pl)) %>%
  pivot_longer(cols = V201_none:V201_Compound, names_to = "variables", values_to = "values")

analysis_data<- readRDS(file = here("data","analysis_data_11042022.rds"))%>%
  mutate(V201_subject_of_publicity = recode(V201_subject_of_publicity,"Other actors" = "other_actors")) 

analysis_summary<- psych::dummy.code(analysis_data$V201_subject_of_publicity) %>% 
  as.data.frame() %>% 
  mutate(across(everything(),~as.factor(recode(.x,`1` = "yes",`0`="no")))) %>% 
  rename(V201_none = None,
         V201_Self = Self,
         V201_other_actors = other_actors,
         V201_Compound = Compound) %>% 
  group_by(V201_none,V201_Self,V201_other_actors,V201_Compound) %>% 
  tally() %>% 
  mutate(perc = n/nrow(analysis_data)) %>% 
  pivot_longer(V201_none:V201_Compound,names_to = "variables", values_to = "values")

summaries<- left_join(summary,analysis_summary,by = c("variables","values")) %>% 
  mutate(training_sample_n = nrow(analysis_data)) %>% 
  mutate(population_n = nrow(data_pl)) %>% 
  rename(in_population_n = n.x,
         in_sample_n = n.y,
         in_population_perc = perc.x,
         in_sample_perc = perc.y)

differences<- summaries %>%
  select(variables,values,in_population_perc,in_sample_perc) %>% 
  mutate(diff = in_sample_perc-in_population_perc)


# reverse dummy coding ----------------------------------------------------


data_pl$V201_subject_of_publicity<- ifelse(data_pl$V201_Self == "yes"& data_pl$V201_Compound == "no"& data_pl$V201_other_actors == "no" & data_pl$V201_none == "no","Self",
                                           ifelse(data_pl$V201_Self == "no"& data_pl$V201_Compound == "yes"& data_pl$V201_other_actors == "no" & data_pl$V201_none == "no","Compound",
                                                  ifelse(data_pl$V201_Self == "no"& data_pl$V201_Compound == "no"& data_pl$V201_other_actors == "yes" & data_pl$V201_none == "no","Other_actors",
                                                         ifelse(data_pl$V201_Self == "no"& data_pl$V201_Compound == "no"& data_pl$V201_other_actors == "no" & data_pl$V201_none == "yes","None","Mistake"))))

v201_predictions<- data_pl %>% select(screen_name, status_id,V201_subject_of_publicity)


v201_prediction_mistakes<- v201_predictions %>% 
  group_by(V201_subject_of_publicity) %>% 
  tally() %>% 
  mutate(perc = n/nrow(v201_predictions))



# precision recall of models ----------------------------------------------



all_models %>% 
  filter(metric == "F1") %>% 
  ggplot(aes(x = model_name, y= score))+
  geom_bar(aes(fill = score),stat = "identity", position = "dodge")+
  geom_hline(yintercept = .7,color = "red")+
  labs(title = "F1 score of wikipedia2vec embedding models",
       subtitle = "precision: how many of the retrieved documents are relevant\nrecall: how many of the relevant documents are retrieved\n F1: harmonic mean of Precision and Recall")+
  theme_bw()+
  facet_wrap(~variable)

all_models %>% 
  filter(metric == "Precision") %>% 
  ggplot(aes(x = model_name, y= score))+
  geom_bar(aes(fill = score),stat = "identity", position = "dodge")+
  geom_hline(yintercept = .7,color = "red")+
  labs(title = "Precision score of wikipedia2vec embedding models",
       subtitle = "precision: how many of the retrieved documents are relevant\nrecall: how many of the relevant documents are retrieved\n F1: harmonic mean of Precision and Recall")+
  theme_bw()+
  facet_wrap(~variable)


all_models %>% 
  filter(metric == "Recall") %>% 
  ggplot(aes(x = model_name, y= score))+
  geom_bar(aes(fill = score),stat = "identity", position = "dodge")+
  geom_hline(yintercept = .7,color = "red")+
  labs(title = "Recall score of wikipedia2vec embedding models",
       subtitle = "precision: how many of the retrieved documents are relevant\nrecall: how many of the relevant documents are retrieved\n F1: harmonic mean of Precision and Recall")+
  theme_bw()+
  facet_wrap(~variable)

## SO in general, when models classifies something as positive they tend to be positive.
## However, models have hard time identifying all positives. That is 
## Some of the positive outcomes are classified as negative by models.
#Recall: when the model says positive, how many of them are true positives
#Precision: how many of the true positives are identified as true positives.


# interrater agreement: man vs machine ------------------------------------


