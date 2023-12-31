###################################################################
# Title: One way communication indicators                         #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 08/02/2022                                                #
###################################################################


# Setup -------------------------------------------------------------------


library(pacman)

packs<- c("tidyverse","here","patchwork","sjlabelled","ggrepel","viridis","patchwork","hrbrthemes","circlize","networkD3")

p_load(char = packs)

graphs_path<- here("Graphs")

data_path<- here("Data","Analysis_data")

data<- readRDS(file = file.path(data_path,"analysis_data_11042022.rds")) %>% 
  mutate(across(V301_01_Identity_and_mandate:V401_12_None,~as.integer(as.character(.x)))) %>% 
  mutate(Actor_type = replace_na(Actor_type, "Agency"),
         is_reply = replace_na(is_reply, 0)) %>% 
  mutate(Actor_type = recode(Actor_type, "High representative and vice president" = "Comissioner"))


# Overall Executive communication -----------------------------------------

overall_oop_freqs<- data %>%
  select(screen_name,Actor_type,status_id,matches(match = "V301_*"),is_reply) %>% 
  summarise(iam_perc = (sum(V301_01_Identity_and_mandate)/n()),
            output_perc = (sum(V301_02_Output)/n()),
            activity_perc = (sum(V301_03_Activity)/n()),
            opinion_perc = (sum(V301_04_Opinion)/n()),
            input_perc = (sum(V301_05_Input_seeking)/n()),
            other_perc = (sum(V301_06_Other)/n()),
            reply_perc = (sum(is_reply)/n())) %>% 
  mutate(across(iam_perc:other_perc, ~round(.x,2))) %>% 
  pivot_longer(cols = iam_perc:reply_perc,names_to = "object_type",values_to = "percentage") %>% 
  mutate(com_strat = ifelse(object_type %in% c("iam_perc","output_perc","activity_perc","opinion_perc","other_perc"),"one_way",
                            ifelse(object_type == "input_perc","two_way_asymetric","two_way_symetric"))) 
  
overall_oop_plot<- overall_oop_freqs %>%
  ggplot(aes(x = reorder(object_type,-percentage),y = percentage,fill = com_strat))+
  geom_bar(position = "dodge",stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  scale_fill_brewer(palette = "Dark2",
                    labels = c("One-way","Two-way asymmetric","Two-way symmetric"),
                    name = "Communication strategies")+
  scale_x_discrete(labels = c("output","activity","opinion","other","reply","mandate","input seeking"))+
  labs(x = "Object of communication", y= "Percentage")


ggsave(filename = "overall_oop_plot.jpeg",
       plot = overall_oop_plot,
       path = graphs_path,
       bg = "white",
       width = 5,
       height = 6,
       units = "in")


actor_oop_freqs<- data %>%
  select(screen_name,Actor_type,status_id,matches(match = "V301_*"),is_reply) %>% 
  group_by(Actor_type) %>% 
  summarise(iam_perc = (sum(V301_01_Identity_and_mandate)/n()),
            output_perc = (sum(V301_02_Output)/n()),
            activity_perc = (sum(V301_03_Activity)/n()),
            opinion_perc = (sum(V301_04_Opinion)/n()),
            input_perc = (sum(V301_05_Input_seeking)/n()),
            other_perc = (sum(V301_06_Other)/n()),
            reply_perc = (sum(is_reply)/n())) %>% 
  pivot_longer(cols = iam_perc:reply_perc,names_to = "object_type",values_to = "percentage") %>% 
  mutate(com_strat = ifelse(object_type %in% c("iam_perc","output_perc","activity_perc","opinion_perc","other_perc"),"one_way",
                            ifelse(object_type == "input_perc","two_way_asymetric","two_way_symetric")))


actor_oop_point<- actor_oop_freqs %>%
  mutate(object_type = str_remove_all(string = object_type,"_perc")) %>% 
  mutate(object_type = recode(object_type, "iam" = "Identity & Mandate",
                              "output" = "Output",
                              "activity" = "Activity",
                              "opinion" = "Opinion",
                              "input" = "Input seeking",
                              "reply" = "Reply to others",
                              "other" = "Other")) %>% 
  mutate(percentage = round(percentage, 2)) %>% 
  ggplot(aes(x=object_type,y=percentage))+
  geom_bar(aes(fill = percentage),stat ="identity", position = "dodge",show.legend = F)+
  theme_bw()+
  coord_flip()+
  labs(x = "Object of Publicity", y = "Percentage")+
  facet_wrap(~Actor_type)
  
ggsave(filename = "actor_oop_point.jpeg",
       plot = actor_oop_point,
       path = graphs_path,
       bg = "white",
       width = 5,
       height = 6,
       units = "in")

rm(list = ls())
