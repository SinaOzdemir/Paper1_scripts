###################################################################
# Title: Subject of publicity  indicators                         #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 08/02/2022                                                #
###################################################################


# Setup -------------------------------------------------------------------


library(pacman)

packs<- c("tidyverse","here","patchwork","igraph","ggraph","quanteda","ggrepel")

p_load(char = packs)

graphs_path<- here("Graphs")

data_path<- here("Data","Analysis_data")

data<- readRDS(file = file.path(data_path,"ml_analysis_data.RDS")) %>% 
  mutate(Actor_type = recode(Actor_type, "Commissioner"="Comissioner"))


# SOP frequency analysis -----------------------------------------

##TODO: can't find a way to show who other actors are with a publication quality graph....

sop_data<- data %>%
  select(screen_name,
         Actor_type,
         status_id,
         V201_subject_of_publicity)


sop_summary<- sop_data %>% 
  group_by(V201_subject_of_publicity) %>% 
  summarise(sop_count = n()/nrow(sop_data))

actor_type<- sop_data %>% 
  group_by(Actor_type) %>% 
  summarise(a_tweet = n())

actor_type_sop_summary<- sop_data %>%
  group_by(Actor_type,V201_subject_of_publicity) %>% 
  summarise(sop_count = n()) %>% 
  left_join(.,actor_type,by = "Actor_type") %>% 
  mutate(sop_perc = round(sop_count/a_tweet,2)) %>% 
  mutate(label_y = cumsum(sop_perc) - 0.5 * sop_perc)

# sop_graph<- actor_type_sop_summary %>% 
#   ggplot(aes(x = reorder(as.factor(V201_subject_of_publicity),-sop_perc), y= sop_perc))+
#   geom_bar(aes(fill = sop_perc),stat="identity",position = "dodge")+
#   scale_fill_continuous(name = "Percentage")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=45,vjust=.7))+
#   labs(x = "Subjet of publicity",y = "percentage")+
#   facet_wrap(~Actor_type)

sop_max_actor_graph<- actor_type_sop_summary %>% 
  group_by(Actor_type,V201_subject_of_publicity) %>%
  slice_max(order_by = sop_perc,n = 2) %>% 
  ggplot(aes(x = Actor_type, y = sop_perc))+
  geom_bar(aes(fill = V201_subject_of_publicity), stat = "identity", position = "dodge")+
  guides(fill = guide_legend(title = "Subject of Publicity"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = .5))+
  labs(x = "Executive type", y = "Percentage")

sop_max_graph<- actor_type_sop_summary %>%
  group_by(V201_subject_of_publicity) %>% 
  summarise(sop_count = sum(sop_count),
            tweet_count = sum(a_tweet)) %>% 
  mutate(sop_perc = round((sop_count/tweet_count),2)) %>% 
  ggplot(aes(x = reorder(V201_subject_of_publicity,-sop_perc), y = sop_perc))+
  geom_bar(aes(fill = sop_perc),stat = "identity",position = "dodge",show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))+
  labs(x = "Subject of publicity", y = "Percentage")

figure_2 <- (sop_max_graph + sop_max_actor_graph)

# SOP-OOP analysis --------------------------------------------------------

actor_type_message_n <- data %>%
  group_by(Actor_type,V201_subject_of_publicity) %>%
  summarise(tweet_n = n())

sop_oop<- data %>%
  group_by(Actor_type,V201_subject_of_publicity) %>% 
  summarise(across(V301_01_Identity_and_mandate:V301_06_Other,~sum(.x))) %>% 
  left_join(.,actor_type_message_n,by = c("Actor_type","V201_subject_of_publicity")) %>% 
  mutate(across(V301_01_Identity_and_mandate:V301_06_Other,~round((.x/tweet_n),2))) %>% 
  pivot_longer(cols = V301_01_Identity_and_mandate:V301_06_Other,names_to = "variables",values_to = "values")


sop_oop %>% 
  ggplot(aes(x=V201_subject_of_publicity,y= values))+
  geom_bar(aes(fill = variables),stat = "identity",position = "stack")+
  theme_bw()+
  facet_wrap(~Actor_type)





# network analysis --------------------------------------------------------

sop_data$V202_01_Subjects<-NULL

sop_mention_data<- left_join(sop_data,data_text_long,by = "status_id")

sop_mention_data<- sop_mention_data %>% 
  filter(mentioned_actor != "") %>% 
  mutate(mentioned_actor = trimws(mentioned_actor)) %>% 
  mutate(mentioned_actor = str_remove_all(string = mentioned_actor, pattern = "@|#")) %>%
  mutate(mentioned_actor = tolower(mentioned_actor)) %>% 
  filter(!mentioned_actor%in%c("we","our","us","my","i"))

#further noise reduction dictionary
au<- c(african_union = "	
_africanunion",
       african_union = "african union",
       african_union = "africanunion")

eu<- c(the_eu = "eu",
       the_eu ="eufunds",
       the_eu = "european union",
       the_eu = "the eu")

com<-c(eu_commission = "commission",
       eu_commission = "eu-commission",
       eu_commission = "eu commission",
       eu_commission = "eu_commission",
       eu_commission = "european commission",
       eu_commission = "the commission",
       eu_commission = "the european commission")


sop_mention_data<- sop_mention_data %>%
  mutate(mentioned_actor = ifelse(.$mentioned_actor%in%au,"african_union",.$mentioned_actor)) %>% 
  mutate(mentioned_actor = ifelse(.$mentioned_actor%in%eu,"eu",.$mentioned_actor)) %>% 
  mutate(mentioned_actor = ifelse(.$mentioned_actor%in%com, "eu_commission",.$mentioned_actor))

sop_mention_data_summary<- sop_mention_data %>% 
  mutate(Actor_type = recode(Actor_type, "High representative and vice president" = "Comissioner")) %>% 
  filter(V201_subject_of_publicity %in%c("Other actors","Compound")) %>% 
  group_by(Actor_type,mentioned_actor) %>% 
  tally(name= "mention_freq") %>% 
  filter(mentioned_actor != "")

sop_mention_data_summary_a<- left_join(sop_mention_data_summary,actor_type,by = "Actor_type")

sop_mention_data_summary_a<- sop_mention_data_summary_a %>% 
  mutate(menion_perc = round((mention_freq/a_tweet),2))

sop_mention_table <-left_join(sop_mention_data_summary,actor_type,by = "Actor_type")  %>% 
  mutate(mentioned_actor = str_remove_all(string = mentioned_actor,pattern = "[[:punct:]]"),
         mentioned_actor = str_trim(string = mentioned_actor,side = "both")) %>% 
  group_by(Actor_type, mentioned_actor) %>% 
  summarise(mention_freq = sum(mention_freq)) %>% 
  left_join(.,sop_mention_data_summary_a %>% select(Actor_type,a_tweet),by = "Actor_type") %>% 
  mutate(mention_perc = round(mention_freq/a_tweet,2)) %>% 
  group_by(Actor_type) %>% 
  filter(mention_perc>0) %>% 
  distinct(Actor_type,mentioned_actor,.keep_all = T)
  
sop_aggregate_table <- sop_mention_table %>% 
  group_by(Actor_type) %>% 
  summarise(mentioned_actor = paste(mentioned_actor,collapse = ", ")) %>% 
  ungroup() %>% 
  as.data.frame()

xlsx::write.xlsx(x = sop_aggregate_table,file = here("Drafts","JEPP","graphs","actor_type_mention_table.xlsx"))

sop_mention_data_summary_top <- sop_mention_data_summary_a %>% 
  group_by(Actor_type) %>% 
  slice_max(order_by = menion_perc, n = 1)

sop_mention_data_summary_top %>% 
  ggplot(aes(x = reorder(Actor_type,menion_perc),y = menion_perc))+
  geom_bar(aes(fill = menion_perc), stat = "identity",position = "dodge",show.legend = F)+
  geom_text_repel(aes(label = mentioned_actor),nudge_y = 0.02,min.segment.length = 0,size = 4)+
  theme_bw()+
  labs(x = "mentioned actors", y = "percentage")+
  coord_flip()
 