# Identifying missing codings

library(pacman)

packs<- c("tidyverse","here","xlsx")

p_load(char = packs, install = T)

db<- read.csv(file = here("Analysis","databank_PR_plus_2022-05-17_09-59.csv"),
              sep = "\t",
              na.strings = c(""," ","NA"),
              fileEncoding = "UTF-16") %>% 
  drop_na()

coded_data<- xlsx::read.xlsx(file = here("Analysis","data_PR_plus_2022-05-17_09-58.xlsx"),sheetIndex = 1) %>% 
  filter(CI01!=3) %>% 
  drop_na(V101_02) %>% 
  mutate(STARTED = lubridate::ymd_hms(STARTED)) %>% 
  group_by(CI01,V101_01) %>% 
  filter(STARTED == max(STARTED))

#--------
coded_status<- coded_data %>% pull(V101_02) %>% str_split(string = .,pattern = "/",simplify = T)

coded_status<- coded_status[,6]

coded_data$status_id<- coded_status


#--------
coding_links<- read.csv(file = here("Analysis","Coding_links.csv")) %>% select(-ï..status_id)

coding_link_sid<- str_split(coding_links$status_link,pattern = "/",simplify = T)
coding_link_sid<- coding_link_sid[,6]
coding_links$status_id<- coding_link_sid
#--------

db_status_id<- db %>% pull(D0)

db_status_id<- str_split(db_status_id,pattern = "/",simplify = T)

db_status_id<- db_status_id[,6]

db$status_id<- db_status_id
#-----

#uncoded data in the db

uncoded_db<- db %>% filter(!(status_id%in%coded_data$status_id))

uncoded_data_links<- coding_links %>% filter(status_id%in%uncoded_db$status_id)

uncoded_db$match<- ifelse(uncoded_db$status_id==uncoded_data_links$status_id,T,F)


uncoded_db <- uncoded_db %>% filter(match ==T) %>% select(D0,D1,D2) %>% rename(twitter_link = D0, sosci_link = D1, coded = D2)

xlsx::write.xlsx(x = uncoded_db,file = here("Analysis","missing_tweets_links.xlsx"),col.names = T,row.names = F,showNA = T)
