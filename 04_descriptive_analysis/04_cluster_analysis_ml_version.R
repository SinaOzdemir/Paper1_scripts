###################################################################
# Title: Message clustering                                       #
# Author: Sina Özdemir                                            #
#         PhD candidate                                           #
#         Department of sociology and political science           #
#         NTNU, Norway                                            #
# Date: 13/04/2022                                                #
###################################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("cluster","tidyverse","patchwork","here","NbClust")

p_load(char = packs)
## Rclusterpp installation from the archive
# No need 
# url<-"https://cran.r-project.org/src/contrib/Archive/Rclusterpp/Rclusterpp_0.2.3.tar.gz"
# pkgFile<- "	Rclusterpp_0.2.3.tar.gz"
# 
# download.file(url = url,destfile = pkgFile)
### install dependencies
install.packages(c("Rcpp", "RcppEigen"))
pacman::p_install_gh("nolanlab/Rclusterpp")
### finally
install.packages(pkgs = pkgsFile,pgsFile , type = "source", repos = NULL)

data_path<- here("Data","Analysis_data")

graph_path<- here("Graphs","draft_2_graphs")


data<- readRDS(file = file.path(data_path,"ml_analysis_data.RDS")) %>% 
  mutate(Actor_type = recode(Actor_type, "Commissioner"="Comissioner")) %>% 
  mutate(across(V301_01_Identity_and_mandate:is_reply,~as.character(.x))) %>% 
  mutate(across(V301_01_Identity_and_mandate:is_reply,~recode(.x, "no" = 0, "yes" = 1))) %>% 
  mutate(across(V301_01_Identity_and_mandate:is_reply,~as.numeric(.x))) %>% 
  distinct(status_id,.keep_all = T)

object_data<- data %>%
  select(status_id,matches(match = "V301_"),is_reply) %>% 
  mutate(is_reply = as.numeric(is_reply)) %>% 
  mutate(is_reply = replace_na(is_reply,0))

# Clustering ------------------------------------------------

object_data<- object_data %>%
  mutate(across(V301_01_Identity_and_mandate:is_reply,~as.numeric(as.character(.x)))) %>% 
  column_to_rownames(var = "status_id")

# determine the relevant number of clusters using 22 indices:
#alternative clustering method once the ideal number of clusters determined:https://github.com/nolanlab/Rclusterpp

## Bootstrapping with 1000 itterations to determine the number of clusters with 30 something tests
#test


for (j in 1:400) {
  
  cat("bootstrapping itteration ",j,"\n")

clustering_data<- object_data %>% 
  slice_sample(prop = .1,replace = T)

test_stat<-c("kl",
             "ch",
             "hartigan",
             "ccc",
             "scott",
             "marriot",
             "trcovw",
             "tracew",
             "friedman",
             "rubin",
             "cindex",
             "db",
             "silhouette",
             "beale",
             "ratkowsky",
             "ball",
             "ptbiserial",
             "gap",
             "mcclain",
             "dunn",
             "sdindex",
             "sdbw")

##hubert and d index are graphical tests, should be run seperately
## gamma,gplus,tau takes too long to run

test_results<-data.frame()
for (i in 1:length(test_stat)) {
  
  cat("calculating ",test_stat[i],"\n")
  
  obj_clust_test_err<- NbClust(data = clustering_data,
                               distance = "binary",
                               min.nc = 1,
                               max.nc = 7,
                               method = "ward.D2",
                               index = test_stat[i])
  

  obj_clust_i_res<- data.frame(index_name = test_stat[i],
                               n_clust = obj_clust_test_err$Best.nc[["Number_clusters"]],
                               index_val = obj_clust_test_err$Best.nc[["Value_Index"]])
  
  test_results<- rbind(test_results, obj_clust_i_res)

}

a<- list(clustering_data)

test_results<- test_results %>% 
  as_tibble() %>% 
  mutate(sample_id = j) %>% 
  mutate(test_sample = a)

saveRDS(object = test_results,file = here("Results","ml_cluster_number","replace_true",paste0(j,"_","nbcluster_count_results.rds")))
}


# cluster number examination ----------------------------------------------

files<- list.files(here("Results","ml_cluster_number"),pattern = "*.rds",full.names = T)

cluster_numbers<- map_dfr(files,.f = readRDS,.id = "sample_id")

saveRDS(cluster_numbers, file = here("Results","ml_cluster_number","cluster_number_results.RDS"))


test_indices<- cluster_numbers %>% pull(index_name) %>% unique()

test_indices<- paste(test_indices,collapse = ", ")
cluster_numbers<- cluster_numbers %>% group_by(n_clust) %>% tally()



cluster_numbers_graph<- cluster_numbers %>% 
  ggplot(aes(x = as.factor(n_clust),y= n))+
  geom_bar(aes(fill = n),stat = "identity",position = "dodge")+
  theme_bw()+
  labs(x = "Suggested number of clusters",y = "Suggestion count")


ggsave(filename = "ml_cluster_count.jpg",plot = cluster_numbers_graph,path = here("Drafts","JEPP","graphs"), bg = "white")

## most suggested number of clusters is three



# clustering --------------------------------------------------------------

#devtools installation from github

library(Rclusterpp)

Rclusterpp::Rclusterpp.setThreads(threads = 3)

clustering_data<- readRDS(file = here("Data","Analysis_data","ml_analysis_data.RDS")) %>%
  select(screen_name,status_id,starts_with("V301_"),is_reply) %>% 
  mutate(tweet_id = paste0(screen_name,"_",status_id)) %>%
  distinct(tweet_id,.keep_all = T) %>%
  column_to_rownames(var = "tweet_id") %>% 
  select(-screen_name,-status_id) %>% 
  mutate(across(everything(),~recode(.x,"yes" = 1,"no" = 0))) %>% 
  mutate(across(everything(),~as.numeric(.x))) %>% 
  as.matrix()


clustering<- Rclusterpp::Rclusterpp.hclust(x = clustering_data,method = "ward",distance = "euclidean")

saveRDS(clustering,file = here("Results","ml_clustering_results.RDS"))

two_clusters<- cutree(tree = clustering,k = 2)

obj_clusters<- data.frame(status_id = names(two_clusters),
                          cluster_n = two_clusters)

obj_clustered<- readRDS(file = here("Data","Analysis_data","ml_analysis_data.RDS")) %>%
  select(screen_name,status_id,starts_with("V301_"),is_reply) %>% 
  mutate(tweet_id = paste0(screen_name,"_",status_id)) %>%
  distinct(tweet_id,.keep_all = T) %>%
  column_to_rownames(var = "tweet_id") %>% 
  select(-screen_name,-status_id) %>% 
  mutate(across(everything(),~recode(.x,"yes" = 1,"no" = 0))) %>% 
  mutate(across(everything(),~as.numeric(.x)))  %>%
  rownames_to_column(var = "status_id") %>% 
  left_join(x = .,y = obj_clusters, by = "status_id")


cluster_sum <- obj_clustered %>%
  group_by(cluster_n) %>% 
  summarise(total = n(),
            iam = sum(V301_01_Identity_and_mandate),
            output = sum(V301_02_Output),
            activity = sum(V301_03_Activity),
            opinion = sum(V301_04_Opinion),
            input = sum(V301_05_Input_seeking),
            other = sum(V301_06_Other),
            reply = sum(is_reply)) %>% 
  pivot_longer(cols = iam:reply, names_to = "variables", values_to = "values") %>% 
  mutate(object_share = round((values/total),2))

cluster_sum<- cluster_sum%>% 
  mutate(cluster_share = round((total/sum(unique(cluster_sum$total))),2)) %>% 
  mutate(cluster_n = recode(cluster_n, `1` = "All-in-one",`2` = "Formal-Communication")) %>% 
  mutate(variables = recode(variables, "iam" = "Identity & Mandate",
                            "output" = "Output",
                            "activity" = "Activity",
                            "opinion" = "Opinion",
                            "input" = "Input seeking",
                            "reply" = "Reply to others",
                            "other" = "Other"))

cluster_n<- cluster_sum %>%
  ggplot(aes(x = reorder(cluster_n,-cluster_share), y= cluster_share))+
  geom_bar(aes(fill = cluster_share),stat = "identity",position = "dodge",show.legend = F)+
  theme_bw()+
  labs(x = "Clusters", y = "Percentage",title = "Share of clusters in the whole sample",subtitle = paste0("N =", sum(unique(cluster_sum$total))))

cluster_insight<- cluster_sum %>% mutate(cluster_counts = paste0(cluster_n," (N = ",total,")")) %>% 
  mutate(object_share = values/total) %>% 
  ggplot(aes(x = variables,y = object_share))+
  geom_bar(aes(fill = object_share),position = "dodge",stat="identity",show.legend = F)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,vjust = 1))+
  labs(x= "object of publicity",y = "Share of objects in clusters",title = "Cluster content distribution")+
  facet_grid(cols =  vars(cluster_counts))

library(patchwork)
cluster_details<-cluster_n+cluster_insight

account_n <- data %>% 
  mutate(tweet_id = paste0(screen_name,"-",status_id)) %>% 
  distinct(tweet_id,.keep_all = T) %>% 
  group_by(Actor_type) %>% 
  summarise(message_count = n())


account_cluster<- data%>% 
  mutate(Actor_type = recode(Actor_type,"High representative and vice president" = "Comissioner")) %>% 
  mutate(tweet_id = paste0(screen_name,"_",status_id)) %>% 
  distinct(tweet_id,.keep_all = T) %>% 
  left_join(.,obj_clusters,by = c("tweet_id" = "status_id")) %>% 
  group_by(Actor_type,cluster_n) %>% 
  summarise(cluster_count = n()) %>% 
  mutate(Actor_type = replace_na(Actor_type, "Agency")) %>% 
  left_join(.,account_n, by = "Actor_type") %>% 
  mutate(cluster_perc = round((cluster_count/message_count),2)) %>% 
  mutate(cluster_n = recode(cluster_n, `1` = "All-in-one",`2` = "Formal-Communication")) %>% 
  ggplot(aes(x = cluster_n,y = cluster_perc))+
  geom_bar(aes(fill = cluster_perc),stat = "identity",position="dodge",show.legend = F)+
  theme_bw()+
  coord_flip()+
  labs(x = "Cluster of tweets",y = "Percentage")+
  facet_wrap(~Actor_type)


ggsave(filename = "actor_cluster.jpeg",plot = account_cluster,path = here("Drafts","JEPP","graphs"),width = 5,height = 6,units = "in",bg = "white")

