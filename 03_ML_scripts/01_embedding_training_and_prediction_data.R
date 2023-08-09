################################################################################
#Title: Paper 2 classification models using wikipedia2vec pretrained embeddings#
#Author: Sina Özdemir                                                          #
#        PhD candidate                                                         #
#        Department of sociology and political science                         #
#        Norwegian university of science and technology                        #
#        sina.özdemir@ntnu.no                                                  #
#Date: 02/11/2022                                                              #
################################################################################



# setup -------------------------------------------------------------------

library(pacman)
packs<- c("tidyverse","here","tidytext","word2vec","beepr")
p_load(char = packs)


# data --------------------------------------------------------------------

coded_tweets<- readRDS(file = here("data","analysis_data_11042022.rds"))

eu_exec_tweets<- readRDS(file = here("data","eu_data_011219_310720.RDS"))


# preprocessing -----------------------------------------------------------

training_labels<- coded_tweets %>% 
  select(screen_name,status_id,V201_subject_of_publicity,V301_01_Identity_and_mandate:V301_06_Other) %>% 
  mutate(screen_name_l = tolower(screen_name))

eu_exec_tweets<- eu_exec_tweets %>% 
  mutate(screen_name_l = tolower(screen_name)) %>% 
  select(screen_name,screen_name_l,status_id,text,lang) %>% 
  filter(screen_name_l %in% training_labes$screen_name_l)

training_text<- eu_exec_tweets %>% 
  filter(status_id%in%coded_tweets$status_id) %>% 
  select(screen_name_l,status_id,text,lang)

training_text<- training_text %>% 
  distinct(status_id,.keep_all = T) %>% 
  filter(status_id%in%training_labes$status_id)

prediction_data<- eu_exec_tweets %>% 
  filter(!(status_id%in%training_text$status_id))

remove(coded_tweets,eu_exec_tweets)

# training data quality check ---------------------------------------------

## check for duplicates



# training data embedding -------------------------------------------------

text_embedding<- function(training_data, pretrained_embeddings_path, language){
  
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = 5, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  message(" tokenizing the text data")
  to_embedd <- training_data %>% 
    filter(lang == language) %>% 
    mutate(text = str_remove_all(string = text,pattern = emo::ji_rx)) %>% #remove emoji
    mutate(text = str_remove_all(string = text,pattern = "(https?://t\\.co[^ ]*)|(t\\.co[^ ]*)|(http[^ ]*)|(ftp[^ ]*)|(www\\.[^ ]*)")) %>% # these regular expressions are obtained from qdapRegex:::reg_check(pattern = "@rm_url", dictionary = getOption("regex.library")) and qdapRegex:::reg_check(pattern = "@rm_twitter_url", dictionary = getOption("regex.library"))
    mutate(text = str_remove_all(string = text, pattern ="[[:punct:]]" )) %>% #remove punctuations
    mutate(text = str_remove_all(string = text, pattern = "[[:digit:]]")) %>% #remove numbers
    mutate(text = str_remove_all(string = text, pattern = "#|@")) %>% #remove hash and mention signs
    unnest_tokens(input = "text",output = "word") %>% 
    filter(!(word%in%stopwords::stopwords(language = language,source = "stopwords-iso")))
  
 
  setTxtProgressBar(pb, 1)
  
  message(" extracting vocabulary")
  vocab_to_embedd<- to_embedd %>% pull(word) %>% unique()
  
 
  setTxtProgressBar(pb, 2)
  
  message(" getting the embedding")
  path_embedding_to_use <- list.files(path = pretrained_embeddings_path,pattern = "*.txt",full.names = T) %>% 
    grep(pattern = language,x = .,value = T)
  
 
  setTxtProgressBar(pb, 3)
  
  message(" extracting the vectors")
  embedding_to_use<- word2vec::read.wordvectors(file = path_embedding_to_use,type = "txt",normalize = T) %>% 
    as.matrix()
  
  embedding_to_use<- embedding_to_use[which(rownames(embedding_to_use)%in%vocab_to_embedd),] %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "word") 
  
 
  setTxtProgressBar(pb, 4)
  
  message("embedding the text data")
  embedded_data <- left_join(to_embedd, embedding_to_use, by = "word") %>% 
    mutate(across(V1:V100,~replace_na(.x,0))) %>% 
    group_by(screen_name_l,status_id) %>% 
    summarise(across(V1:V100,~mean(.x))) %>% 
    ungroup()
  
 
  
  message("returning the embedded data")
  setTxtProgressBar(pb, 5)
 
  
  return(embedded_data)
}

## function test

# lang<-unique(training_text$lang)[3]
# 
# system.time(expr = {test_res<-text_embedding(training_data = training_text,pretrained_embeddings_path = here("pretrained_embeddings"),language = lang)
# })

#works like a charm!


##furrr would make it possible to parallelize this,
##however each worker would need to read in the embedding matrix,
##do stuff and return the data while keeping everything in the memory.
##So I am not sure if paralleling along languages makes much sense.
## See: https://furrr.futureverse.org/articles/gotchas.html

#parallelization test

# install.packages("furrr")
# library(furrr)
# langs<- unique(training_text$lang)[2:3]
# 
# plan(multisession, workers = 8)
# 
# system.time(expr = {parallel_test<- future_map_dfr(langs,text_embedding,training_data = training_text,pretrained_embeddings_path = here("pretrained_embeddings"))})
# 
# system.time(expr = {non_parallel_test<- map_dfr(langs,text_embedding,training_data = training_text,pretrained_embeddings_path = here("pretrained_embeddings"))})

#Normal mapping didn't work because of memory limitations, I suspect I would bump into this if I do 
# parallelisation with the full data, so might be better to deploy with a for loop

# deploy function ---------------------------------------------------------

embedded_languages<- list.files(path = here("pretrained_embeddings"),pattern = "*.txt")


## training data implementation

langs<- unique(training_text$lang)

training_embeddings<- data.frame()

for (i in 1:length(langs)) {
  match <- grepl(pattern = langs[i],x = embedded_languages)
  
  if(any(match)){
  embedded_tweets<- text_embedding(training_data = training_text,
                                   pretrained_embeddings_path = here("pretrained_embeddings"),
                                   language = langs[i]) %>% 
    left_join(.,training_labels, by = c("screen_name_l","status_id"))
  
  training_embeddings<- rbind(training_embeddings,embedded_tweets)
  remove(embedded_tweets)} else{
    next
  }
}

saveRDS(training_embeddings, file = here("embedded_training_data.RDS"))
## prediction_data implementation----


langs<- unique(prediction_data$lang)

prediction_embeddings<- data.frame()


for (i in 1:length(langs)) {
  match<- grepl(pattern = langs[i],x = embedded_languages)
  
  if(any(match)){
  embedded_tweets<- text_embedding(training_data = prediction_data,
                                   pretrained_embeddings_path = here("pretrained_embeddings"),
                                   language = langs[i])
  
  prediction_embeddings<- rbind(prediction_embeddings,embedded_tweets)
  remove(embedded_tweets)}else{
    next
  }
}

saveRDS(prediction_embeddings, file = here("embedded_prediction_data.RDS"))
