#########################################################
# Title: Model training for paper 2                     #
# Author: Sina Özdemir                                  #
#         PhD candidate                                 #
#         Department of sociology and political science #
#         sina.ozdemir@ntnu.no                          #
#########################################################


# setup -------------------------------------------------------------------

library(pacman)

dt_packs<- c("tidyverse","here","psych")
p_load(char = dt_packs)

# data and preprocessing --------------------------------------------------

training_data<- readRDS(file = here::here("embedded_training_data.RDS"))

V201_dummy<- psych::dummy.code(x = training_data$V201_subject_of_publicity) %>% 
  as.data.frame() %>% 
  `colnames<-`(c("V201_Other_actors","V201_Self","V201_Compound","V201_none"))

training_data<- training_data %>% 
  select(-V201_subject_of_publicity) %>% 
  cbind(.,V201_dummy)

DVs<- grep(pattern = "V301_|V201_",x = colnames(training_data),value = T)
IVs<- grep(pattern = "V301_|V201_|screen_name|screen_name_l|status_id",x = colnames(training_data),value = T,invert = T)

training_data<- training_data %>% 
  mutate(across(.cols = any_of(DVs), ~as.factor(.x))) %>% 
  mutate(across(.cols = any_of(DVs), ~recode_factor(.x, `1` = "yes",`0`="no")))

# fit models for v301_02 --------------------------------------------------------------


pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(DVs), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar


for (i in 7:length(DVs)) {
  
  cat("training models for ", DVs[i],"\n")
  
  features<- c(DVs[i],IVs)
  
  dv_balance<- table(training_data[DVs[i]])[1]/nrow(training_data)
  
  imbalance<- ifelse(dv_balance>.6|dv_balance<.4, T, F)
  
 
  # 1 = no, 2 = yes
  models<- c("glmnet","regLogistic","svmRadial","ranger","xgbTree")
  model_formula<- as.formula(paste0(DVs[i],"~","."))
 
 
  
  for (j in 1:length(models)) {
    
    cat("training ",models[j]," for ", DVs[i],"\n" )
    

    
    traincontrol<- caret::trainControl(method = "repeatedcv",
                                       number = 5,
                                       repeats = 5,
                                       search =  "grid",
                                       verboseIter = T,
                                       classProbs = T)
    
    model_data<- training_data %>% select(all_of(features))
    #I think this would cause data leak
    tt_index<- caret::createDataPartition(y = model_data[,DVs[i]],p = .7,list = F)
    
    train_dt<- model_data[tt_index,]
    test_x<- model_data[-tt_index,IVs]
    test_y<- model_data[-tt_index,DVs[i]]
    
    #create essembly here
    ml_models<- list()
    ml_performance<- list()
    
    for (k in 1:5) {
    
    if (models[j] == "glmnet") {
      traincontrol<- caret::trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 10,
                                         search =  "random",
                                         verboseIter = T,
                                         classProbs = T)
    }  
      

    #this is still selecting negative cases, filter doesn't
    model_data_positive <- train_dt %>% filter((!!as.symbol(DVs[i]))=="yes")
      print(k+1)
    
      if(isFALSE(imbalance)){
        model_data_negative<- train_dt %>% filter((!!as.symbol(DVs[i]))=="no") %>%
          slice_sample(n = nrow(model_data_positive),replace = T)
        
      }else{
        model_data_negative<- train_dt %>% filter((!!as.symbol(DVs[i]))=="no") %>%
          slice_sample(n = nrow(model_data_positive))
      }
      
    
    
    print(k+2)                               
    smot_df<- rbind(model_data_positive,model_data_negative)
    svsmot_df<- smot_df[,c(DVs[i],sample(IVs,size = (length(IVs)*.8),replace = F))]
    print(k+3)
    
    ml_model<-caret::train(form = model_formula,
                             data = svsmot_df,
                             method = models[j],
                             metric = "Accuracy",
                             maximize = T,
                             trControl = traincontrol)
      
      
    print(k+4)
    ml_models[[k]]<-ml_model
    print(k+5)
    model_test<- predict(ml_model,test_x,type = "raw")
    print(k+6)

    ml_performance[[k]]<- caret::confusionMatrix(data = model_test,reference = test_y,positive = "yes")$byClass
    print(k+7)
    
  }
  save(list = c("ml_models","ml_performance"),file = here::here("final_models",paste0(DVs[i],"_embedding_",models[j],".Rdata")))
  
  }
 }


beepr::beep(sound = 4)
close(pb)