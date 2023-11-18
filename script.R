library(tidyverse)
library(Metrics)
library("MLmetrics")
library(moments)
library(caret)
library(corrplot)
library(RColorBrewer)
library(caTools)
library(randomForest) 
#data import
data<- read_csv('jobfair_train.csv',show_col_types = FALSE)
data_train <- data.frame(data, league_weight=1:nrow(data)) 
data<- read_csv('jobfair_test.csv',show_col_types = FALSE)
data_test <- data.frame(data, league_weight=1:nrow(data))
#data preparing
  #adding new col to connect club with league with quantitative characteristics of league

    for(i in 1:nrow(data_train))
      {
        league<-subset(data_train, league_id==data_train$league_id[i])
        data_train$league_weight[i]<-mean(league$days_active_last_28_days,na.rm = TRUE)+
         mean(league$avg_stars_top_11_players,na.rm = TRUE)+
         mean(league$avg_training_factor_top_11_players,na.rm = TRUE)+
         mean(league$global_competition_level,na.rm = TRUE)
    }

    for(i in 1:nrow(data_test))
      {
      league<-subset(data_test, league_id==data_test$league_id[i])
      data_test$league_weight[i]<-mean(league$days_active_last_28_days,na.rm = TRUE)+
       mean(league$avg_stars_top_11_players,na.rm = TRUE)+
       mean(league$avg_training_factor_top_11_players,na.rm = TRUE)+
       mean(league$global_competition_level,na.rm = TRUE)
    }

  #data normalization
    cols <- c('league_weight','days_active_last_28_days', 
          'league_match_won_count_last_28_days',
          'avg_stars_top_14_players',
          'training_count_last_28_days',
          'avg_training_factor_top_11_players', 
          'global_competition_level',
          'avg_stars_top_11_players','tokens_spent_last_28_days')
    pre_proc_val <- preProcess(data_train[,cols], method = c("center", "scale"))
    data_train[,cols] = predict(pre_proc_val, data_train[,cols])
    pre_proc_val <- preProcess(data_test[,cols], method = c("center", "scale"))
    data_test[,cols] = predict(pre_proc_val, data_test[,cols])
    #removing NA
    for(i in 1:nrow(data_train))
    {
      if(is.na(data_train$league_weight[i]))
      {
        data_train$league_weight[i]<- mean(data_train$league_weight,na.rm = TRUE)
      }
      if(is.na(data_train$global_competition_level[i]))
      {
        data_train$global_competition_level[i]<- mean(data_train$global_competition_level,na.rm = TRUE)
      }
    }
    for(i in 1:nrow(data_test))
    {
      if(is.na(data_train$league_weight[i]))
      {
        data_test$league_weight[i]<- mean(data_test$league_weight,na.rm = TRUE)
      }
      if(is.na(data_test$global_competition_level[i]))
      {
        data_test$global_competition_level[i]<- mean(data_test$global_competition_level,na.rm = TRUE)
      }
    }
  #saving prepared data
    write.csv(data_train,'norm_weight_train.csv')
    write.csv(data_test,'norm_weight_test.csv')
#training random forest on data train data set, parameters were selected by correlation analysis
    
    RF = randomForest(formula=league_rank~league_weight+days_active_last_28_days+
                        league_match_won_count_last_28_days+
                        training_count_last_28_days+
                        global_competition_level+
                        avg_stars_top_11_players+
                        avg_stars_top_14_players+
                        avg_training_factor_top_11_players+tokens_spent_last_28_days, data=data_train,
                      ntree = 100,type= 'regression',importance=TRUE) 
#predicting values
    y_pred<-round(predict(RF,data_test[,cols]),digits = 0)
    data_league_rank_predictions<- data.frame(data_test[,"club_id"], league_rank=y_pred)
#saving predictions
    write.csv(data_league_rank_predictions,'league_rank_predictions.csv')