## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(cache=TRUE,
                      echo = TRUE,
                      warning = FALSE,
                      message = FALSE)


## ----load_pkgs, results="hide"-----------------------------------------------------------------------
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
#Rborist and ranger are random forest algorithm wrappers
if(!require(Rborist)) 
  install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(ranger)) 
  install.packages("ranger", repos = "http://cran.us.r-project.org")
#zoo allows rolling operations
if(!require(zoo))
  install.packages("zoo", repos = "http://cran.us.r-project.org")
#matrix stats
if(!require(matrixStats)) 
  install.packages("matrixStats", repos = "http://cran.us.r-project.org")
#rpart.plot shows the decision tree of an rpart result
if(!require(rpart.plot)) 
  install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
#kableExtra allows more customization of tables
if(!require(kableExtra)) 
  install.packages("kableExtra")
if(!require(RColorBrewer)) 
  install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
#for dark background plots
if(!require(ggdark)) 
  install.packages("ggdark", repos = "http://cran.us.r-project.org")


## ----load_stats--------------------------------------------------------------------------------------
#if player played for multiple teams in same season, only keep total stats
advanced<-read_csv("Advanced.csv") %>% 
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% select(seas_id:mp,ows:ws,vorp)
totals<-read_csv("Player Totals.csv") %>% 
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm))
advanced_and_totals<-inner_join(totals,advanced)


## ----load_train--------------------------------------------------------------------------------------
free_agents<-read_csv("2016-2019 Free Agents.csv")
#gathered from Basketball-Reference
#contract info from Capology, Spotrac, Basketball-Insiders


## ----load_eval---------------------------------------------------------------------------------------
fa_2020<-read_csv("Free Agents 2020.csv")
#separate out options to compare what players options get if declined
fa_2020_options<-fa_2020 %>% filter(str_detect(type,"PO|CO"))
#remove club options (unsure how to handle), make player options all declined (UFA's)
fa_2020<-fa_2020 %>% filter(type != "CO") %>% 
  mutate(type=ifelse(type=="PO","UFA",type)) %>% group_by(player) %>% slice(1)


## ----load_cap_hist-----------------------------------------------------------------------------------
salary_cap_hist<-read_csv("Salary Cap History.csv")
#create variable of first year salary as percentage of cap
#easier to compare across years
free_agents<-free_agents %>% select(-c(terms,Source)) %>% 
  left_join(.,salary_cap_hist) %>% 
  mutate(first_year_percent_of_cap=yr_1_salary/cap) %>% 
  select(-c(yr_1_salary,cap))


## ----pre_process-------------------------------------------------------------------------------------
create_data<-function(x){
  a<-advanced_and_totals %>% group_by(player_id) %>% 
    #three year sum
    mutate(across(-c(1:10,fg_percent,x3p_percent,
                     x2p_percent:e_fg_percent,ft_percent),
                  list(three_yrs=~rollapplyr(.,3,sum,partial=TRUE)),
                  .names="{col}_last_3_yrs")) %>%
    inner_join(.,x) %>%
    mutate(ws_per_48_last_3_yrs=ws_last_3_yrs/mp_last_3_yrs*48) %>%
    #recalc percentages
    mutate(fg_percent_last_3_yrs=
             ifelse(fga_last_3_yrs==0,0,fg_last_3_yrs/fga_last_3_yrs),
           x3p_percent_last_3_yrs=
             ifelse(x3pa_last_3_yrs==0,0,x3p_last_3_yrs/x3pa_last_3_yrs),
           x2p_percent_last_3_yrs=
             ifelse(x2pa_last_3_yrs==0,0,x2p_last_3_yrs/x2pa_last_3_yrs),
           e_fg_percent_last_3_yrs=
             ifelse(fga_last_3_yrs==0,0,
                    (fg_last_3_yrs+0.5*x3p_last_3_yrs)/fga_last_3_yrs),
           ft_percent_last_3_yrs=
             ifelse(fta_last_3_yrs==0,0,ft_last_3_yrs/fta_last_3_yrs)) %>%
    #remove categories that aren't predictive vars or linear combo of others
    select(-c(hof,lg,pos,tm,ws,ws_last_3_yrs,
              trb,trb_last_3_yrs,fg,fga,fg_last_3_yrs,fga_last_3_yrs)) %>%
    #convert contract year and last 3 year stats to per game (except games)
    mutate(across(c(mp,x3p:x3pa,x2p:x2pa,ft:fta,orb:pts),list(per_game=~./g)),
           .after="gs") %>%
    select(-c(mp,x3p:x3pa,x2p:x2pa,ft:fta,orb:pts)) %>% 
    mutate(across(mp_last_3_yrs:pts_last_3_yrs,list(per_game=~./g_last_3_yrs)),
           .after="gs_last_3_yrs") %>%
    select(-c(mp_last_3_yrs:pts_last_3_yrs)) %>% ungroup() %>%
    #scale games & cumulative advanced stats (since short season due to COVID)
    mutate(across(starts_with("g")|starts_with("vorp")|
                    contains("dws")|contains("ows"),
                  list(scaled=~scale(.))),.keep="unused") %>%
    relocate(g_scaled,gs_scaled,.after="experience") %>%
    relocate(g_last_3_yrs_scaled,gs_last_3_yrs_scaled,
             .before="mp_last_3_yrs_per_game") %>%
    relocate(vorp_scaled,ows_scaled,dws_scaled,.after="ft_percent")
  return(a)
}
advanced_and_totals_train_set<-create_data(free_agents)


## ----targets_corr, echo=FALSE,fig.height=3.75,fig.width=7.5------------------------------------------
#plot first year salary against contract length
advanced_and_totals_train_set %>% 
  ggplot(aes(x=factor(contract_yrs),y=first_year_percent_of_cap)) + 
  geom_boxplot() + geom_jitter(alpha=0.1,width=0.2) + 
  labs(x="Contract Years",y="First Year Cap %") +
  scale_y_continuous(labels = scales::percent) +
  annotate("text",x=1.5,y=0.3,label=paste0("Correlation Coeff:\n",round(
    cor(advanced_and_totals_train_set$contract_yrs,
        advanced_and_totals_train_set$first_year_percent_of_cap),4))) +
  dark_theme_gray()


## ----ws_vs_salary, echo=FALSE,fig.height=3.75,fig.width=7.5------------------------------------------
#plot win shares vs salary, color points based on contract length
free_agents %>%
  ggplot(aes(x=first_year_percent_of_cap,y=ws,color=factor(contract_yrs))) + 
  geom_point() +
  scale_colour_brewer(palette="RdYlGn") +
  labs(x="First Year Cap %",y="Win Shares",color="Contract Years") +
  annotate("text",x=0.05,y=12.5,
           label=paste0("Correlation Coeff:\n",round(
             cor(free_agents$ws,free_agents$first_year_percent_of_cap),4))) +
  dark_theme_gray()


## ----contracts_by_season, echo=FALSE,fig.height=4,fig.width=7.5--------------------------------------
#plot distribution of contracts by season as percentage of total contracts given out
advanced_and_totals_train_set %>%
  ggplot(aes(x=factor(season),fill=factor(contract_yrs))) +
  geom_bar(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..]), 
           position="dodge") +
  labs(x="Season",y="Percent of Contracts Given",fill="Contract Years") +
  scale_fill_brewer(palette="RdYlGn") +
  scale_y_continuous(labels = scales::percent) +
  dark_theme_gray()


## ----svm_explain, out.width="50%", fig.cap="H1 does not separate the classes. H2 does, but only with a small margin. H3 separates them with the maximal margin. By User:ZackWeinberg, based on PNG version by User:Cyc - This file was derived from: Svm separating hyperplanes.png, CC BY-SA 3.0, https://commons.wikimedia.org/w/index.php?curid=22877598",echo=FALSE----
#svm graphic from wikipedia
knitr::include_graphics("SVM2.png")


## ----actual_values,include=FALSE---------------------------------------------------------------------
#remove actuals to check against predictions
actual_values=advanced_and_totals_train_set %>% 
  select(1:8,contract_yrs,first_year_percent_of_cap)

## ----get_important_vars, include=FALSE---------------------------------------------------------------
#https://stats.stackexchange.com/a/113622
get_5_top_important<- function(model){
  ImpMeasure<-data.frame(varImp(model)$importance)
  ImpMeasure$Vars<-row.names(ImpMeasure)
  # minus to order descending
  ImpMeasure[order(-ImpMeasure$Overall),][1:5,]
}

## ----model_table_func, include=FALSE-----------------------------------------------------------------
get_yrs_metrics<-function(x,ml_method,first=TRUE){
  #base newdata off whether variable was predicted first or second
  if (first==TRUE){
    vec=predict(x,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                  select(-c(seas_id:birth_year,first_year_percent_of_cap)))
  }
  else{
    vec=predict(x,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                  select(-c(seas_id:birth_year)))
  }
  #how many correct predictions
  percent_correct=sum(actual_values$contract_yrs==round(vec,0))/length(vec)
  #how many predictions off by more than one year
  off_by_more_than_one=sum(abs(actual_values$contract_yrs-round(vec,0))>1)/
    length(vec)
  #how many predictions are for max contract length of 5
  num_max_yr_predicts=sum(round(vec,0)==5)
  mean_abs_error=min(x$results[["MAE"]])
  resid_mean_sq_error=min(x$results[["RMSE"]])
  return (tibble(Method=ml_method,"Correct Predict %"=percent_correct,
                 "Off By >1 Yr"=off_by_more_than_one,
                 "Max Year Predicts"=num_max_yr_predicts,MAE=mean_abs_error,
                 RMSE=resid_mean_sq_error))
}
get_sal_metrics<-function(x,ml_method,first=FALSE){
  #base newdata off whether variable was predicted first or second
  if (first==FALSE){
    vec=predict(x,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                  select(-c(seas_id:birth_year)))
  }
  else{
    vec=predict(x,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                  select(-c(seas_id:birth_year,contract_yrs)))
  }
  #how many predictions are off by more than 5%
  off_by_more_than_five_percent=
    sum(abs(actual_values$first_year_percent_of_cap-vec)>0.05)/length(vec)
  mean_abs_error=min(x$results[["MAE"]])
  resid_mean_sq_error=min(x$results[["RMSE"]])
  return (tibble(Method=ml_method,"Off By >5%"=off_by_more_than_five_percent,
                 MAE=mean_abs_error,RMSE=resid_mean_sq_error))
}


## ----lin_yrs_indep-----------------------------------------------------------------------------------
lin_yrs<-train(contract_yrs~.,
               data=(advanced_and_totals_train_set %>% ungroup() %>% 
                       select(-c(seas_id:birth_year,
                                 first_year_percent_of_cap))),
               method="lm",
               trControl=trainControl(method="loocv"))


## ----lin_yrs_top_vars, echo=FALSE--------------------------------------------------------------------
get_5_top_important(lin_yrs)


## ----knn_yrs_indep-----------------------------------------------------------------------------------
knn_yrs<-train(contract_yrs~.,
               data=(advanced_and_totals_train_set %>% ungroup() %>% 
                       select(-c(seas_id:birth_year,first_year_percent_of_cap))),
               method="knn",
               tuneGrid=data.frame(k=c(5:25)),
               trControl=trainControl(method="loocv"))
knn_yrs$finalModel


## ----rpart_yrs_indep---------------------------------------------------------------------------------
set.seed(100,sample.kind = "Rounding")
rpart_yrs<-train(contract_yrs~.,
                 data=(advanced_and_totals_train_set %>% ungroup() %>% 
                         select(-c(seas_id:birth_year,first_year_percent_of_cap))), 
                 method="rpart", tuneGrid = data.frame(cp=seq(0.005,0.02,0.001)),
                 trControl=trainControl(method="loocv"))

## ----decision_tree, echo=FALSE, fig.align='center',fig.height=4.4,fig.width=7.8----------------------
rpart.plot(rpart_yrs$finalModel)


## ----random_forests----------------------------------------------------------------------------------
set.seed(10,sample.kind="Rounding")
ranger_yrs<-train(contract_yrs~.,
                  data=(advanced_and_totals_train_set %>% ungroup() %>% 
                          select(-c(seas_id:birth_year,first_year_percent_of_cap))), 
                  method="ranger", importance="permutation")
set.seed(3,sample.kind="Rounding")
rborist_yrs<-train(contract_yrs~.,
                   data=(advanced_and_totals_train_set %>% ungroup() %>% 
                           select(-c(seas_id:birth_year,first_year_percent_of_cap))), 
                   method="Rborist", importance="permutation")
get_5_top_important(ranger_yrs)


## ----rborist_imp-------------------------------------------------------------------------------------
get_5_top_important(rborist_yrs)


## ----svm_yrs-----------------------------------------------------------------------------------------
set.seed(2,sample.kind="Rounding")
svm_yrs<-train(contract_yrs~.,
               data=(advanced_and_totals_train_set %>% ungroup() %>% 
                       select(-c(seas_id:birth_year,first_year_percent_of_cap))),
               method="svmRadial", trControl=trainControl(method="loocv"))


## ----yrs_first_metrics,echo=FALSE--------------------------------------------------------------------
models<-get_yrs_metrics(lin_yrs,"Linear")
models<-rbind(models,get_yrs_metrics(knn_yrs,"KNN"),
              get_yrs_metrics(rpart_yrs,"Decision Tree"),
              get_yrs_metrics(ranger_yrs,"Ranger"),
              get_yrs_metrics(rborist_yrs,"Rborist"),
              get_yrs_metrics(svm_yrs,"SVM"))
models %>% knitr::kable()


## ----yrs_predict_train-------------------------------------------------------------------------------
#best performers are random forests, but have hard time predicting five years
#if any models predict five years, use that prediction (since rare)
#else use median of rborist, ranger and svm
rborist_yrs_vec=predict(rborist_yrs,newdata=advanced_and_totals_train_set %>% 
                          ungroup() %>% 
                          select(-c(seas_id:birth_year,first_year_percent_of_cap)))
ranger_yrs_vec=predict(ranger_yrs,newdata=advanced_and_totals_train_set %>% 
                         ungroup() %>% 
                         select(-c(seas_id:birth_year,
                                   first_year_percent_of_cap)))
svm_yrs_vec=predict(svm_yrs,newdata=advanced_and_totals_train_set %>% 
                      ungroup() %>% 
                      select(-c(seas_id:birth_year,
                                first_year_percent_of_cap)))
linear_yrs_vec=predict(lin_yrs,newdata=advanced_and_totals_train_set %>% 
                         ungroup() %>% 
                         select(-c(seas_id:birth_year,
                                   first_year_percent_of_cap)))
rpart_yrs_vec=predict(rpart_yrs,newdata=advanced_and_totals_train_set %>% 
                        ungroup() %>% 
                        select(-c(seas_id:birth_year,
                                  first_year_percent_of_cap)))
knn_yrs_vec=predict(knn_yrs,newdata=advanced_and_totals_train_set %>% 
                      ungroup() %>% 
                      select(-c(seas_id:birth_year,
                                first_year_percent_of_cap)))

advanced_and_totals_sal_predict=advanced_and_totals_train_set %>% 
  select(-contract_yrs) %>%
  add_column(contract_yrs=
               ifelse(round(rowMaxs(cbind(rborist_yrs_vec,ranger_yrs_vec,
                                          svm_yrs_vec,linear_yrs_vec,
                                          rpart_yrs_vec,knn_yrs_vec)))>=5,5,
                      round(rowMedians(cbind(rborist_yrs_vec,ranger_yrs_vec)))))


## ----lin_sal_based_on_yrs----------------------------------------------------------------------------
lin_sal_second<-train(first_year_percent_of_cap~.,
                      data=(advanced_and_totals_sal_predict %>% ungroup() %>% 
                              select(-c(seas_id:birth_year))),
                      method="lm",
                      trControl=trainControl(method="loocv"))
get_5_top_important(lin_sal_second)


## ----knn_sal_based_on_yrs----------------------------------------------------------------------------
knn_sal_second<-train(first_year_percent_of_cap~.,
                      data=(advanced_and_totals_sal_predict %>% ungroup() %>% 
                              select(-c(seas_id:birth_year))),
                      tuneGrid=data.frame(k=c(5:25)),
                      method="knn",
                      trControl=trainControl(method="loocv"))
knn_sal_second$finalModel


## ----randforest_sal_based_on_yrs---------------------------------------------------------------------
set.seed(10,sample.kind="Rounding")
ranger_sal_second<-train(first_year_percent_of_cap~.,
                         data=(advanced_and_totals_sal_predict %>% ungroup() %>% 
                                 select(-c(seas_id:birth_year))),
                         method="ranger",importance="permutation")

set.seed(20,sample.kind="Rounding")
rborist_sal_second<-train(first_year_percent_of_cap~.,
                          data=(advanced_and_totals_sal_predict %>% ungroup() %>% 
                                  select(-c(seas_id:birth_year))),
                          method="Rborist",importance="permutation")

## ----randforest_sal_second_importances---------------------------------------------------------------
get_5_top_important(ranger_sal_second)
get_5_top_important(rborist_sal_second)


## ----svm_sal_based_on_yrs----------------------------------------------------------------------------
set.seed(30,sample.kind="Rounding")
svm_sal_second<-train(first_year_percent_of_cap~.,
                      data=(advanced_and_totals_sal_predict %>% ungroup() %>% 
                              select(-c(seas_id:birth_year))),
                      method="svmRadial", trControl=trainControl(method="loocv"))

## ----sal_second_metrics,echo=FALSE-------------------------------------------------------------------
models<-get_sal_metrics(lin_sal_second,"Linear")
models<-rbind(models,get_sal_metrics(knn_sal_second,"KNN"),
              get_sal_metrics(ranger_sal_second,"Ranger"),
              get_sal_metrics(rborist_sal_second,"Rborist"),
              get_sal_metrics(svm_sal_second,"SVM"))
models %>% knitr::kable()


## ----lin_sal_indep-----------------------------------------------------------------------------------
lin_sal<-train(first_year_percent_of_cap~.,
               data=(advanced_and_totals_train_set %>% ungroup() %>% 
                       select(-c(seas_id:birth_year,contract_yrs))),
               method="lm",
               trControl=trainControl(method="loocv"))
get_5_top_important(lin_sal)


## ----knn_sal_indep-----------------------------------------------------------------------------------
knn_sal<-train(first_year_percent_of_cap~.,
               data=(advanced_and_totals_train_set %>% ungroup() %>% 
                       select(-c(seas_id:birth_year,contract_yrs))),
               method="knn",
               tuneGrid=data.frame(k=c(5:25)),
               trControl=trainControl(method="loocv"))
knn_sal$finalModel


## ----random_forests_sal_indep------------------------------------------------------------------------
set.seed(10,sample.kind="Rounding")
ranger_sal<-train(first_year_percent_of_cap~.,
                  data=(advanced_and_totals_train_set %>% ungroup() %>% 
                          select(-c(seas_id:birth_year,contract_yrs))), 
                  method="ranger", importance="permutation")
set.seed(3,sample.kind="Rounding")
rborist_sal<-train(first_year_percent_of_cap~.,
                   data=(advanced_and_totals_train_set %>% ungroup() %>% 
                           select(-c(seas_id:birth_year,contract_yrs))), 
                   method="Rborist", importance="permutation")


## ----rand_forest_sal_first_importances---------------------------------------------------------------
get_5_top_important(ranger_sal)
get_5_top_important(rborist_sal)


## ----svm_sal_indep-----------------------------------------------------------------------------------
set.seed(2,sample.kind="Rounding")
svm_sal<-train(first_year_percent_of_cap~.,
               data=(advanced_and_totals_train_set %>% ungroup() %>% 
                       select(-c(seas_id:birth_year,contract_yrs))),
               method="svmRadial", trControl=trainControl(method="loocv"))

## ----sal_first_metrics,echo=FALSE--------------------------------------------------------------------
models<-get_sal_metrics(lin_sal,"Linear",TRUE)
models<-rbind(models,get_sal_metrics(knn_sal,"KNN",TRUE),
              get_sal_metrics(ranger_sal,"Ranger",TRUE),
              get_sal_metrics(rborist_sal,"Rborist",TRUE),
              get_sal_metrics(svm_sal,"SVM",TRUE))
models %>% knitr::kable()


## ----sal_predict_train-------------------------------------------------------------------------------
#take medians sinces MAEs close
rborist_sal_vec=predict(rborist_sal,newdata=advanced_and_totals_train_set %>% 
                          ungroup() %>% 
                          select(-c(seas_id:birth_year,contract_yrs)))
ranger_sal_vec=predict(ranger_sal,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                         select(-c(seas_id:birth_year,contract_yrs)))
svm_sal_vec=predict(svm_sal,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                      select(-c(seas_id:birth_year,contract_yrs)))
linear_sal_vec=predict(lin_sal,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                         select(-c(seas_id:birth_year,contract_yrs)))
knn_sal_vec=predict(knn_sal,newdata=advanced_and_totals_train_set %>% ungroup() %>% 
                      select(-c(seas_id:birth_year,contract_yrs)))

advanced_and_totals_yrs_predict=advanced_and_totals_train_set %>% 
  select(-first_year_percent_of_cap) %>%
  add_column(first_year_percent_of_cap=rowMedians(cbind(
    rborist_sal_vec,ranger_sal_vec,svm_sal_vec,linear_sal_vec,knn_sal_vec)))


## ----lin_yrs_based_on_sal----------------------------------------------------------------------------
lin_yrs_second<-train(contract_yrs~.,
                      data=(advanced_and_totals_yrs_predict %>% ungroup() %>% 
                              select(-c(seas_id:birth_year))),
                      method="lm",
                      trControl=trainControl(method="loocv"))
get_5_top_important(lin_yrs_second)


## ----knn_yrs_based_on_sal----------------------------------------------------------------------------
knn_yrs_second<-train(contract_yrs~.,
                      data=(advanced_and_totals_yrs_predict %>% ungroup() %>% 
                              select(-c(seas_id:birth_year))),
                      method="knn",
                      tuneGrid=data.frame(k=c(5:25)),
                      trControl=trainControl(method="loocv"))
knn_yrs_second$finalModel


## ----decision_tree_yrs_based_on_sal------------------------------------------------------------------
set.seed(100,sample.kind="Rounding")
rpart_yrs_second<-train(contract_yrs~.,
                        data=(advanced_and_totals_yrs_predict %>% ungroup() %>% 
                                select(-c(seas_id:birth_year))),
                        method="rpart", tuneGrid = data.frame(cp=seq(0.005,0.02,0.001)),
                        trControl=trainControl(method="loocv"))

## ----decision_tree_2, echo=FALSE, fig.align='center',fig.height=3.85,fig.width=7.8-------------------
rpart.plot(rpart_yrs_second$finalModel)


## ----randforest_yrs_based_on_sal---------------------------------------------------------------------
set.seed(10,sample.kind="Rounding")
ranger_yrs_second<-train(contract_yrs~.,
                         data=(advanced_and_totals_yrs_predict %>% ungroup() %>% 
                                 select(-c(seas_id:birth_year))),
                         method="ranger", importance="permutation")
set.seed(20,sample.kind="Rounding")
rborist_yrs_second<-train(contract_yrs~.,
                          data=(advanced_and_totals_yrs_predict %>% ungroup() %>% 
                                  select(-c(seas_id:birth_year))),
                          method="Rborist", importance="permutation")

## ----randforest_yrs_second_importances---------------------------------------------------------------
get_5_top_important(ranger_yrs_second)
get_5_top_important(rborist_yrs_second)


## ----svm_yrs_based_on_sal----------------------------------------------------------------------------
set.seed(30,sample.kind="Rounding")
svm_yrs_second<-train(contract_yrs~.,
                      data=(advanced_and_totals_yrs_predict %>% ungroup() %>% 
                              select(-c(seas_id:birth_year))),
                      method="svmRadial", trControl=trainControl(method="loocv"))

## ----yrs_second_metrics,echo=FALSE-------------------------------------------------------------------
models<-get_yrs_metrics(lin_yrs_second,"Linear",FALSE)
models<-rbind(models,get_yrs_metrics(knn_yrs_second,"KNN",FALSE),
              get_yrs_metrics(rpart_yrs_second,"Decision Tree",FALSE),
              get_yrs_metrics(ranger_yrs_second,"Ranger",FALSE),
              get_yrs_metrics(rborist_yrs_second,"Rborist",FALSE),
              get_yrs_metrics(svm_yrs_second,"SVM",FALSE))
models %>% knitr::kable()


## ----eval_set----------------------------------------------------------------------------------------
advanced_and_totals_eval_set<-create_data(fa_2020)


## ----removed, echo=FALSE-----------------------------------------------------------------------------
#players unable to make predictions on (no contract year stats)
anti_join(fa_2020,advanced_and_totals_eval_set) %>% 
  select(player,type) %>% knitr::kable() %>%
  kable_styling(latex_options = "HOLD_position",fixed_thead = T) %>% row_spec(0,bold=T)


## ----predicting_wrappers, include=FALSE--------------------------------------------------------------
#refactored so code doesn't repeat
predict_yrs<-function(x,first=TRUE){
  if (first==TRUE){
    dataset=advanced_and_totals_eval_set %>% ungroup() %>% 
      select(-c(seas_id:birth_year,first_year_percent_of_cap))
  }
  else{
    dataset=s1y2 %>% ungroup() %>% 
      select(-c(seas_id:birth_year))
  }
  a<-predict(x,newdata=dataset)
  return(a)
}
predict_sal<-function(x,first=FALSE){
  if (first==FALSE){
    dataset=y1s2 %>% ungroup() %>% 
      select(-c(seas_id:birth_year))
  }
  else{
    dataset=advanced_and_totals_eval_set %>% ungroup() %>% 
      select(-c(seas_id:birth_year,contract_yrs))
  }
  a<-predict(x,newdata=dataset)
  return(a)
}

## ----------------------------------------------------------------------------------------------------
#Y1S2 model, take max prediction if any predict
#else take median of rborist, ranger and svm
lin_2020_yrs_first=predict_yrs(lin_yrs)
knn_2020_yrs_first=predict_yrs(knn_yrs)
rpart_2020_yrs_first=predict_yrs(rpart_yrs)
ranger_2020_yrs_first=predict_yrs(ranger_yrs)
rborist_2020_yrs_first=predict_yrs(rborist_yrs)
svm_2020_yrs_first=predict_yrs(svm_yrs)

yrs_first_vec=ifelse(round(rowMaxs(cbind(
  rborist_2020_yrs_first,ranger_2020_yrs_first,svm_2020_yrs_first,
  lin_2020_yrs_first,rpart_2020_yrs_first,knn_2020_yrs_first)))>=5,5,
  round(rowMedians(cbind(rborist_2020_yrs_first,ranger_2020_yrs_first,
                         svm_2020_yrs_first))))
y1s2<-advanced_and_totals_eval_set %>% 
  select(-contract_yrs) %>% add_column(contract_yrs=yrs_first_vec)

#take median of salaries
lin_2020_sal_second=predict_sal(lin_sal_second)
knn_2020_sal_second=predict_sal(knn_sal_second)
ranger_2020_sal_second=predict_sal(ranger_sal_second)
rborist_2020_sal_second=predict_sal(rborist_sal_second)
svm_2020_sal_second=predict_sal(svm_sal_second)

sal_second_vec=rowMedians(cbind(rborist_2020_sal_second,ranger_2020_sal_second,
                                lin_2020_sal_second,svm_2020_sal_second,
                                knn_2020_sal_second))


## ----------------------------------------------------------------------------------------------------
#S1Y2 model, take median of salaries
lin_2020_sal_first=predict_sal(lin_sal,first=TRUE)
knn_2020_sal_first=predict_sal(knn_sal,first=TRUE)
ranger_2020_sal_first=predict_sal(ranger_sal,first=TRUE)
rborist_2020_sal_first=predict_sal(rborist_sal,first=TRUE)
svm_2020_sal_first=predict_sal(svm_sal,first=TRUE)

sal_first_vec=rowMedians(cbind(rborist_2020_sal_first,ranger_2020_sal_first,
                               lin_2020_sal_first,svm_2020_sal_first,
                               knn_2020_sal_first))

s1y2<-advanced_and_totals_eval_set %>% 
  select(-first_year_percent_of_cap) %>% 
  add_column(first_year_percent_of_cap=sal_first_vec)

#take median of all years models except KNN
lin_2020_yrs_second=predict_yrs(lin_yrs_second,first=FALSE)
rpart_2020_yrs_second=predict_yrs(rpart_yrs_second,first=FALSE)
ranger_2020_yrs_second=predict_yrs(ranger_yrs_second,first=FALSE)
rborist_2020_yrs_second=predict_yrs(rborist_yrs_second,first=FALSE)
svm_2020_yrs_second=predict_yrs(svm_yrs_second,first=FALSE)

yrs_second_vec=round(rowMedians(cbind(rborist_2020_yrs_second,
                                      ranger_2020_yrs_second,
                                      svm_2020_yrs_second,
                                      lin_2020_yrs_second,
                                      rpart_2020_yrs_second)))


## ----both_model_results,include=FALSE----------------------------------------------------------------
eval_set<-advanced_and_totals_eval_set %>% 
  select(-c(contract_yrs,first_year_percent_of_cap)) %>%
  add_column(yrs_Y1S2=y1s2$contract_yrs,
             yr1_cap_percent_Y1S2=sal_second_vec,
             yrs_S1Y2=yrs_second_vec,
             yr1_cap_percent_S1Y2=s1y2$first_year_percent_of_cap)

## ----yrs_dist,echo=FALSE,results='asis'--------------------------------------------------------------
#check distribution of years
y1s2=kable(eval_set %>% group_by(yrs_Y1S2) %>% tally())
s1y2=kable(eval_set %>% group_by(yrs_S1Y2) %>% tally())
cat(c("\\begin{table}[h]",s1y2,"\\hspace{1cm} \\centering ",y1s2,"\\end{table}"))


## ----rand_sample,echo=FALSE--------------------------------------------------------------------------
#random sample of players who are predicted to get 1-year contracts
kable(eval_set %>% filter(yrs_Y1S2==1) %>% 
        select(player,yrs_Y1S2,yr1_cap_percent_Y1S2) %>% slice_sample(n=5)) %>%
  kable_styling(position="center") 
kable(eval_set %>% filter(yrs_S1Y2==1) %>%
        select(player,yrs_S1Y2,yr1_cap_percent_S1Y2) %>% slice_sample(n=5)) %>%
  kable_styling(position="center")


## ----convert_less_than_mins--------------------------------------------------------------------------
#convert players w/less than minimum salary prediction to 0 salary & 0 years
eval_set<-eval_set %>% 
  mutate(yrs_Y1S2=ifelse(yr1_cap_percent_Y1S2<=0.01325,0,yrs_Y1S2)) %>%
  mutate(yr1_cap_percent_Y1S2=ifelse(yrs_Y1S2==0,0,yr1_cap_percent_Y1S2)) %>%
  mutate(yrs_S1Y2=ifelse(yr1_cap_percent_S1Y2<=0.01325,0,yrs_S1Y2)) %>%
  mutate(yr1_cap_percent_S1Y2=ifelse(yrs_S1Y2==0,0,yr1_cap_percent_S1Y2))

## ----yrs_dist_corrected,echo=FALSE, results='asis'---------------------------------------------------
#see corrected distribution
y1s2=kable(eval_set %>% group_by(yrs_S1Y2) %>% tally())
s1y2=kable(eval_set %>% group_by(yrs_Y1S2) %>% tally())
cat(c("\\begin{table}[h]",s1y2,"\\hspace{1cm} \\centering ",y1s2,"\\end{table}"))


## ----total_val_add-----------------------------------------------------------------------------------
#make total contract value assuming $115 million cap, 5% annual raises
eval_set<-eval_set %>% 
  mutate(total_Y1S2=
           round(yr1_cap_percent_Y1S2*115*((1.05)^(yrs_Y1S2)-1)/0.05,2),
         total_S1Y2=
           round(yr1_cap_percent_S1Y2*115*((1.05)^(yrs_S1Y2)-1)/0.05,2))


## ----join_options,echo=FALSE-------------------------------------------------------------------------
#show players with options (descending value of option)
options_decisions=inner_join(eval_set,fa_2020_options %>% filter(type=="PO") %>% 
                               select(player,first_year_percent_of_cap)) %>%
  arrange(desc(first_year_percent_of_cap)) %>%
  mutate(first_year_percent_of_cap=
           round(first_year_percent_of_cap/1000000,2)) %>%
  rename("Y1S2 Cap %"=yr1_cap_percent_Y1S2,
         "S1Y2 Cap %"=yr1_cap_percent_S1Y2,
         "2021 Option"=first_year_percent_of_cap) %>%
  select(player,"Y1S2 Cap %",yrs_Y1S2,total_Y1S2,"S1Y2 Cap %",yrs_S1Y2,
         total_S1Y2,"2021 Option")
write_csv(options_decisions,"Options.csv")
kable(options_decisions) %>% kable_styling(latex_options="scale_down")


## ----get_five_at_a_time,include=FALSE----------------------------------------------------------------
#arrange players by mean of predicted salary (descending order)
non_options= anti_join(eval_set,fa_2020_options %>% select(player)) %>%
  arrange(desc((yr1_cap_percent_Y1S2+yr1_cap_percent_S1Y2)/2)) %>%
  rename("Y1S2 Cap %"=yr1_cap_percent_Y1S2, "S1Y2 Cap %"=yr1_cap_percent_S1Y2) %>%
  select(player,"Y1S2 Cap %",yrs_Y1S2,total_Y1S2,"S1Y2 Cap %",yrs_S1Y2,total_S1Y2)
write_csv(non_options,"Non-Option Contracts.csv")
get_five_at_a_time<-function(x){
  return(anti_join(eval_set,fa_2020_options %>% select(player)) %>%
           arrange(desc((yr1_cap_percent_Y1S2+yr1_cap_percent_S1Y2)/2)) %>%
           rename("Y1S2 Cap %"=yr1_cap_percent_Y1S2, 
                  "S1Y2 Cap %"=yr1_cap_percent_S1Y2) %>%
           select(player,"Y1S2 Cap %",yrs_Y1S2,total_Y1S2,
                  "S1Y2 Cap %",yrs_S1Y2,total_S1Y2) %>% slice(x))
}

## ----top_five,echo=FALSE-----------------------------------------------------------------------------
get_five_at_a_time(1:5) %>% knitr::kable()

## ----next_five,echo=FALSE----------------------------------------------------------------------------
get_five_at_a_time(6:10) %>% knitr::kable()

## ----fifteen,echo=FALSE------------------------------------------------------------------------------
get_five_at_a_time(11:15) %>% knitr::kable()