library(tidyverse)
library(data.table)
library(rstudioapi)
library(glue)
library(highcharter)
library(plotly)
library(h2o)  
library(skimr)
library(inspectdf)
library(caret)
library(scorecard)
library(collapse)

raw <- fread('mushrooms.csv')
raw %>% skim()

raw %>% glimpse()

names(raw) <- names(raw) %>% 
  str_replace_all(" ","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("%","_")

colnames <- raw %>% colnames()

for ( i in colnames){
  raw[[i]] <- raw[[i]] %>% str_replace_all("'","") %>% as.factor()
}
raw


raw
raw$class <- raw$class%>% 
  factor(levels = c("e","p"),
         labels = c(1,0))

colnames



raw %>% skim()

raw$class %>% table() %>% prop.table() %>% round(2)



# --------------------------------- Modeling ----------------------------------


h2o.init()

h2o_data <- raw %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]


target <- 'class'
features <- raw %>% select(-class)%>% names()


# Fitting h2o model ----
model <- h2o.automl(
  x = features,
  y = target,
  training_frame    = train,
  validation_frame  = test,
  leaderboard_frame = test,
  stopping_metric = "AUC",
  seed = 123,
  nfolds=10,
  max_runtime_secs =360)

model@leaderboard %>% as.data.frame()
model <- model@leader


# Predicting the Test set results ----
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict


# Threshold / Cutoff ----  

model@leader %>% 
  h2o.performance(test) %>% 
  h2o.find_threshold_by_max_metric('f1') -> treshold


#Accuracy

model@leader %>% 
  h2o.confusionMatrix(test) %>% 
  as_tibble() %>% 
  select("0","1") %>% 
  .[1:2,] %>% t() %>% 
  fourfoldplot(conf.level = 0, color = c("red", "darkgreen"),
               main = paste("Accuracy = ",
                            round(sum(diag(.))/sum(.)*100,1),"%"))


#AUC

model@leader %>% 
  h2o.performance(test) %>% 
  h2o.auc() %>% round(2) -> auc


#Gini

model@leader %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c('train','test','cross_val')) %>%
  mutate(gini = 2*value-1) %>%
  select(data,auc=value,gini)

