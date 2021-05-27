knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

ExchangeUSD <- read_excel("ExchangeUSD.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())


#all the input is in only one dataframe to be able to preserve the testing and training
#dataset for the two sets of input variables
usd_exchange_full = ExchangeUSD %>%
  mutate(previous_one_day_set_a = lag(ExchangeUSD$usd_eur,1),
         previous_one_day_set_b = lag(ExchangeUSD$usd_eur,1),
         previous_two_day_set_b = lag(ExchangeUSD$usd_eur,2),
         previous_one_day_set_c = lag(ExchangeUSD$usd_eur,1),
         previous_two_day_set_c = lag(ExchangeUSD$usd_eur,2),
         previous_three_day_set_c = lag(ExchangeUSD$usd_eur,3),
         previous_one_day_set_d = lag(ExchangeUSD$usd_eur,1),
         previous_two_day_set_d = lag(ExchangeUSD$usd_eur,2),
         five_day_rolling = rollmean(usd_eur,5, fill = NA),
         ten_day_rolling = rollmean(usd_eur,10, fill = NA)) %>%
  
  
  drop_na()

usd_exchange_full %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "First Set of Input Variables") +
  theme(legend.position = "none")


usd_exchange_full %>%
  pivot_longer(cols = c(4,5),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Set of Input Variables") +
  theme(legend.position = "none")

usd_exchange_full %>%
  pivot_longer(cols = 6:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Set of Input Variables") +
  theme(legend.position = "none")

usd_exchange_full %>%
  pivot_longer(cols = 9:12,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Set of Input Variables") +
  theme(legend.position = "none")

# We can create a function to normalize the data from 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# All the variables are normalized
normalized_usd = usd_exchange_full %>%
  mutate(across(3:12, ~normalize(.x)))
# Look at the data that has been normalized
summary(normalized_usd)

set.seed(123)
usd_train <- normalized_usd[1:400,]
usd_test <- normalized_usd[401:491,]

# We can create a function to unnormalize the data=
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }
# Get the min and max of the original training values
usd_min_train <- min(usd_exchange_full[1:400,3])
usd_max_train <- max(usd_exchange_full[1:400,3])
# Get the min and max of the original testing values
usd_min_test <- min(usd_exchange_full[401:491,3])
usd_max_test <- max(usd_exchange_full[401:491,3])
# Check the range of the min and max of the training dataset
usd_min_test

usd_min_train

usd_max_test
usd_max_train
relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}





relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



set.seed(12345)
# function setup that creates 2 layer model
model_two_hidden_layers = function(hidden,sec_hidden) {
  nn_model_true = neuralnet(usd_eur ~ previous_one_day_set_a, data=usd_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  train_results = compute(nn_model_true,usd_test[,3:4])
  truthcol = usd_exchange_full[401:491,3]$usd_eur
  predcol = unnormalize(train_results$net.result,usd_min_train, usd_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}
# creation of different models with varying number of nodes
results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_two_hidden_layers(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
# save the stat indices to a dataframe
set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])



# Combine the dataframes
set_a_models = rbind(set_a_models_two_layers,set_a_models_two_layers)









