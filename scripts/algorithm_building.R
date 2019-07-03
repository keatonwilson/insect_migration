#Building a machine learning model to predict monarch hectares in MX
#Keaton Wilson
#keatonwilson@me.com
#2019-06-27

#packages
library(caret)
library(tidyverse)
library(recipes)
library(rsample)
library(caretEnsemble)
library(doParallel)

#parallel processing
# cores = detectCores()
# doParallel::registerDoParallel(cores = 2)

#Set seed
set.seed(42)

#Reading in the data
monarch_synth = read_csv("./data/monarch_synth.csv")
monarch_real = read_csv("./data/monarch_data_real.csv")

glimpse(monarch_real)
glimpse(monarch_synth)

#Splitting year off of both
monarch_synth = monarch_synth %>%
  dplyr::select(-year)
monarch_real = monarch_real %>%
  dplyr::select(-year)

#Don't need to split into training and test, because we already have a test set

#A fair amount of missing, but we can try and impute 
#Building the recipe
monarch_rec = head(monarch_synth) %>%
  recipe(hectares ~ .) %>%
  step_knnimpute(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
  
  

#Prepping
prepped_monarch = prep(monarch_rec, training = monarch_synth, retain = FALSE)

monarch_train_data = bake(prepped_monarch, new_data = monarch_synth)
monarch_test_data = bake(prepped_monarch, new_data = monarch_real)
rm(prepped_monarch)
#Modeling
#Let's try a simple linear regression first
train_control = trainControl(method = "repeatedcv", number = 3, repeats = 3)

lm_mod = train(hectares ~ ., data = monarch_train_data,
               method = "lm", trControl = train_control, tuneLength = 10)
#Not too bad... I wonder if a transformation of the price variable might help? Oof. Way worse. back to normal. 
summary(lm_mod)

saveRDS(lm_mod, "./output/lm_model.rds")

#Random forest
rf_mod = train(hectares ~ ., data = monarch_train_data,
               method = "ranger", trControl = train_control, 
               tuneLength = 3, verbose = TRUE)

summary(rf_mod)
rf_mod
saveRDS(rf_mod, "./output/rf_model.rds")

monarch_synth %>%
  ggplot(aes(x = hectares)) +
  geom_histogram()

#xgboost
xgboost_mod = train(hectares ~ ., data = monarch_train_data,
                    method = "xgbTree", trControl = train_control, tuneLength = 3)

saveRDS(xgboost_mod, "./output/xgboost_model.rds")

#Can we try and ridge and lasso?
ridge_lasso_mod = train(hectares ~ ., data = monarch_train_data,
                        method = "glmnet", trControl = train_control, tuneLength = 10)

ridge_lasso_mod
summary(ridge_lasso_mod)
saveRDS(ridge_lasso_mod, "./output/ridge_lasso_mod.rds")

#Running models on test data
rf_mod_fit = predict(rf_mod, monarch_test_data)
xgboost_mod_fit = predict(xgboost_mod, monarch_test_data)
lm_mod_fit = predict(lm_mod, monarch_test_data)
ridge_lasso_fit = predict(ridge_lasso_mod, monarch_test_data)

postResample(pred = rf_mod_fit, obs = monarch_test_data$hectares)
postResample(pred = xgboost_mod_fit, obs = monarch_test_data$hectares)
postResample(pred = lm_mod_fit, obs = monarch_test_data$hectares)
postResample(pred = ridge_lasso_fit, obs = monarch_test_data$hectares)
