library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)

## Read in Data

data <- vroom('data.csv')

## Split Into train and test

trainData <- data %>%
  drop_na(shot_made_flag) %>%
  mutate(shot_made_flag = factor(shot_made_flag))

testData <- data %>%
  filter(is.na(shot_made_flag)) %>%
  select(-shot_made_flag)

## Goal: Log-Loss < 0.6

## Recipe (Post-Editing, Target Encoding)
#####

target_recipe <- recipe(shot_made_flag ~ ., data = trainData) %>%
  step_date(game_date, features="month") %>%
  step_date(game_date, features="year") %>%
  step_mutate(game_event_id = factor(game_event_id)) %>%
  step_mutate(period = factor(period)) %>%
  step_mutate(playoffs = factor(playoffs)) %>%
  step_mutate(game_date_month = factor(game_date_month)) %>%
  step_mutate(game_date_year = factor(game_date_year)) %>%
  step_rm(shot_id, team_name, team_id, matchup, game_id, game_date) %>%
  step_other(all_nominal_predictors(), threshold = 0.001) %>%
  step_lencode_mixed(all_factor_predictors(), 
                     outcome = vars(shot_made_flag)) %>%
  step_normalize(all_factor_predictors())

target_prep <- prep(target_recipe)
bake(target_prep, new_data = trainData)

#####

## Penalized Logistic Regression - Score: 0.61244 (wow! close!)
#####

library(glmnet)

preg_mod <- logistic_reg(mixture=tune(), penalty=tune()) %>%
  set_engine("glmnet")

preg_workflow <- workflow() %>%
  add_recipe(target_recipe) %>%
  add_model(preg_mod)

### Grid of values to tune over

tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

### Split data for CV

folds <- vfold_cv(trainData, v = 5, repeats = 1)

### Run the CV

CV_results <- preg_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics(metric_set(brier_class)))

### Find Best Tuning Parameters

bestTune <- CV_results %>%
  select_best(metric="brier_class")

### Finalize the Workflow & fit it

final_wf <-
  preg_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

### Predict

pen_reg_predictions <- final_wf %>%
  predict(new_data = testData, type="prob")

### Kaggle

pen_reg_kaggle_submission <- pen_reg_predictions %>%
  bind_cols(., testData) %>%
  select(shot_id, .pred_1) %>%
  rename(shot_made_flag=.pred_1)

vroom_write(x=pen_reg_kaggle_submission, file="./PenRegPreds.csv", delim=',')


#####

## Regression Trees - Score: 0.61162 (taking down!)
#####

library(rpart)

tree_mod <- rand_forest(mtry=tune(),
                        min_n=tune(),
                        trees=500) %>%
  set_engine("ranger") %>%
  set_mode("classification")
  
tree_workflow <- workflow() %>%
  add_recipe(target_recipe) %>%
  add_model(tree_mod)

### Grid of values to tune over

tuning_grid <- grid_regular(mtry(range=c(1,20)),
                            min_n(),
                            levels=5)

### CV

folds <- vfold_cv(trainData, v = 5, repeats = 1)

CV_results <- tree_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=(metric_set(brier_class)))

### Find best tuning parameters

bestTune <- CV_results %>%
  select_best(metric="brier_class")

### Finalize workflow

final_wf <-
  tree_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

### Predict

tree_predictions <- final_wf %>%
  predict(new_data = testData, type="prob")

### Kaggle

tree_kaggle_submission <- tree_predictions %>%
  bind_cols(., testData) %>%
  select(shot_id, .pred_1) %>%
  rename(shot_made_flag=.pred_1)

vroom_write(x=tree_kaggle_submission, file="./RegTreePreds.csv", delim=',')

#####