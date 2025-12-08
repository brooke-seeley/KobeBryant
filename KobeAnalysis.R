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

# target_recipe <- recipe(shot_made_flag ~ ., data = trainData) %>%
#   step_date(game_date, features="month") %>%
#   step_date(game_date, features="year") %>%
#   step_mutate(game_event_id = factor(game_event_id)) %>%
#   step_mutate(period = factor(period)) %>%
#   step_mutate(playoffs = factor(playoffs)) %>%
#   step_mutate(game_date_month = factor(game_date_month)) %>%
#   step_mutate(game_date_year = factor(game_date_year)) %>%
#   step_rm(shot_id, team_name, team_id, matchup, game_id, game_date) %>%
#   step_other(all_nominal_predictors(), threshold = 0.001) %>%
#   step_lencode_mixed(all_factor_predictors(),
#                      outcome = vars(shot_made_flag)) %>%
#   step_normalize(all_factor_predictors())
# 
# target_prep <- prep(target_recipe)
# bake(target_prep, new_data = trainData)

#####

## New Recipe
#####

new_recipe <- recipe(shot_made_flag ~ ., data = trainData) %>%
  step_date(game_date, features="month") %>%
  step_date(game_date, features="year") %>%
  step_mutate(home_away = if_else(str_detect(matchup, "@"), "away", "home")) %>%
  step_mutate(game_event_id = factor(game_event_id)) %>%
  step_mutate(period = factor(period)) %>%
  step_mutate(playoffs = factor(playoffs)) %>%
  step_mutate(home_away = factor(home_away)) %>%
  step_mutate(game_date_month = factor(game_date_month)) %>%
  step_mutate(game_date_year = factor(game_date_year)) %>%
  step_rm(shot_id, team_name, team_id, matchup, game_id, game_date,
          combined_shot_type) %>%
  step_other(all_nominal_predictors(), threshold = 0.001) %>%
  step_lencode_mixed(all_factor_predictors(),
                     outcome = vars(shot_made_flag)) %>%
  step_normalize(all_factor_predictors())

new_prep <- prep(new_recipe)
bake(new_prep, new_data = trainData)

#####

## Switching to Dummy Recipe
#####
# dummy_recipe <- recipe(shot_made_flag ~ ., data = trainData) %>%
#   step_date(game_date, features = "month") %>%
#   step_date(game_date, features = "year") %>%
#   step_mutate(home_away = if_else(str_detect(matchup, "@"), "away", "home")) %>%
#   step_mutate(across(c(game_event_id, period, playoffs, home_away,
#                        game_date_month, game_date_year), factor)) %>%
#   step_rm(shot_id, team_name, team_id, matchup, game_id, game_date, combined_shot_type) %>%
#   step_other(all_nominal_predictors(), threshold = 0.001) %>%
#   step_dummy(all_nominal_predictors())
# 
# dummy_prep <- prep(dummy_recipe)
# bake(dummy_prep, new_data = trainData)
#####

## Penalized Logistic Regression - Score: 0.61244 (wow! close!)
## New Recipe: 0.61240
#####

# library(glmnet)
# 
# preg_mod <- logistic_reg(mixture=tune(), penalty=tune()) %>%
#   set_engine("glmnet")

# preg_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(preg_mod)

# preg_workflow <- workflow() %>%
#   add_recipe(new_recipe) %>%
#   add_model(preg_mod)
# 
# ### Grid of values to tune over
# 
# tuning_grid <- grid_regular(penalty(),
#                             mixture(),
#                             levels = 5)
# 
# ### Split data for CV
# 
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# ### Run the CV
# 
# CV_results <- preg_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(brier_class)))
# 
# ### Find Best Tuning Parameters
# 
# bestTune <- CV_results %>%
#   select_best(metric="brier_class")
# 
# ### Finalize the Workflow & fit it
# 
# final_wf <-
#   preg_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# pen_reg_predictions <- final_wf %>%
#   predict(new_data = testData, type="prob")
# 
# ### Kaggle
# 
# pen_reg_kaggle_submission <- pen_reg_predictions %>%
#   bind_cols(., testData) %>%
#   select(shot_id, .pred_1) %>%
#   rename(shot_made_flag=.pred_1)
# 
# vroom_write(x=pen_reg_kaggle_submission, file="./PenRegPreds.csv", delim=',')
# 
# vroom_write(x=pen_reg_kaggle_submission, file="./NewPenRegPreds.csv", delim=',')

#####

## Regression Trees - Score: 0.61162 (taking down!), 
## Fully Tuned Trees: 0.61070, Slightly Less Tuned: 0.61246
## New Recipe: 0.60980, 0.60788, 0.60771
#####

# library(rpart)

# tree_mod <- rand_forest(mtry=tune(),
#                         min_n=tune(),
#                         trees=500) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")

# tree_mod <- rand_forest(mtry=tune(),
#                         min_n=tune(),
#                         trees=tune()) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")

# tree_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(tree_mod)

# tree_workflow <- workflow() %>%
#   add_recipe(new_recipe) %>%
#   add_model(tree_mod)

### Grid of values to tune over

# tuning_grid <- grid_regular(mtry(range=c(1,20)),
#                             min_n(),
#                             levels=5)

# tuning_grid <- grid_regular(mtry(range=c(1,20)),
#                             min_n(),
#                             trees(range=c(100,1000)),
#                             levels=5)

# tuning_grid <- grid_regular(mtry(range = c(2, 10)),
#                             min_n(range = c(1, 20)),
#                             trees(range = c(200, 600)),
#                             levels = 5)

# tuning_grid <- grid_regular(mtry(range = c(2, 10)),
#                             min_n(),
#                             levels=10)

# tuning_grid <- grid_regular(mtry(range = c(2, 6)),
#                             min_n(range = c(20, 80)),
#                             levels=5)

# tuning_grid <- grid_regular(mtry(range = c(2, 8)),
#                             min_n(range = c(60, 120)),
#                             levels=5)

# ### CV

# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# CV_results <- tree_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics=(metric_set(brier_class)))

# ### Find best tuning parameters

# bestTune <- CV_results %>%
#   select_best(metric="brier_class")

# ### Finalize workflow

# final_wf <-
#   tree_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)

# ### Predict

# tree_predictions <- final_wf %>%
#   predict(new_data = testData, type="prob")

# ### Kaggle

# tree_kaggle_submission <- tree_predictions %>%
#   bind_cols(., testData) %>%
#   select(shot_id, .pred_1) %>%
#   rename(shot_made_flag=.pred_1)

# vroom_write(x=tree_kaggle_submission, file="./RegTreePreds.csv", delim=',')

# vroom_write(x=tree_kaggle_submission, file="./FullyTunedTreePreds.csv", delim=',')

# vroom_write(x=tree_kaggle_submission, file="./TunedTreePreds.csv", delim=',')

# vroom_write(x=tree_kaggle_submission, file="./NewRFPreds.csv", delim=',')

# vroom_write(x=tree_kaggle_submission, file="./NewRFPreds2.csv", delim=',')

# vroom_write(x=tree_kaggle_submission, file="./NewRFPreds3.csv", delim=',')

#####

## K-Nearest Neighbors - Score: 0.97432 (super bad, yikes)
#####

# library(kknn)
# 
# knn_model <- nearest_neighbor(neighbors=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("kknn")
# 
# knn_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(knn_model)
# 
# ### Tuning Parameters
# 
# tuning_grid <- grid_regular(neighbors())
# 
# ### CV
# 
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# CV_results <- knn_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(brier_class)))
# 
# ### Find best K
# 
# bestTune <- CV_results %>%
#   select_best(metric="brier_class")
# 
# ### Finalize Workflow
# 
# final_wf <-
#   knn_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# knn_predictions <- final_wf %>%
#   predict(knn_workflow, new_data=testData, type="prob")
# 
# ### Kaggle
# 
# knn_kaggle_submission <- knn_predictions %>%
#   bind_cols(., testData) %>%
#   select(shot_id, .pred_1) %>%
#   rename(shot_made_flag=.pred_1)
# 
# vroom_write(x=knn_kaggle_submission, file="./KNNTreePreds.csv", delim=',')

#####

## Naive Bayes - Score: 1.62340 (expected that)
#####

# library(discrim)
# library(naivebayes)
# 
# nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
#   set_mode("classification") %>%
#   set_engine("naivebayes")
# 
# nb_workflow <- workflow() %>%
#   add_recipe(target_recipe) %>%
#   add_model(nb_model)
# 
# ### Grid of values to tune over
# 
# tuning_grid <- grid_regular(Laplace(),
#                             smoothness(),
#                             levels=5)
# 
# ### CV
# 
# folds <- vfold_cv(trainData, v = 5, repeats = 1)
# 
# CV_results <- nb_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics(metric_set(brier_class)))
# 
# ### Find best tuning parameters
# 
# bestTune <- CV_results %>%
#   select_best(metric="brier_class")
# 
# ### Finalize Workflow
# 
# final_wf <-
#   nb_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=trainData)
# 
# ### Predict
# 
# nb_predictions <- final_wf %>%
#   predict(nb_workflow, new_data=testData, type="prob")
# 
# ### Kaggle
# 
# nb_kaggle_submission <- nb_predictions %>%
#   bind_cols(., testData) %>%
#   select(shot_id, .pred_1) %>%
#   rename(shot_made_flag=.pred_1)
# 
# vroom_write(x=nb_kaggle_submission, file="./NBTreePreds.csv", delim=',')

#####

## Boosted Trees - Score: 
#####

boost_mod <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost", importance = "impurity", eval_metric = "logloss") %>%
  set_mode("classification")


boost_workflow <- workflow() %>%
  add_recipe(new_recipe) %>% 
  add_model(boost_mod)

### Grid of Values

tuning_grid <- grid_regular(
  trees(range = c(300, 500)),
  tree_depth(range = c(3, 8)),
  learn_rate(range = c(-3, -1)),   # log10 scale
  min_n(range = c(10, 100)),
  loss_reduction(),
  levels = 3
)

### CV

folds <- vfold_cv(trainData, v = 5, repeats = 1)

CV_results <- boost_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=(metric_set(brier_class)))

bestTune <- CV_results %>%
  select_best(metric="brier_class")

vroom_write(bestTune, "BoostedBestTune.csv")

### Finalize workflow & make predictions

final_wf <-
  boost_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

boost_predictions <- final_wf %>%
  predict(new_data = testData, type="prob")

### Kaggle

boost_kaggle_submission <- boost_predictions %>%
  bind_cols(., testData) %>%
  select(shot_id, .pred_1) %>%
  rename(shot_made_flag=.pred_1)

vroom_write(x=boost_kaggle_submission, file="./BoostedPreds.csv", delim=',')

#####