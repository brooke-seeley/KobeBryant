library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)

## Read in Data

data <- vroom('data.csv')

## Split Into train and test

trainData <- data %>%
  drop_na(shot_made_flag)

testData <- data %>%
  filter(is.na(shot_made_flag)) %>%
  select(-shot_made_flag)

## Initial Recipe (All Variables, Target Encoding)
#####

target_recipe <- recipe(shot_made_flag ~ ., data = trainData) %>%
  step_rm(shot_id, team_name, team_id) %>%
  step_mutate_at(all_of(c("game_event_id","game_id","period","playoffs")),
                 fn = list(as_factor = ~ as.factor(.))) %>%
  step_other(all_nominal_predictors(), threshold = 0.001) %>%
  step_lencode_mixed(all_factor_predictors(), outcome = vars(shot_made_flag)) %>%
  step_normalize(all_factor_predictors())

target_prep <- prep(target_recipe)
bake(target_prep, new_data = trainData)

#####