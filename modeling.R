library(tidymodels)
pl_model <- pl_final %>% filter(Squat1SF == 1, Squat2SF == 1, Bench1SF == 1, Bench2SF == 1, Deadlift1SF == 1, Deadlift2SF == 1)

#create split data included train and test sets
set.seed(2222)
pl_split <- initial_split(pl_model, strata = Equipment)
pl_train <- training(pl_split)
pl_test <- testing(pl_split)

#create 10-fold cross-validation set
set.seed(4444)
pl_cv_fold <- vfold_cv(pl_train, v = 10)

#create a recipe
S1Kg_rcp <- recipe(Squat1Kg ~ Squat3Kg + Age + Sex + BodyweightKg + Equipment, data = pl_train_small) %>%
  step_dummy(all_nominal(), -all_outcomes())

#start with linear model
lm_model <- linear_reg() %>%
  set_engine("lm")

#create workflow for model

S1Kg_wf_lm <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(S1Kg_rcp)

#fitting the 10-fold cv data into the workflow
S1Kg_resamples_lm <- S1Kg_wf_lm %>%
  fit_resamples(pl_cv_fold)

#checking the rmse for LM, which = 6.73
S1Kg_resamples_lm %>% collect_metrics()


#next, the GLM model
glm_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine('glmnet')

glm_wf <- workflow() %>%
  add_model(glm_model) %>%
  add_recipe(S1Kg_rcp)

#create a set of parameters to use for elastic net tuning
glm_set <- parameters(penalty(range = c(-5,1), trans = log10_trans()),
                      mixture())

glm_grid <- grid_regular(glm_set, levels = c(7, 5))
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

set.seed(123)
glm_tune <- tune_grid(glm_wf,
                      resamples = pl_cv_fold,
                      grid = glm_grid,
                      metrics = metric_set(rmse),
                      control = ctrl)

best_glm <- select_best(glm_tune, metric = "rmse")

#again find the RMSE, this time ~6.75
glm_wf %>% finalize_workflow(best_glm) %>% fit_resamples(pl_cv_fold) %>% collect_metrics()


#random forest model, repeating the same steps as the LM
ranger_model <- rand_forest(seed = 1, splitrule = "extratrees") %>% #attempt at using extratrees to boost the training speed
  set_engine("ranger") %>%
  set_mode("regression")

ranger_wf <- workflow() %>%
  add_model(pl_ranger) %>%
  add_recipe(S1Kg_rcp)

#rmse is 16.4 this time, which is pretty bad; with its low speed, RF will not be considered
ranger_wf %>% fit_resamples(pl_cv_fold) %>% collect_metrics()