##### FINAL ENSEMBLE MODEL ######

# Assumes the existence of two data sets: The labeled data set df
# and an unlabeled dataset df for fresh predictions.
# STACK ENSEMBLE

# LOADING LIBRARIES
library(tidyverse)
library(tidymodels)
library(tibble)
library(stacks)
library(rpart)
library(baguette)
library(themis)
library(purrr)
library(nnet)
library(ranger)
library(kernlab)
library(xgboost)

# OPTIONS

theme_set(theme_bw)
set.seed(1)

# LOADING DATA


# INITIAL VALIDATION SPLIT

df_split <- initial_split(df %>% as_tibble())
df_train <- training(df_split)
df_test <- training(df_split)

# CROSSVALIDATION SPLITS ON TRAINING DATA

folds <- rsample::vfold_cv(df_train, v = 5)

# RECIPE AND WORKFLOW DEFINITION

df_recipe <- recipe(value ~ ., data = df_train) %>% 
    step_smote(value)

df_workflow <- workflow() %>% 
    add_recipe(df_recipe)

df_metric <- metric_set(accuracy, roc_auc, kap, sens, spec)

# HYPERPARAMETER TUNING

df_control_grid <- control_stack_grid()
df_control_resamples <- control_stack_resamples()

## MODEL DEFINITIONS: ##

# MULTINOMIAL

multinomial_spec <- 
    multinom_reg(mode = 'classification') %>% 
    set_engine('nnet')

multinomial_wf <- 
    df_workflow %>% 
    add_model(multinomial_spec)

multinomial_res <- 
    fit_resamples(
        multinomial_wf,
        resamples = folds,
        metrics = df_metric,
        control = df_control_resamples
    )

# RANDOM FOREST

randforest_spec <- 
    rand_forest(mode = 'classification',
                mtry = tune(),
                trees = 1000,
                min_n = tune()) %>% 
    set_engine("ranger")

randforest_wf <- 
    df_workflow %>% 
    add_model(randforest_spec)

randforest_res <- 
    tune_grid(
        randforest_wf,
        resamples = folds,
        metrics = df_metric,
        control = df_control_grid
    )

# SUPPORT VECTOR MACHINES

svm_spec <- 
    svm_rbf(
        cost = 0.05,
        rbf_sigma = tune()
    ) %>% 
    set_engine('kernlab') %>% 
    set_mode('classification')

svm_wf <- 
    df_workflow %>% 
    add_model(svm_spec)

svm_res <- 
    tune_grid(
        svm_wf,
        resamples = folds,
        grid = 5,
        metrics = df_metric,
        control = df_control_grid
    )

# NEURAL NETWORKS

nnet_spec <- 
    mlp(hidden_units = tune(),
        penalty = 0.05,
        epochs = tune()) %>% 
    set_mode('classification') %>% 
    set_engine('nnet')

nnet_rec <- 
    df_recipe %>% 
    step_normalize(all_predictors())

nnet_wf <- 
    df_workflow %>% 
    add_model(nnet_spec)

nnet_res <- 
    tune_grid(
        object = nnet_wf,
        resamples = folds,
        grid = 10,
        metrics = df_metric,
        control = df_control_grid
    )

# XGBOOST

xgb_spec <- 
    boost_tree(
        trees = 100,
        min_n = tune(),
        mode = 'classification')%>% 
    set_engine('xgboost')

xgb_rec <- 
    df_recipe %>% 
    step_normalize(all_predictors())

xgb_wf <- 
    df_workflow %>% 
    add_model(nnet_spec)

xgb_res <- 
    tune_grid(
        object = xgb_wf,
        resamples = folds,
        grid = 10,
        metrics = df_metric,
        control = df_control_grid
    )

# bagging trees

bag_spec <- 
    bag_tree(
        cost_complexity = tune(),
        min_n = tune(),
        mode = 'classification') %>% 
    set_engine('rpart')

bag_rec <- 
    df_recipe %>% 
    step_normalize(all_predictors())

bag_wf <- 
    df_workflow %>% 
    add_model(nnet_spec)

bag_res <- 
    tune_grid(
        object = bag_wf,
        resamples = folds,
        grid = 10,
        metrics = df_metric,
        control = df_control_grid
    )

## SINGLE MODEL METRICS

multinomial_res %>% 
    collect_metrics() %>% 
    group_by(.metric) %>% 
    top_n(1)

randforest_res %>% 
    collect_metrics() %>% 
    group_by(.metric) %>% 
    top_n(1)

svm_res %>% 
    collect_metrics() %>% 
    group_by(.metric) %>% 
    top_n(1)

nnet_res %>% 
    collect_metrics() %>% 
    group_by(.metric) %>% 
    top_n(1)

xgb_res %>% 
    collect_metrics() %>% 
    group_by(.metric) %>% 
    top_n(1)

bag_res %>% 
    collect_metrics() %>% 
    group_by(.metric) %>% 
    top_n(1)

## ROC CURVE

svm_res %>% 
    collect_predictions() %>% 
    roc_curve(value, .pred_Bajo, .pred_Medio, .pred_Alto) %>% 
    autoplot()

randforest_res %>% 
    collect_predictions() %>% 
    roc_curve(value, .pred_Bajo, .pred_Medio, .pred_Alto) %>% 
    autoplot()

bag_res %>% 
    collect_predictions() %>% 
    roc_curve(value, .pred_Bajo, .pred_Medio, .pred_Alto) %>% 
    autoplot()

multinomial_res %>% 
    collect_predictions() %>% 
    roc_curve(value, .pred_Bajo, .pred_Medio, .pred_Alto) %>% 
    autoplot()

nnet_res %>% 
    collect_predictions() %>% 
    roc_curve(value, .pred_Bajo, .pred_Medio, .pred_Alto) %>% 
    autoplot()

xgb_res %>% 
    collect_predictions() %>% 
    roc_curve(value, .pred_Bajo, .pred_Medio, .pred_Alto) %>% 
    autoplot()

## STACKING ##

df_data_stack <-  
    stacks() %>% 
    add_candidates(multinomial_res) %>% 
    add_candidates(randforest_res) %>% 
    add_candidates(nnet_res) %>% 
    add_candidates(svm_res) %>% 
    add_candidates(xgb_res)

df_model_stack <- 
    df_data_stack %>% 
    blend_predictions()

df_model_stack <- 
    df_model_stack %>% 
    fit_members()

autoplot(df_model_stack)

predict(df_model_stack, pred_df) %>% 
    cbind(fresh_predictions) 

df_stack_testing <- 
    df_test %>%
    bind_cols(predict(df_model_stack, df_test, members = TRUE)) %>%
    select(value, .pred_class)

### DOESN'T WORK



model_comparison %>% 
    mutate(id = row_number(),
           truth = factor(truth)) %>% 
    pivot_longer(cols = ordered_logit:random_forest) %>% 
    group_by(id, mensaje, truth) %>% 
    count(value) %>% 
    top_n(1) %>% 
    ungroup() %>%
    mutate(value = factor(value)) %>% 
    metrics(truth = truth, estimate = value)

super_learner_example <- model_comparison %>% 
    glm(truth == 'Medio' ~ ordered_logit + multinomial_logit + random_forest, family = binomial, data = .) 

super_learner_example %>% 
    tidy()

super_learner_example %>% 
    predict(type = 'response') %>% 
    bind_cols(model_comparison$truth == 'Medio') %>% 
    rename(pred = ...1, truth = ...2) %>% 
    mutate(pred = factor(pred > 0.5),
           truth = factor(truth)) %>% 
    metrics(truth, pred)

super_learner_example %>% 
    predict(type = 'response') %>% 
    bind_cols(model_comparison$truth == 'Medio') %>% 
    rename(pred = ...1, truth = ...2) %>% 
    mutate(truth = factor(truth)) %>% 
    roc_curve(truth, pred) %>% 
    autoplot()
