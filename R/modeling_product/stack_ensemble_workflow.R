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

theme_set(theme_bw())
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
    add_model(xgb_spec)

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
    add_model(bag_spec)

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
multiclass_metrics <- metric_set(accuracy, sensitivity, kap)

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
    polr(truth ~ ordered_logit + multinomial_logit + random_forest, data = ., Hess = TRUE) 

super_learner_example %>% 
    tidy()

super_learner_example %>% 
    predict(type = 'class') %>% 
    bind_cols(model_comparison$truth) %>% 
    rename(pred = ...1, truth = ...2) %>% 
    mutate(pred = factor(pred, levels = c('Bajo', 'Medio', 'Alto')),
           truth = factor(truth, levels = c('Bajo', 'Medio', 'Alto'))) %>%
    multiclass_metrics(truth = truth, estimate = pred)

super_learner_example %>% 
    predict(type = 'probs') %>%
    as.data.frame() %>% 
    bind_cols(model_comparison$truth) %>% 
    `colnames<-`(c("Bajo", "Medio", "Alto", "truth")) %>% 
    mutate(truth = factor(truth)) %>%
    roc_curve(truth, Bajo:Alto) %>% 
    autoplot()

##### TESTING ON NEW DATA
library(readxl)
library(stringi)
fourth_calib <- read_excel("calibration_tests/Cuarta encuesta de calibracion de engagement (Responses).xlsx")

new_data <- data.frame(
    message = str_remove_all(colnames(fourth_calib)[-1], 
                             pattern = 'Por favor califique el nivel de engagement que considera que corresponde a cada interacción \\[|Intervenciones \\[') %>%
        str_remove_all('\\]') %>% str_to_lower() %>% 
        stri_trans_general(id = 'Latin-ASCII'),
    value = t(fourth_calib[1,-1])
) %>% na.omit()

new_data$numero_de_palabras <- str_count(new_data$message, "\\S+")
new_data$unique_letters <- sapply(new_data$message,
                              function(x){nchar(rawToChar(unique(charToRaw(str_remove(x, ' ')))))}
)
new_data$message_complexity <- (new_data$numero_de_palabras > 2)*1 +
    (new_data$unique_letters > 5)*1 + 
    new_data$unique_letters*0.1 + 
    log2(new_data$numero_de_palabras)*0.25
new_data$penalized_words <- str_count(new_data$message, penalized_words)
new_data$is_question <- str_count(new_data$message, question_indicators)
new_data$relevancy_score <- str_count(new_data$message, regex(relevancy_indicators))
new_data$is_link <- str_detect(new_data$message, link_indicator)

table(df$value)/nrow(df)

# creating the super learner

training_superlearner <- data.frame(
    truth = df$value,
    logit = (full_model %>% predict(df)),
    forest = (full_modelforest %>% predict(df)),
    svm = (full_modelsvm %>% predict(df))
)
colnames(training_superlearner) <- c('truth', 'logit', 'forest', 'svm')
training_superlearner %>% 
    multiclass_metrics(truth, estimate = forest)

#superlearner <- polr(truth ~ ., data = training_superlearner, Hess = TRUE)
superlearner <- forest_reg %>% 
    fit(truth ~., data = training_superlearner)

superlearner %>% predict(training_superlearner) %>% 
    bind_cols(training_superlearner$truth) %>% 
    rename(super = .pred_class, truth = ...2) %>% 
    multiclass_metrics(truth, estimate = super)

#
test_df <- new_data %>%
    dplyr::select(-message) %>% 
    mutate_if(is.logical, as.numeric)

test_superlearner <- data.frame(
    new_data$value,
    predict(full_model, test_df),
    predict(full_modelforest, test_df),
    predict(full_modelsvm, test_df)
)
colnames(test_superlearner) <- c('truth', 'logit', 'forest', 'svm')

predict(full_modelforest, new_data %>% dplyr::select(-message)) %>% 
    bind_cols(new_data$value) %>%
    rename(truth = ...2, pred = .pred_class) %>% 
    mutate(truth = factor(truth, levels = c('Bajo', 'Medio', 'Alto')),
           pred = factor(pred, levels = c('Bajo', 'Medio', 'Alto'))) %>% 
    multiclass_metrics(truth = truth, estimate = pred)

predict(superlearner, test_superlearner) %>% 
    bind_cols(new_data$value) %>%
    rename(truth = ...2, pred = .pred_class) %>% 
    mutate(truth = factor(truth, levels = c('Bajo', 'Medio', 'Alto')),
           pred = factor(pred, levels = c('Bajo', 'Medio', 'Alto'))) %>% 
    multiclass_metrics(truth = truth, estimate = pred)

scored_messages <- predict(full_modelforest, new_data, type = 'prob') %>% 
    mutate(
        low = 1*.pred_Bajo,
        mid = 2.5*.pred_Medio,
        high = 20*.pred_Alto,
        message_score = low + mid + high
    ) %>% 
    bind_cols(message = new_data$message,
              value = new_data$value)


1/(table(df$value)/nrow(df))/1.4621

# la forma de hacer valoracion del correo es crear un correo exclusivo donde el profesor
# envie los correos con intervenciones.

# mostrar como se veria el puntaje si sumamos todo
# si sumamos el promedio por clase
# como se veria, es el caso real, tomando las 800 que realmente ha corregido, y lo que bota
# lo que he corregido. Pero como ponerle puntaje?

# Despues ya mostrando el ajuste por cantidad. Para comparar como mejora haciendo el ajuste.
# 



# clases 3, slack correo 2, whatsapp 1

# CANTIDAD VERSUS CALIDAD
# 

library(patchwork)

a_ <- final_df %>% 
    mutate(value = factor(value, levels = c('Bajo', 'Medio', 'Alto'))) %>% 
    ggplot(aes(x = value, y = message_complexity, fill = value)) +
    geom_boxplot() +
    guides(fill = FALSE) +
    labs(x = 'Real', 
         y = 'Complejidad del mensaje')

b_ <- scored_messages %>% 
    mutate(value = factor(value, levels = c('Bajo', 'Medio', 'Alto'))) %>% 
    ggplot(aes(x = value, y = message_score, fill = value)) +
    geom_boxplot() +
    guides(fill = FALSE) +
    labs(x = 'Real',
         y = 'Score de modelo ensemble')


a_ + b_


scored_messages %>% 
    ggplot(aes(x = value, y = high)) +
    geom_boxplot()

# En promedio en una clase los alumnos participan 80% en chat y 20% en

# conseguir un machine learning engineer.

clipr::write_clip(pen)

predict(full_modelforest, new_data) %>% 
    bind_cols(truth = new_data$value,
              message = new_data$message) %>% 
    sample_n(20) %>% 
    clipr::write_clip()

predict(full_modelforest, new_data) %>% 
    bind_cols(truth = new_data$value,
              message = new_data$message) %>% 
    filter(truth != .pred_class) %>% 
    sample_n(20) %>% 
    clipr::write_clip()

calib %>% 
    rbind(new_data) %>% 
    select(value, numero_de_palabras) %>% 
    GGally::ggpairs(aes(fill = value))

calib %>% 
    rbind(new_data) %>% 
    count(value) %>% 
    mutate(odds = n/sum(n),
           odds = 1/odds/1.429799)
