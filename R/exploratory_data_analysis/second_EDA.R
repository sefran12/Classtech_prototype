### SECOND EDA 

library(readr)
library(tidyverse)
library(tidymodels)
library(themis)
library(nnet)
library(ranger)
library(broom)
library(broom.mixed)
theme_set(theme_bw())
options(pillar.sigfig = 6)
#install.packages('ranger', dependencies = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))

chat <- read_csv("Classtech_prototype/Classtech_prototype/processed_data/augmented_chat_data.csv")
calib <- calib %>% 
    mutate(value = factor(value, levels = c('Bajo', 'Medio', 'Alto')))

# little EDA

calib %>% 
    select(-message) %>%  
    mutate(value = factor(value, levels = c('Bajo', 'Medio', 'Alto'))) %>% 
    GGally::ggpairs(aes(fill = value, color = value, alpha = 0.7))
                    
### model

df <- calib %>% 
    select(-message) %>% 
    mutate_if(is.logical, as.numeric)

# sampling

df_boot <- bootstraps(df)
multinom_formula <- value ~ message_complexity + penalized_words*relevancy_score*is_question

df_rec <- df %>% 
    recipe(value ~ .) %>% 
    step_smote(value)

df_prep <- prep(df_rec)

juice(df_prep)

# modeling
# 
multinom_reg_ <- multinom_reg(mode = 'classification') %>%
    set_engine("nnet")
# 
# multinom_reg <- rand_forest(mode = 'classification') %>% 
#     set_engine("ranger")

df_wf <- workflow() %>% 
    add_recipe(df_rec) %>% 
    add_model(multinom_reg_)

df_result <- fit_resamples(
    df_wf,
    df_boot,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
)

# metrics

df_result %>% 
    collect_metrics()

df_result %>% 
    collect_predictions() %>% 
    conf_mat(value, .pred_class)

df_result %>% 
    collect_predictions() %>% 
    ppv(value, .pred_class)

df_result %>% 
    collect_predictions() %>%
    group_by(id) %>% 
    ppv(value, .pred_class)

df_result %>% 
    collect_predictions() %>%
    sens(value, .pred_class)

df_result %>% 
    collect_predictions() %>%
    spec(value, .pred_class)

# exploring the model

#### FULL MODEL
df_boot <- vfold_cv(df, v = 5)
multinom_formula <- value ~ message_complexity + penalized_words*relevancy_score*is_question

df_rec <- df %>% 
    recipe(value ~ .) %>% 
    step_smote(value)

df_prep <- prep(df_rec)

juice(df_prep)

# modeling
# 
multinom_reg_ <- multinom_reg(mode = 'classification') %>%
    set_engine("nnet", importance = 'permutation')
forest_reg <- rand_forest(mode = 'classification') %>%
    set_engine("ranger", importance = 'permutation')

df_wf <- workflow() %>% 
    add_recipe(df_rec) %>% 
    add_model(multinom_reg_)

df_result <- fit_resamples(
    df_wf,
    df_boot,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
)
unique(chat$user)
# metrics

df_result %>% 
    collect_metrics()

df_result %>% 
    collect_predictions() %>% 
    metrics(truth = value, estimate = .pred_class)

predictions <- df_result %>% 
    collect_predictions(summarize = TRUE) %>% 
    cbind(calib$message)

df_result %>% 
    collect_predictions() %>% 
    conf_mat(value, .pred_class) 

df_result %>% 
    collect_predictions() %>% 
    ppv(value, .pred_class)

df_result %>% 
    collect_predictions() %>%
    group_by(id) %>% 
    ppv(value, .pred_class)

df_result %>% 
    collect_predictions() %>%
    sens(value, .pred_class)

df_result %>% 
    collect_predictions() %>%
    spec(value, .pred_class)

pred_df <- 
    chat %>% 
    select(-(date:language)) %>% 
    mutate_if(is.logical, as.numeric)

colnames(df)
colnames(pred_df)

full_model <-  multinom_reg_ %>% 
    fit(multinom_formula, data = df)

nnet::multinom(multinom_formula, data = df, Hess = TRUE) %>% 
    tidy() %>% 
    mutate_if(is.numeric, round, 5)

full_modelforest <-  forest_reg %>% 
    fit(value ~., data = df)

fresh_predictions <- full_model %>%
    predict(
        new_data = pred_df
        ) %>%  `colnames<-`('multinomlogit') %>% 
    cbind(chat) 

fresh_predictions <- full_modelforest %>% 
    predict(
        new_data = pred_df
    ) %>% `colnames<-`('randomforest') %>% 
    cbind(fresh_predictions)

fresh_predictions %>% 
    dplyr::select(randomforest, multinomlogit, message, user) %>% 
    View()

user_engagement <- fresh_predictions %>% 
    count(user, randomforest) %>% 
    pivot_wider(names_from = randomforest, values_from = n)

### BY ORDINAL REGRESSION

library(MASS)

model_comparison <- polr(multinom_formula, data = df) %>% 
    predict() %>% 
    tibble() %>% 
    `colnames<-`('ordered_preds') %>% 
    cbind(full_model %>% predict(df)) %>% 
    cbind(full_modelforest %>% predict(df)) %>% 
    cbind(truth = df$value) %>% 
    cbind(as.character(calib$message)) %>% 
    `colnames<-`(c('ordered_logit', 'multinomial_logit', 'random_forest', 'truth', 'mensaje'))

model_comparison %>% 
    count(ordered_logit, truth)

model_comparison %>% 
    metrics(truth = truth, estimate = ordered_logit)

model_comparison %>% 
    metrics(truth = truth, estimate = multinomial_logit)

model_comparison %>% 
    metrics(truth = truth, estimate = random_forest)

fresh_predictions %>% 
    select(randomforest, multinomlogit, user, message, message_complexity) %>% 
    View()

fresh_predictions %>% 
    dplyr::select(randomforest, multinomlogit, message_complexity) %>% 
    GGally::ggpairs(mapping = aes(fill = randomforest, alpha = 0.7))

df %>% 
    mutate(is_question = factor(is_question),
           is_link = factor(is_link)) %>% 
    bind_cols(random_forest = predict(full_modelforest, df)) %>% 
    GGally::ggpairs(mapping = aes(fill = value, alpha = 0.8))

library(vip)

vip::vip(full_modelforest)
