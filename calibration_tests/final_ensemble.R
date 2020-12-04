### FINAL MODEL PREDICTION


final_df <- rbind(df, test_df)

# TRAINING MODELS:

full_model <-  multinom_reg_ %>% 
    fit(value~numero_de_palabras + unique_letters + penalized_words + relevancy_score*message_complexity*is_question + is_link,
        data = final_df[1:800,])

full_modelforest <-  forest_reg %>% 
    fit(value ~., data = final_df[1:800,])

full_modelsvm <- kernlab::ksvm(value ~ ., kernel = 'polydot',
                               data = final_df[1:800,],
                               rbf_sigma = 0.00000228009)

### SUPERLEARNER

training_superlearner <- data.frame(
    truth = final_df$value[801:998],
    logit = (full_model %>% predict(final_df[801:998,])),
    forest = (full_modelforest %>% predict(final_df[801:998,])),
    svm = (full_modelsvm %>% predict(final_df[801:998,]))
)
colnames(training_superlearner) <- c('truth', 'logit', 'forest', 'svm')
training_superlearner %>% 
    multiclass_metrics(truth, estimate = forest)

#superlearner <- polr(truth ~ ., data = training_superlearner, Hess = TRUE)
superlearner <- multinom_reg %>% 
    fit(truth ~., data = training_superlearner)

superlearner %>% predict(training_superlearner) %>% 
    bind_cols(training_superlearner$truth) %>% 
    rename(super = .pred_class, truth = ...2) %>% 
    multiclass_metrics(truth, estimate = super)

## PREDICTING NEW DATA

# MODEL ON FULL DATA

full_model <-  multinom_reg_ %>% 
    fit(value~numero_de_palabras + unique_letters + penalized_words + relevancy_score*message_complexity*is_question + is_link, data = final_df)

full_modelforest <-  forest_reg %>% 
    fit(value ~., data = final_df)

full_modelsvm <- kernlab::ksvm(value ~ ., kernel = 'rbfdot',
                               data = final_df,
                               rbf_sigma = 0.00000228009)

training_superlearner <- data.frame(
    truth = final_df$value,
    logit = (full_model %>% predict(final_df)),
    forest = (full_modelforest %>% predict(final_df)),
    svm = (full_modelsvm %>% predict(final_df))
)
colnames(training_superlearner) <- c('truth', 'logit', 'forest', 'svm')
training_superlearner %>% 
    multiclass_metrics(truth, estimate = svm)

superlearner <- forest_reg %>% 
    fit(truth ~., data = training_superlearner)

# new data (unlabeled)

chat$numero_de_palabras <- str_count(chat$message, "\\S+")
chat$unique_letters <- sapply(chat$message,
                                  function(x){nchar(rawToChar(unique(charToRaw(str_remove(x, ' ')))))}
)
chat$message_complexity <- (chat$numero_de_palabras > 2)*1 +
    (chat$unique_letters > 5)*1 + 
    chat$unique_letters*0.1 + 
    log2(chat$numero_de_palabras)*0.25
chat$penalized_words <- str_count(chat$message, penalized_words)
chat$is_question <- str_count(chat$message, question_indicators)
chat$relevancy_score <- str_count(chat$message, regex(relevancy_indicators))
chat$is_link <- str_detect(chat$message, link_indicator)

# getting predictions
predict_df <- chat %>% 
    dplyr::select(numero_de_palabras, unique_letters, message_complexity, penalized_words, is_question, relevancy_score, is_link, message, user) %>% 
    mutate_if(is.logical, as.numeric)

predict_df$logit = (full_model %>% predict(predict_df)) %>% pull()
predict_df$forest = (full_modelforest %>% predict(predict_df)) %>% pull()
predict_df$svm = (full_modelsvm %>% predict(predict_df))

predict_df$estimated_engagement <- superlearner %>% predict(predict_df) %>% pull()
predict_df$estimated_engagement_probs <- superlearner %>% predict(predict_df, type = 'prob') 
predict_df$confidence_in_estimation <- apply(predict_df$estimated_engagement_probs, function(x) sd(matrix(x)), MARGIN = 1)

predict_df %>% 
    dplyr::select(logit, forest, svm, estimated_engagement, confidence_in_estimation, message, user, estimated_engagement_probs)  %>% 
    View()

predict_df %>% 
    dplyr::select(estimated_engagement, confidence_in_estimation, message)  %>% 
    bind_cols(predict_df$estimated_engagement_probs) %>% 
    arrange((confidence_in_estimation)) %>% 
    filter(nchar(message) < 100) %>% 
    head(10) %>% 
    select(-confidence_in_estimation) %>%
    clipr::write_clip()


predict_df %>% 
    count(user, estimated_engagement) %>% 
    pivot_wider(values_from = n, names_from = estimated_engagement) %>% 
    rowwise() %>% 
    mutate(score = sum(23*Alto, 2.6*Medio, Bajo, na.rm = TRUE)) %>% 
    View()

predict_df %>% 
    filter(str_detect(message, 'grac|chau|jaj|hola|buenas noch', negate = TRUE)) %>% 
    count(estimated_engagement) %>% 
    mutate(n/sum(n))

nnet::multinom(value~numero_de_palabras + unique_letters + penalized_words + relevancy_score*message_complexity*is_question + is_link, data = final_df, Hess = TRUE) %>% 
    tidy() %>% 
    mutate_if(is.numeric, round, 5) %>%
    print(n = 30)

predict_df %>% 
    dplyr::select(estimated_engagement, confidence_in_estimation, message, user)  %>% 
    bind_cols(predict_df$estimated_engagement_probs) %>% 
    rowwise() %>% 
    mutate(ensemble_score = 26*.pred_Alto + 2.6*.pred_Medio + .pred_Bajo) %>% 
    group_by(user) %>% 
    summarise(sum(ensemble_score)) %>% 
    View()

######
user_engagement <- user_engagement %>% 
    rowwise() %>% 
    mutate(boletos = sum(Bajo, 2.5*Medio, 20*Alto, na.rm = TRUE))

skim(final_df)

# Premiar consistencia
# independientemente de la calidad

library(Rtsne)
library(plotly)

total_labs <- rbind(calib, new_data %>% mutate(message = labels(message))) %>% 
    distinct(across(-message), .keep_all = TRUE)
tsnedf <- total_labs %>%
    dplyr::select(-message)

a_ <- tsnedf %>% 
    Rtsne(dims = 3, perplexity = 9)

a_ <- a_$Y %>% 
    bind_cols(value = total_labs$value,
              text = total_labs$message) %>%
    rename(x = ...1, y = ...2, z = ...3, truth = value)

plot_ly(a_, x = ~x, y = ~y, z = ~z, color = ~truth, text = ~text)

clipr::write_clip(sample(observed_dictionary, 50))
clipr::write_clip(sample(financial_dictionary$word, 50))



training_superlearner %>%
    bind_cols(ensemble = predict(superlearner, training_superlearner) %>% pull) %>% 
    select(truth, ensemble) %>% 
    GGally::ggpairs(aes(fill = truth))

predict_df %>% 
    bind_cols(class = str_remove(chat$filename, 'raw_data/oldchats/')) %>% 
    mutate(class = str_remove(class, '.txt')) %>% 
    count(user, estimated_engagement, class, .drop = FALSE) %>% 
    ggplot(aes(x = class, y = n, fill = estimated_engagement)) +
    geom_col() +
    scale_x_discrete(labels=NULL) +
    facet_wrap(~user)

predict_df %>% 
    dplyr::select(estimated_engagement, confidence_in_estimation, message, user)  %>% 
    bind_cols(predict_df$estimated_engagement_probs) %>% 
    rowwise() %>% 
    mutate(ensemble_score = 26*.pred_Alto + 2.6*.pred_Medio + .pred_Bajo) %>% 
    bind_cols(class = str_remove(chat$filename, 'raw_data/oldchats/')) %>% 
    mutate(class = str_remove(class, '.txt')) %>% 
    group_by(user, class) %>% 
    summarise(ensemble_score = sum(ensemble_score)) %>% 
    ggplot(aes(x = class, y = ensemble_score)) +
    geom_col() +
    scale_x_discrete(labels=NULL) +
    facet_wrap(~user)

predict_df %>% 
    dplyr::select(estimated_engagement, confidence_in_estimation, message, user)  %>% 
    bind_cols(predict_df$estimated_engagement_probs) %>% 
    rowwise() %>% 
    mutate(ensemble_score = 26*.pred_Alto + 2.6*.pred_Medio + .pred_Bajo) %>% 
    bind_cols(class = str_remove(chat$filename, 'raw_data/oldchats/')) %>% 
    mutate(class = str_remove(class, '.txt')) %>% 
    ggplot(aes(x = ensemble_score)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(~class)

predict_df %>% 
    group_by(user) %>% 
    count(estimated_engagement) %>% 
    ggplot(aes(x = fct_reorder(user, n, sum), y = n, fill = estimated_engagement)) +
    geom_col() +
    labs(y = 'n',
         x = 'user') +
    coord_flip() 

predict_df %>% 
    group_by(user) %>% 
    count(estimated_engagement) %>% 
    pivot_wider(names_from = estimated_engagement, values_from = n) %>% 
    replace_na(value = 0) %>% View
    clipr::write_clip()

user_tickets <- predict_df %>% 
    dplyr::select(estimated_engagement, confidence_in_estimation, message, user)  %>% 
    bind_cols(predict_df$estimated_engagement_probs) %>%
    bind_cols(class = str_remove(chat$filename, 'raw_data/oldchats/')) %>% 
    mutate(class = str_remove(class, '.txt')) %>% 
    rowwise() %>% 
    mutate(ensemble_score = 26*.pred_Alto + 2.6*.pred_Medio + .pred_Bajo) %>%
    ungroup() %>% 
    group_by(user) %>% 
    summarise(class_score = sum(ensemble_score),
              ) 
user_tickets %>% 
    ggplot(aes(x = fct_reorder(user, class_score), y = class_score)) +
    geom_col() +
    geom_label(aes(label = round(class_score))) +
    labs(y = 'Score sintético',
         x = 'User') +
    coord_flip()

user_tickets %>% 
    ggplot(aes(x = fct_reorder(user, class_score), y = class_score)) +
    geom_col(fill = 'white',  col = 'black') +
    annotate(y = median_notes_calibration$TotalInt + 15, x = 1, label = median_notes_calibration$Nota,
             geom = 'text', ) +
    labs(y = 'Score sintético',
         x = 'User') +
    geom_hline(yintercept = median_notes_calibration$TotalInt, color = 'red') +
    coord_flip()

user_tickets %>% 
    mutate(engagement = cut(class_score, breaks = quantile(.$class_score, c(0, 0.33, 0.66, 1)))) %>% 
    count(user, engagement) %>% 
    pivot_wider(names_from = engagement, values_from = n) %>% 
    print(n = Inf)

library(vip)
vip(superlearner)

final_real_grades %>% 
    skimr::skim()
