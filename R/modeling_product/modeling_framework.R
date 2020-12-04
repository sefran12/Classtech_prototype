### AGRI 2020-2 ###

library(tidyverse)
library(readxl)
library(patchwork)
library(stringi)
library(extrafont)

library(tidytext)
library(tm)

library(tidymodels)
library(nnet)
library(themis)
library(broom)
library(ranger)
library(xgboost)

### OPTIONS ###
theme_set(theme_bw() + theme(panel.grid = element_blank()))

### TRAINING MODEL ###


## CREATING TRAINING DATA

first_survey <- read_excel("calibration_tests/Calibracion de medidas de engagement (Responses).xlsx")[,-1]
second_survey <- read_excel("calibration_tests/Segunda encuesta de calibracion de medidas de engagement (Responses).xlsx")[,-1]
third_survey <- read_excel("calibration_tests/Tercera encuesta de calibracion de medidas de engagement (Responses).xlsx")[,-1]
fourth_survey <- read_excel("calibration_tests/Cuarta encuesta de calibracion de engagement (Responses).xlsx")[,-1]

invert_survey <- function(survey){
    df <- data.frame(
        message = str_remove_all(colnames(survey)[-1], 
                                 pattern = 'Por favor califique el nivel de engagement que considera que corresponde a cada interacción \\[|Intervenciones \\[') %>%
            str_remove_all('\\]') %>% str_to_lower() %>% 
            stri_trans_general(id = 'Latin-ASCII'),
        value = t(survey[1,-1])
    ) %>% na.omit()
    rownames(df) <- 1:nrow(df)
    return(df)
}

survey_data <- map(list(first_survey, second_survey, third_survey, fourth_survey),
                   invert_survey) %>% do.call(rbind, .)

## FEATURE ENGINEERING

# creating dictionaries

penalized_words <- c('prof|rub.*n|parcial|exam|PCs|la nota|buenos di|buenas no|buenas tar|chau|adios|nota|final|blackboard|punto')
penalized_words <- paste0(penalized_words, paste0(c('|profe', 'nota', 'examen', 'parcial', 
                                                    'final', 'pc', 'pd', 'exam', 'notas',
                                                    'dirigida', 'calificada', 'up', 'seminario', 'salon', 'ciclo',
                                                    'clases', 'semestre', 'parci', 'promedio',
                                                    'pensiones'), collapse = '|'), collapse = '|')

financial_dictionary <- read_excel("calibration_tests/financial_dictionary.xlsx", 
                                   col_names = 'word')
financial_dictionary <- str_split(financial_dictionary$word, ': ') %>% 
    simplify() %>% 
    as.data.frame() 

financial_dictionary <- financial_dictionary %>% 
    `colnames<-`('message') %>% 
    mutate(message = as.character(message)) %>% 
    unnest_tokens(word, message) %>% 
    mutate(word = stri_trans_general(str_to_lower(word), id = 'Latin-ASCII'),
           word = stemDocument(word, language = 'es')) %>% 
    filter(nchar(word) > 3)

tokenized_survey <- survey_data  %>% 
    mutate(message = as.character(message)) %>% 
    unnest_tokens(output = word, input = message) %>% 
    mutate(word = stri_trans_general(str_to_lower(word), id = 'Latin-ASCII'))

observed_dictionary <- tokenized_survey %>% 
    filter(value == 'Alto') %>% 
    dplyr::select(word) %>%
    mutate(word = stri_trans_general(str_remove(str_to_lower(word), '[:punct:]'), id = 'Latin-ASCII')) %>% 
    filter(!(word %in% stopwords('es')),
           !(str_detect(word, penalized_words)),
           !(str_detect(word, '[:digit:]+'))) %>% 
    rowwise() %>% 
    mutate(word = str_sub(word, 1, 5)) %>%
    count(word) %>% 
    arrange(desc(n)) 

observed_dictionary <- observed_dictionary %>% 
    filter(nchar(word) > 4) %>% 
    pull(word)

# applying dictionaries

relevancy_indicators <- c(financial_dictionary$word, observed_dictionary)
relevancy_indicators <- paste(relevancy_indicators, collapse = '|')
link_indicator <- paste0(c('http', '\\.com', 'www\\.', '\\.html'),
                         collapse = "|")
question_indicators <- paste0(c('\\?', 'por que', 'como asi', 'no entiendo', 'explica',
                                'una pregunta', 'pregunt'),
                              collapse = '|')

## CREATING FEATURES

# copy to df to use

df <- survey_data

# creating features

df$numero_de_palabras <- str_count(df$message, "\\S+")
df$numero_de_caracteres <- str_length(df$message)
df$unique_letters <- sapply(df$message,
                              function(x){nchar(rawToChar(unique(charToRaw(str_remove(x, ' ')))))}
)
df$message_complexity <- (df$numero_de_palabras > 2)*1 +
    (df$unique_letters > 5)*1 + 
    df$unique_letters*0.1 + 
    log2(df$numero_de_palabras)*0.25
df$has_monetary <- str_detect(df$message, "[\\$]|dolar|soles|S/|USD")

df$penalized_words <- str_count(df$message, penalized_words)
df$is_question <- str_count(df$message, question_indicators)
df$relevancy_score <- str_count(df$message, regex(relevancy_indicators))
df$is_link <- str_detect(df$message, link_indicator)

df$numero_de_numericos <- str_count(df$message, "[:digit:]")*!df$is_link

### SETTING UP VARS FOR THE MODEL

df <- juice(prep(df_recipe)) %>% 
    mutate_if(is.logical, as.numeric) %>%
    mutate(value = factor(value, levels = c('Bajo', 'Medio', 'Alto')))

### MODEL

df_split <- initial_split(df)
df_train <- training(df_split)
df_test <- testing(df_split)

ranger_model <- ranger(value ~ .,
                       data = df_train, importance = 'permutation')
ranger_preds <- predict(inference_engine, df_test)
table(df_test$value, ranger_preds$.pred_class)
data.frame(
    truth = df_test$value, 
    pred = ranger_preds$.pred_class
) %>% 
    metrics(truth, pred)

data.frame(
    truth = df_test$value, 
    pred = ranger_preds$.pred_class
) %>% 
    metrics(truth, pred)

### 75% ~ 80% accuracy on test data.

######## TRAINING FINAL MODEL ########

# according to crossvalidation, we have a mtry of 3 and min_n of 40 for max metrics

inference_engine <- 
    forest_reg <- rand_forest(mode = 'classification',
               mtry = 3,
               min_n = 40) %>%
    set_engine("ranger",
               importance = 'permutation',
               keep.inbag=TRUE) %>% 
    fit(value ~., data = df)

write_rds(inference_engine, path = "processed_data/inference_engine.rds")

## VISUALIZING THE INFERENCE ENGINE

inference_engine %>% 
    predict(df, 'prob') %>% 
    cbind(value = df$value) %>% 
    roc_auc(value, .pred_Bajo:.pred_Alto)

inference_engine %>% 
    predict(df, 'prob') %>% 
    cbind(value = df$value) %>% 
    GGally::ggpairs(aes(fill = value, alpha = 0.7))

data.frame(
    truth = df_test$value, 
    pred = ranger_preds$.pred_class
) %>% 
    co(truth, pred)

inference_engine %>% 
    predict(df) %>% 
    cbind(df) %>% 
    GGally::ggpairs(aes(fill = value, color = value, alpha = 0.7))
