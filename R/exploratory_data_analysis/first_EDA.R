##### INITIAL CHAT ANALYSIS ####

# general
library(tidyverse)

# extra tidying
library(lubridate)
library(stringi)
library(xts)

# graphics
library(patchwork)
library(ggpmisc)
library(GGally)
library(ggthemes)
library(ggrepel)
library(plotly)
library(wordcloud2)

# output tables
library(gt)

# Modeling
library(tidymodels)

# NLP  
library(tidytext)
library(tokenizers)
library(syuzhet)
library(hunspell)
library(tm)
library(sjmisc)

# misc
library(skimr) # summarise dataframes

##### OPTION SETTING #####

theme_set(theme_minimal())

##### READING DATA #####

chat <- read.csv('processed_data/clean_chat_data.csv', stringsAsFactors = FALSE, encoding = "UTF-8")

# formatting as needed

chat$date <- chat$date %>% ymd

##### INITIAL SKIMMING #####

skim(chat)

##### EDA 1 #####

chat %>% 
    group_by(user) %>% 
    
    count() %>% 
    arrange(desc(n)) %>% 
    ggplot(aes(x = fct_reorder(user, n),
               y = n)) +
    geom_col(fill = 'dark red') +
    coord_flip() +
    labs(x = NULL,
         y = 'Intervenciones')

chat %>% 
    group_by(user) %>% 
    count() %>% 
    ungroup() %>% 
    select(n) %>% 
    REAT::gini()


##### ANALISIS DE LOS COMENTARIOS #####

chat$numero_de_palabras <- str_count(chat$message, "\\S+")
chat$unique_letters <- sapply(chat$message,
                              function(x){nchar(rawToChar(unique(charToRaw(str_remove(x, ' ')))))}
)
chat$message_complexity <- (chat$numero_de_palabras > 2)*1 +
    (chat$unique_letters > 5)*1 + 
    chat$unique_letters*0.1 + 
    log2(chat$numero_de_palabras)*0.25

##### EDA 2 #####

colplot_words_per_user <- chat %>% 
    group_by(user) %>% 
    summarise(total_de_palabras = sum(numero_de_palabras, na.rm = TRUE)) %>% 
    ggplot(aes(x = fct_reorder(user, total_de_palabras),
               y = total_de_palabras)) +
    geom_col(fill = 'dark red') +
    coord_flip() +
    labs(x = NULL,
         y = 'Total de palabras')

chat %>% 
    group_by(date) %>% 
    summarise(total_de_palabras = sum(numero_de_palabras)) %>% 
    ggplot(aes(x = factor(date),
               y = total_de_palabras)) +
    geom_col(fill = 'dark red') +
    coord_flip() +
    labs(title = 'Palabras por clase',
         x = NULL,
         y = 'Intervenciones')

chat %>% 
    group_by(date) %>% 
    summarise(total_de_intervenciones = n()) %>% 
    ungroup() %>% 
    ggplot(aes(x = seq_along(date),
               y = total_de_intervenciones)) +
    geom_line(linetype = 2) +
    geom_point(size = 2) + 
    scale_x_continuous(breaks = 1:26, label = unique(chat$date)) +
    labs(title = 'Intervenciones por clase',
         x = NULL,
         y = 'Intervenciones')

chat %>% 
    group_by(user) %>% 
    summarise(complejidad_media = mean(message_complexity, na.rm = TRUE)) %>% 
    ggplot(aes(x = fct_reorder(user, complejidad_media),
               y = complejidad_media)) +
    geom_col(fill = 'dark red') +
    coord_flip() +
    labs(x = NULL,
         y = 'Complejidad media')

boxplot_message_complexity_per_user <- chat %>% 
    group_by(user) %>% 
    mutate(complejidad_media = mean(message_complexity, na.rm = TRUE),
           total_de_palabras = sum(numero_de_palabras, na.rm = TRUE)) %>% 
    ggplot(aes(x = fct_reorder(user, total_de_palabras),
               y = message_complexity)) +
    geom_boxplot(fill = 'dark red', alpha = 0.5) +
    coord_flip() +
    labs(x = NULL,
         y = 'Complejidad media de la intervención')

colplot_words_per_user | boxplot_message_complexity_per_user

str_split(chat$user,  ' ', simplify = TRUE)

scatterplot_intervenciones_vs_complejidad_usuario <- 
    chat %>% 
    group_by(user) %>% 
    summarise(complejidad_media = mean(message_complexity, na.rm = TRUE),
              total_de_palabras = sum(numero_de_palabras, na.rm = TRUE),
              total_de_intervenciones = n()) %>% 
    mutate(first_name = str_split(user, ' ', simplify = TRUE)[,1],
           second_name = str_split(user, ' ', simplify = TRUE)[,2]) %>%
    unite(short_name, c('first_name', 'second_name'), sep = ' ') %>% 
    ggplot(aes(x = total_de_intervenciones,
               y = complejidad_media,
               label = short_name)) +
    geom_text() +
    labs(x = 'Total de intervenciones',
         y = 'Complejidad media de la intervención')

scatterplot_intervenciones_vs_complejidad_usuario
ggplotly(scatterplot_intervenciones_vs_complejidad_usuario)

chat %>% 
    group_by(date, language) %>% 
    summarise(total_de_intervenciones = n()) %>% 
    ggplot(aes(x = factor(date),
               y = total_de_intervenciones,
               fill = language)) +
    geom_col() +
    coord_flip() +
    labs(title = 'Intervenciones por clase',
         x = NULL,
         y = 'Intervenciones')

intervention_amount_per_class_language <- chat %>% 
    group_by(date, user, language) %>% 
    summarise(number_of_user_interventions = n()) %>% 
    ggplot(aes(x = number_of_user_interventions,
               fill = factor(language, labels = c('español', 'inglés')))) +
    geom_density(alpha = 0.7) +
    labs(title = 'Intervenciones por clase',
         x = 'Número de intervenciones',
         y = 'Densidad',
         fill = 'Lenguaje')

intervention_amount_per_class_language_boxplot <- chat %>% 
    group_by(date, user, language) %>% 
    summarise(number_of_user_interventions = n()) %>% 
    ggplot(aes(y = number_of_user_interventions,
               group = factor(language, labels = c('español', 'inglés')),
               fill = factor(language, labels = c('español', 'inglés')))
    ) +
    geom_boxplot(alpha = 0.7) +
    labs(title = 'Intervenciones por clase',
         y = 'Número de intervenciones',
         x = 'Lenguage',
         fill = 'Lenguaje') 

intervention_complexity_per_class_language <- chat %>% 
    group_by(date, user, language) %>% 
    summarise(mean_complexity = mean(message_complexity, na.rm = TRUE)) %>% 
    ggplot(aes(x = mean_complexity,
               fill = factor(language, labels = c('español', 'inglés')))) +
    geom_density(alpha = 0.7) +
    labs(title = 'Complejidad de las intervenciones por clase',
         x = 'Complejidad de las intervenciones',
         y = 'Densidad',
         fill = 'Lenguaje') 

intervention_complexity_per_class_language_boxplot <- chat %>% 
    group_by(date, user, language) %>% 
    summarise(mean_complexity = mean(message_complexity, na.rm = TRUE)) %>% 
    ggplot(aes(y = mean_complexity,
               group = factor(language, labels = c('español', 'inglés')),
               fill = factor(language, labels = c('español', 'inglés')))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = 'Complejidad de las intervenciones por clase',
         y = 'Complejidad de las intervenciones',
         x = 'Lenguaje',
         fill = 'Lenguaje') 

intervention_amount_per_class_language + intervention_complexity_per_class_language +
    intervention_amount_per_class_language_boxplot + intervention_complexity_per_class_language_boxplot +
    plot_layout(guides = 'collect')

##### SENTIMENT ANALYSIS ON CHAT DATA #####

## suggesting corrected words

corpus_ <- chat %>% 
    select(date, ï.., timestamp.start, timestamp.end, user, message)

tokenized_corpus <- corpus_ %>% 
    unnest_tokens(output = word, input = message)

tokenized_corpus$word[1:100] %>% 
    get_sentiment(method = "nrc", language = "spanish")

esp <- dictionary("Spanish")                                                                

#semiclean_corpus <- tokenized_corpus$word %>% 
#    hunspell_suggest(dict = esp) 

corrected_corpus <- semiclean_corpus %>% 
    sapply(function(x){x[1]})

corrected_corpus <- corrected_corpus %>% 
    hunspell_stem(dict = esp) %>% 
    sapply(function(x){ifelse(is.null(x[1]), NA, x[1])})

##

tokenized_corpus$suggested_stem <- corrected_corpus

tokenized_corpus$suggested_score <- tokenized_corpus$suggested_stem %>% 
    get_sentiment(method = "nrc", "spanish")

tokenized_corpus$suggested_score2 <-
    apply(tokenized_corpus[,c("word", "suggested_score")], MARGIN = 1,
          function(x) ifelse(str_detect(x[1], pattern = "jaj"), 1, x[2]))

tokenized_corpus$suggested_score2 <- as.numeric(tokenized_corpus$suggested_score2)

#### Save tokenized corpus ####

write.csv(tokenized_corpus, "processed_data/tokenized_corpus.csv")

#### How did emotions evolve with time?

class_beginnings <- tokenized_corpus %>% 
    count(date) %>% 
    pull(n) %>% 
    cumsum()

class <- sapply(1:26, FUN = function(x) rep.int(x, times = class_beginnings[x]))

# we get a reasonable bandwidth for the plot by using
# the mean words in interventions per session per user multiplied
# by the mean number of users in any given class.
# so approx. 25 words are written by the mean student
# and there is an approx. mean of 25 students that participate
# in each class

reasonable_chat_bandwidth <- ( (tokenized_corpus %>% 
    group_by(date, user) %>% 
    summarise(interventions = n()) %>% 
    pull(interventions) %>% 
    mean(na.rm = TRUE)) * (tokenized_corpus %>% 
    group_by(date) %>% 
    summarise(distinct_users = n_distinct(user)) %>% 
    pull(distinct_users) %>% 
    mean(na.rm = TRUE)) ) %>% 
    ceiling()

mean_course_sentiment <-
    rollapply(tokenized_corpus$suggested_score2, FUN =  mean,
              width = reasonable_chat_bandwidth, fill = "extend")

plot.ts(mean_course_sentiment,
        ylim = c(-0.1, 0.3),
        xlim = c(-4000, 17000),
        xlab = "Desarrollo del curso",
        ylab = "Sentimiento promedio de la clase")
abline(a = 0, b = 0, col = "red")
abline(a = 0.05, b = 0, col = "orange")
abline(a = 0.1, b = 0, col = "green")
text(x = -3000, y = -0.05, label = "Negativo", col = "red")
text(x = -3000, y = 0.025, label = "Neutral", col = "orange")
text(x = -3000, y = 0.075, label = "Positivo", col = "green")
text(x = -3000, y = 0.15, label = "Muy Positivo", col = "dark green")
abline(v = class_beginnings, lty = 2, col = "gray")

simple_plot(tokenized_corpus$suggested_score2)

chat$date %>% unique() %>% length()

chat %>% 
    select(Fecha = date, Orden = X, timestamp.start, timestamp.end, user, message, filename) %>% 
    summarise()
    gt() %>% 
    fmt_missing(columns = 1:21, missing_text = '-')

write.csv(tokenized_corpus, 'processed_data/tokenized_corpus.csv')
write.csv(chat, 'processed_data/augmented_chat_data.csv')


class_beginnings <- tokenized_corpus %>% 
    count(date) %>% 
    pull(n) %>% 
    cumsum()

mean_sentiment <-
    rollapply(tokenized_corpus$suggested_score2, FUN =  mean,
              width = 25*25, fill = "extend")

mean_sentiment <- data.frame(
    mean_sentiment = sapply(split(mean_sentiment, ceiling(seq_along(mean_sentiment)/20)),
       mean, na.rm = TRUE)
    )

plt <- mean_sentiment %>% 
    ggplot(aes(x = seq_along(mean_sentiment), y = mean_sentiment)) +
    ylim(c(-0.1, 0.3))+
    xlim(c(-300, 850)) +
    xlab("Desarrollo del curso") +
    ylab("Sentimiento promedio de la clase") +
    geom_line() +
    geom_abline(intercept = 0, slope =  0, col = "red") +
    geom_abline(intercept =  0.05, slope = 0, col = "orange") +
    geom_abline(intercept = 0.1, slope = 0, col = "green") +
    geom_text(x = -200, y = -0.05, label = "Negativo", col = "red") +
    geom_text(x = -200, y = 0.025, label = "Neutral", col = "orange") +
    geom_text(x = -200, y = 0.075, label = "Positivo", col = "green") +
    geom_text(x = -200, y = 0.15, label = "Muy Positivo", col = "dark green") +
    geom_vline(xintercept = class_beginnings/20, lty = 2, col = "gray")

ggplotly(plt)

# RECALIBRACION: 3 MEDIDAS: COMPLEJIDAD, RELEVANCIA, PENALIZACIONES


# DERIVING RELEVANT WORDS
library(readxl)
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

tokenized_calib <- calib %>% 
    rbind(new_data) %>% 
    mutate(message = as.character(message)) %>% 
    unnest_tokens(output = word, input = message) %>% 
    mutate(word = stri_trans_general(str_to_lower(word), id = 'Latin-ASCII'))
    
observed_dictionary <- tokenized_calib %>% 
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

###

relevancy_indicators <- c(financial_dictionary$word, observed_dictionary)
relevancy_indicators <- paste(relevancy_indicators, collapse = '|')
link_indicator <- paste0(c('http', '\\.com', 'www\\.', '\\.html'),
                         collapse = "|")
question_indicators <- paste0(c('\\?', 'por que', 'como asi', 'no entiendo', 'explica',
                         'una pregunta', 'pregunt'),
                         collapse = '|')
    
chat$penalized_words <- str_count(chat$message, penalized_words)
chat$is_question <- str_count(chat$message, question_indicators)
chat$relevancy_score <- str_count(chat$message, regex(relevancy_indicators))
chat$is_link <- str_detect(chat$message, link_indicator)

calib$penalized_words <- str_count(calib$message, penalized_words)
calib$is_question <- str_count(calib$message, question_indicators)
calib$relevancy_score <- str_count(calib$message, regex(relevancy_indicators))
calib$is_link <- str_detect(calib$message, link_indicator)
    
glmodel <- glm(data = calib, value == "Bajo" ~ message_complexity + penalized_words*relevancy_score*is_question + is_link,
    family = binomial)
glmodel %>% tidy() %>% 
    mutate(oddratio = exp(estimate))

lm(data = calib, as.numeric(value) ~ message_complexity + numero_de_palabras + is_question + penalized_words + relevancy_score + is_link*unique_letters) %>%
    tidy()

synthetic_score <- lm(data = calib, as.numeric(value) ~ message_complexity + numero_de_palabras + is_question + penalized_words + relevancy_score + is_link*unique_letters) %>% 
    predict() %>% 
    cbind(calib$value) %>%
    cbind(as.character(calib$message)) %>% 
    `colnames<-`(c('score', 'truth', 'message')) %>% 
    as.data.frame() %>% 
    mutate(score = as.numeric(score))

synthetic_score %>% 
    ggplot(aes(x=truth, y = score)) +
    geom_violin()

calib %>% 
    ggplot(aes(x = numero_de_palabras, y = unique_letters, color = relevancy_score)) +
    geom_point() +
    facet_wrap(~value)

result_sensit <- function(x){
    a <- data.frame(
        preds = predict(glmodel, type = 'response') > x,
        truth = glmodel$data$value == "Bajo"
        )
    
    return(sum(a$preds == a$truth)/nrow(a))
}
    
sapply(seq(0.01, 1, length.out = 100), result_sensit) %>% 
    plot()
    
data.frame(
    preds = predict(glmodel, type = 'response'),
    truth = glmodel$data$value == "Bajo"
) %>% 
    ggplot(aes(x = truth, y = preds)) +
    geom_boxplot()

predict(glmodel) %>% 
    View()
