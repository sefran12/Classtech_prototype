##### INITIAL CHAT ANALYSIS ####

# general
library(tidyverse)

# extra tidying
library(lubridate)
library(stringi)

# graphics
library(patchwork)
library(ggpmisc)
library(GGally)
library(ggthemes)
library(ggrepel)
library(plotly)

# NLP
library(tidytext)
library(tokenizers)

# misc
library(skimr) # summarise dataframes


##### OPTION SETTING #####

theme_set(theme_clean())

##### READING DATA #####

chat <- read.csv('processed_data/clean_chat_data.csv', stringsAsFactors = FALSE)
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
    ggplot(aes(x = factor(date),
               y = total_de_intervenciones)) +
    geom_col(fill = 'dark red') +
    coord_flip() +
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
