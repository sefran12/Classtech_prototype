##### CHAT DATA CLEANING #####

# general
library(tidyverse)

# data cleaning
library(lubridate)

# graphics
library(patchwork)
library(ggpmisc)
library(GGally)
library(ggthemes)

# misc
library(skimr) # summarise dataframes

##### OPTION SETTING #####

theme_set(theme_clean())

##### READING DATA #####

chat <- read.csv('processed_data/chat_data.csv')
chat$X <- NULL

##### INCONSISTENCIES IN NAMES #####

chat %>% 
    select(user) %>% 
    unique() %>% 
    as.data.frame() %>% 
    arrange(user)

# replacements will be done manually.

replacements <- 
    c(
        'sherrerae' = 'Sebastian Alfonso Herrera Eyzaguirre',
        'anfres' = 'Andres Vilcahuaman Zermeno',
        'Bad bunny' = 'Not identifiable',
        'GAP' = 'Not identifiable',
        'Santiago fernandez #4' = 'Santiago Jose Fernandez Pizarro',
        'magaly' = 'Magaly Julisa Acuna Condori',
        'Miguel Prado$' = 'Edwin Miguel Prado Quispe',
        'Fernandez Santiago' = 'Santiago Jose Fernandez Pizarro',
        'Sebastian Collantes' = 'Sebastian Enrique Collantes Liendo',
        'Alex 2' = 'Alexander Johan Pacaya Samame',
        'XIOMI GARCIA' = 'Xiomi Anny Garcia Palomino',
        "Xiomi GarcÃ­a" = 'Xiomi Anny Garcia Palomino'
    )

chat$user <- str_replace_all(chat$user, replacements)

###### GET CLASS DATE ######

monthnames <- 
    c(
        'enero' = 'jan',
        'febrero' = 'feb',
        'marzo' = 'march', 
        'abril' = 'april',
        'mayo' = 'may',
        'junio' = 'june',
        'julio' = 'july',
        'agosto' = 'august',
        'setiembre' = 'september',
        'octubre' = 'october',
        'noviembre' = 'november',
        'diciembre' = 'december'
    )

chat$date <- chat$filename %>% 
    str_remove_all('.txt') %>% 
    str_remove('[:digit:]+_') %>% 
    str_replace_all('_', '/') %>% 
    str_replace_all(monthnames) %>% 
    dmy()

##### SAVE CLEAN FILE ######

write.csv(chat, 'processed_data/clean_chat_data.csv', row.names = FALSE)

##### sUPPORT DATABASES ######

# Language of each session

session_language <- data.frame(
    session = unique(chat$date),
    language = 'ingles'
)

#write.csv(session_language, 'AGRI/oldchats/idioma_de_clase.csv', row.names = FALSE)