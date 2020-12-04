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
        'Marcelo Villar Meza' = 'Marcelo Andres Villar Meza',
        'Fernandez Santiago' = 'Santiago Jose Fernandez Pizarro',
        'Sebastian Collantes' = 'Sebastian Enrique Collantes Liendo',
        'Alex 2' = 'Alexander Johan Pacaya Samame',
        'XIOMI GARCIA' = 'Xiomi Anny Garcia Palomino',
        "Xiomi GarcÃ­a" = 'Xiomi Anny Garcia Palomino',
        'Viviana Cotrina Viviana Purita Cotrina Verano' = 'Viviana Purita Cotrina Verano'
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


##### SUPPORT DATABASES ######

# Language of each session

session_language <- read.csv('raw_data/oldchats/idioma_de_clase.csv', stringsAsFactors = FALSE)
session_language$date <- as.Date(session_language$date, format = '%m/%d/%Y')

# merging with final df

chat <- merge(chat, session_language)
chat$language <- as.factor(chat$language)

##### SAVE CLEAN FILE ######

write.csv(chat, 'processed_data/clean_chat_data.csv', row.names = FALSE)
