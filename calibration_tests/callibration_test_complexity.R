#### CALIBRATION TESTS #####

# Needs clean_chat_data.csv and tokenized_corpus

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

# output tables
library(gt)

# NLP
library(tidytext)
library(tokenizers)
library(syuzhet)
library(hunspell)

# misc
library(skimr) # summarise dataframes

#####

# Filtering irrelevant chats

set.seed(123)
chat %>% 
    filter(message_complexity > 2) %>% 
    mutate(message = str_to_lower(message)) %>% 
    filter(!str_detect(message, 'grac|chau|jaj|hola|buenas noch')) %>% 
    sample_n(size = 150) %>%
    write.csv('calibration_tests/relevancy_calibration_test.csv')

# Importing survey results
