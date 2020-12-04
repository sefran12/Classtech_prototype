#### ASSUMING THE EXISTENCE OF MODELING FRAMEWORK
library(readr)
library(tidyverse)
library(ggrepel)

inference_engine <- read_rds("processed_data/inference_engine.rds")

chat_data_fa <- read_csv("processed_data/chat_data_fa.csv")

##

data_to_predict <- 
    chat_data_fa %>% 
    select(user, message)

data_to_predict$numero_de_palabras <- str_count(data_to_predict$message, "\\S+")
data_to_predict$numero_de_caracteres <- str_length(data_to_predict$message)
data_to_predict$unique_letters <- sapply(data_to_predict$message,
                                         function(x){nchar(rawToChar(unique(charToRaw(str_remove(x, ' ')))))}
)
data_to_predict$message_complexity <- (data_to_predict$numero_de_palabras > 2)*1 +
    (data_to_predict$unique_letters > 5)*1 + 
    data_to_predict$unique_letters*0.1 + 
    log2(data_to_predict$numero_de_palabras)*0.25
data_to_predict$has_monetary <- str_detect(data_to_predict$message, "[\\$]|dolar|soles|S/|USD")

data_to_predict$penalized_words <- str_count(data_to_predict$message, penalized_words)
data_to_predict$is_question <- str_count(data_to_predict$message, question_indicators)
data_to_predict$relevancy_score <- str_count(data_to_predict$message, regex(relevancy_indicators))
data_to_predict$is_link <- str_detect(data_to_predict$message, link_indicator)

data_to_predict$numero_de_numericos <- str_count(data_to_predict$message, "[:digit:]")*!data_to_predict$is_link

##

stream_df <- data_to_predict %>% 
    select(-message, -user) %>% 
    mutate_if(is.logical, as.numeric)

scored_stream_df <- data_to_predict %>% 
    bind_cols(
        predict(inference_engine, stream_df)
    ) %>%
    cbind(predict(inference_engine, stream_df, 'prob')) %>% 
    mutate(score = 23*.pred_Alto + 2.6*.pred_Medio + .pred_Bajo) %>% 
    select(-c(numero_de_palabras:numero_de_numericos))

scored_stream_df %>% 
    group_by(user) %>% 
    summarise(
        class_score = sum(score, na.rm = TRUE)
    ) %>% 
    arrange(desc(class_score)) %>% 
    print(n = Inf)

###

median_notes_calibration <- read_excel("calibration_tests/median_notes_calibration.xlsx")
median_notes_calibration$TotalInt <- median_notes_calibration$TotalInt/3
model <- lm(I(Nota-12) ~ TotalInt-1, data = median_notes_calibration)
score_weight <- tidy(model)
score_weight <- score_weight$estimate

synthetic_grades <- scored_stream_df %>% 
    group_by(user) %>% 
    summarise(
        class_score = sum(score, na.rm = TRUE)
    ) %>% ungroup() %>% 
    mutate(synthetic_grade = class_score*score_weight + 12,
           rounded = round(synthetic_grade + 0.05)) %>% 
    arrange(desc(synthetic_grade))

synthetic_grades %>% 
    clipr::write_clip()
