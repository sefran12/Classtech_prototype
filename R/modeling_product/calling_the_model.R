transform_for_inference <- function(vector_of_chars){
    require(tidyverse)
    df <- data.frame(
        message = str_to_lower(vector_of_chars)
    )
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
    return(df)
}

predict_probs_class_and_soft_score <- function(new_data, engine, rarities){
    df <- predict(inference_engine, new_data = new_data, type = 'prob')
    df <- as.data.frame(df) %>% 
        mutate(
            soft_score = rarities[1]*.pred_Bajo + rarities[2]*.pred_Medio + rarities[3]*.pred_Alto,
            pred_class = unlist(predict(inference_engine, new_data = new_data)),
            message = new_data$message
        )
    return(df)
}

new_data <- c('cuando sera el parcial profe?', 'las finanzas son muy importantes y bonos',
              'the shares of enron have slowly fallen over the years', 'creo que si profe',
              'finanzas, bonos, prestamos y alquileres creo que es lo mas importante')
new_data <- transform_for_inference(new_data)
new_data
predict_probs_class_and_soft_score(new_data, inference_engine, c(1, 2.6, 27))



new_data <- chat_data_df$message
new_data <- transform_for_inference(new_data)
new_data
predict_probs_class_and_soft_score(new_data, inference_engine, c(1, 2.6, 27)) %>% 
    View()
