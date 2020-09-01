library(readxl)
library(tidyverse)
library(broom)

calib <- read_excel("calibration_tests/Segunda encuesta de calibracion de medidas de engagement (Responses).xlsx")
calib <- cbind(calib, read_excel("calibration_tests/Calibracion de medidas de engagement (Responses).xlsx")[,-1])

calib <- data.frame(
    message = str_remove_all(colnames(calib)[-1], 
                             pattern = 'Por favor califique el nivel de engagement que considera que corresponde a cada interacción \\[') %>%
        str_remove_all('\\]'),
    value = t(calib[1,-1])
)

colnames(calib) <- c('message', 'value')
calib$numero_de_palabras <- str_count(calib$message, "\\S+")
calib$unique_letters <- sapply(calib$message,
                              function(x){nchar(rawToChar(unique(charToRaw(str_remove(x, ' ')))))}
)
calib$message_complexity <- (calib$numero_de_palabras > 2)*1 +
    (calib$unique_letters > 5)*1 + 
    calib$unique_letters*0.1 + 
    log2(calib$numero_de_palabras)*0.25

calib$value <- factor(calib$value, levels = c('Bajo', 'Medio', 'Alto'))

calib %>% 
    ggplot(aes(x = value, y = message_complexity)) +
    geom_boxplot()

calib %>% 
    ggplot(aes(fill = value, x = message_complexity)) +
    geom_density(alpha = 0.5)

calib %>% 
    ggplot(aes(col = value, x = unique_letters, y = numero_de_palabras, shape = value)) +
    geom_jitter() +
    geom_density2d() +
    facet_wrap(~value)

calib %>% 
    glm(data = ., formula = value =='Alto' ~ message_complexity, family = binomial) %>%
    tidy() %>% 
    select(term, estimate) %>% 
    mutate(oddratio = exp(estimate))

calib %>% 
    glm(data = ., formula = value=='Alto' ~ numero_de_palabras, family = binomial) %>%
    tidy() %>% 
    select(term, estimate) %>% 
    mutate(oddratio = exp(estimate))

calib %>% 
    glm(data = ., formula = value=='Alto' ~ unique_letters, family = binomial) %>%
    tidy() %>% 
    select(term, estimate) %>% 
    mutate(oddratio = exp(estimate))

calib %>% 
    glm(data = ., formula = value=='Alto' ~ unique_letters*numero_de_palabras, family = binomial) %>%
    tidy() %>% 
    select(term, estimate) %>% 
    mutate(oddratio = exp(estimate))