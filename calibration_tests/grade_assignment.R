library(tidyverse)
library(plotly)
library(readxl)

median_notes_calibration <- read_excel("calibration_tests/median_notes_calibration.xlsx")


plot_ly(median_notes_calibration, x = ~H, y = ~M, z = ~L, color = ~Nota, text = ~Nota)

model <- lm(I(Nota-10) ~ TotalInt-1, data = median_notes_calibration)
summary(model)

user_tickets %>% 
    rowwise() %>% 
    mutate(interval = findInterval(class_score, median_notes_calibration$TotalInt[order(median_notes_calibration$TotalInt, decreasing = FALSE)]),
           grade = rev(median_notes_calibration$Nota)[interval],
           corrected = 10+class_score*0.01446,
           rounded_up = round(corrected+0.05)) %>% 
    filter(!(user %in% c('facilitador56 UP', 'Viviana Cotrina Viviana Purita Cotrina Verano',
                         'Ruben Angel Montenegro Lee', 'Not identifiable', 'Marcelo Villar Meza'))) %>% 
    arrange(desc(corrected)) %>% 
    clipr::write_clip()

REAT::gini(inequality_in_participation$percentage, lc = TRUE)

inequality_in_participation <- predict_df %>%
    group_by(user) %>% 
    count() %>% 
    arrange((n)) %>%
    ungroup() %>% 
    mutate(percentage = n/sum(.$n),
           cum_perc = cumsum(percentage)) 

inequality_in_participation %>% 
    ggplot(aes(x = seq_along(fct_reorder(factor(user), (percentage))), y = cum_perc)) +
    geom_col(fill = 'navy') +
    geom_abline(slope = 1/40, intercept = 0, color = 'red') +
    scale_x_discrete(labels=NULL) +
    geom_text(x = 5, y = 1, label = 'Gini: 0.506') +
    labs(y = 'Porcentaje acumulado',
         x = 'Alumno')

predict_df %>% 
    group_by(user, estimated_engagement) %>% 
    count() %>%
    ggplot() +
    geom_col(aes(x = reorder(user, n, sum), y = n, fill = estimated_engagement), position = 'stack') +
    scale_x_discrete(labels=NULL) +
    coord_flip() +
    labs(fill = 'Engagement',
         y = 'Interacciones',
         x = 'Alumno') +
    scale_fill_brewer(type = 'seq', palette = 3) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank())

plot(full_modelforest)
