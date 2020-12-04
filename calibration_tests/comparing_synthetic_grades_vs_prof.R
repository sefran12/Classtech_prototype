### COMPARING PREDICTIONS VS PROFESSOR JUDGEMENT

library(tidyverse)
library(readxl)
library(extrafont)
library(tidypredict)

font_import()
theme_set(theme_bw(base_family = "Times New Roman") + theme(panel.grid = element_blank()))

final_real_grades <- read_excel("calibration_tests/final_real_grades.xlsx")
GGally::ggpairs(final_real_grades %>% 
                    select(-c(Nombre1:Apellido2)))


## LINEAR MODEL

linmod <- final_real_grades %>% 
    lm(engagmentMitad ~ corrected - 1, data = .)

linmod %>% 
    summary()

par(mfrow = c(2, 3))
quadmod %>% 
    plot(which = 1:6)
par(mfrow = c(1, 1))

## QUADRATIC MODEL

quadmod <- final_real_grades %>% 
    lm(engagmentMitad ~ corrected + I(corrected^2) - 1, data = .)

par(mfrow = c(2, 3))
quadmod %>% 
    plot(which = 1:6)
par(mfrow = c(1, 1))

## DEVIATIONS FROM CONSTANT UNIT EQUIVALENCY

offsetmod <- final_real_grades %>% 
    lm(engagmentMitad ~ offset(1*corrected) - 1, data = .)

summary(offsetmod) 

par(mfrow = c(2, 3))
offsetmod %>% 
    plot(which = 1:6)
par(mfrow = c(1, 1))

residuals(offsetmod)
hatvalues(offsetmod)
predict(offsetmod)

## DEVIATIONS

deviance(linmod)
deviance(offsetmod)
deviance(quadmod)

## ANALYSING RESIDUALS

final_real_grades %>% 
    mutate(residuals = engagmentMitad - corrected,
           sqresiduals = residuals ^ 2) %>% 
    GGally::ggpairs(columns = 5:ncol(.))


final_real_grades %>% 
    mutate(residuals = engagmentMitad - corrected,
           sqresiduals = residuals ^ 2) %>% 
    arrange(desc(sqresiduals)) %>% 
    View()

predict_df %>%  
    View()

chat
###
library(ggpmisc)
library(patchwork)

formula <- y ~ offset(x) + 0
final_real_grades_corrected <- final_real_grades %>% 
    rowwise() %>% 
    mutate(corrected = min(corrected, 20),
           rounded_up = round(corrected + 0.05))

notr_ <- final_real_grades%>% 
    ggplot(aes(y = engagmentMitad, x = corrected)) +
    geom_point() +
    stat_fit_deviations(method = "lm", formula = formula, colour = "red") +
    geom_smooth(method = "lm", formula = formula) + 
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  
    geom_abline(slope = 1, intercept = 0, color = 'black', linetype = 2) +
    geom_abline(slope = 0, intercept = 20, color = 'black', linetype = 2) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(label = comma_format(accuracy = 1)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    labs(x = 'Infergrader',
         y = 'Profesor',
         subtitle = 'Sin truncar notas > 20') +
    coord_equal()

tr_ <- final_real_grades_corrected %>% 
    ggplot(aes(y = engagmentMitad, x = corrected)) +
    geom_point() +
    stat_fit_deviations(method = "lm", formula = formula, colour = "red") +
    geom_smooth(method = "lm", formula = formula) + 
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  
    geom_abline(slope = 1, intercept = 0, color = 'black', linetype = 2) +
    geom_abline(slope = 0, intercept = 20, color = 'black', linetype = 2) +
    geom_vline(xintercept = 20, color = 'black', linetype = 2) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(label = comma_format(accuracy = 1)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    labs(x = 'Infergrader',
         y = NULL,
         subtitle = 'Truncando notas > 20') +
    coord_equal()

notr_ + tr_

#### GREAT DISCREPANCIES:

# WHY IS THERE BIAS TO HIGH?

final_real_grades_corrected %>% 
    ggplot(aes(y = engagmentMitad, x = corrected)) +
    geom_point() +
    stat_fit_deviations(method = "lm", formula = formula, colour = "red") +
    geom_smooth(method = "lm", formula = formula) + 
    geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE) +
    stat_poly_eq(aes(label = stat(eq.label)),
                 eq.with.lhs = "italic(prof)~`=`~",
                 eq.x.rhs = "~italic(infer)",
                 formula = y ~ x, parse = TRUE) +
    geom_abline(slope = 1, intercept = 0, color = 'black', linetype = 2) +
    geom_abline(slope = 0, intercept = 20, color = 'black', linetype = 2) +
    geom_vline(xintercept = 20, color = 'black', linetype = 2) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(label = comma_format(accuracy = 1)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    labs(x = 'Infergrader',
         y = NULL,
         subtitle = 'Truncando notas > 20') +
    coord_equal()

### UNCERTAINTY IN PREDICTIONS

predict(full_modelforest, new_data = chat %>% 
            mutate_if(is.logical, as.numeric), type = 'conf_int')

scores_with_uncertainty <- predict_df %>% 
    dplyr::select(estimated_engagement, confidence_in_estimation, message, user)  %>% 
    bind_cols(predict_df$estimated_engagement_probs) %>%
    bind_cols(class = str_remove(chat$filename, 'raw_data/oldchats/')) %>% 
    mutate(class = str_remove(class, '.txt')) %>% 
    rowwise() %>% 
    mutate(ensemble_score = 26*.pred_Alto + 2.6*.pred_Medio + .pred_Bajo) %>%
    ungroup() %>% 
    group_by(user) %>% 
    summarise(class_score = sum(ensemble_score),
              class_uncertainty = mean(confidence_in_estimation*n())
    ) %>% 
    mutate(
        upper_conf = class_score + class_uncertainty,
        lower_con = class_score - class_uncertainty
    )


scores_with_uncertainty %>% 
    rowwise() %>% 
    mutate(interval = findInterval(class_score, median_notes_calibration$TotalInt[order(median_notes_calibration$TotalInt, decreasing = FALSE)]),
           grade = rev(median_notes_calibration$Nota)[interval],
           corrected = 10+class_score*0.01446,
           corrected_low = 10+lower_con*0.01446,
           corrected_high = 10+upper_conf*0.01446,
           rounded_up = round(corrected+0.05)) %>% 
    filter(!(user %in% c('facilitador56 UP', 'Viviana Cotrina Viviana Purita Cotrina Verano',
                         'Ruben Angel Montenegro Lee', 'Not identifiable', 'Marcelo Villar Meza'))) %>% 
    arrange(desc(corrected)) %>% 
    write.csv('calibration_tests/synthetic_grades_with_uncertainty.csv')

# plotting uncertainty

final_real_grades %>% 
    ggplot(aes(y = jitter(engagmentMitad), x = corrected)) +
    geom_smooth(method = "lm", formula = formula) + 
    geom_abline(slope = 1, intercept = 0, color = 'black', linetype = 2) +
    geom_abline(slope = 0, intercept = 20, color = 'black', linetype = 2) +
    geom_pointrange(aes(xmin = corrected_low, xmax =  corrected_high), alpha = 0.5, width = 5) +
    geom_vline(xintercept = 20, color = 'black', linetype = 2) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(label = comma_format(accuracy = 1)) +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    labs(x = 'Infergrader',
         y = 'Profesor',
         subtitle = 'Incluyendo incertidumbre de la predicción') +
    coord_equal()
