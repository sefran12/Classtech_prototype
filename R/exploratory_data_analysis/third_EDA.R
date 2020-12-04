### THIRD EDA

library(tidyverse)
library(tableone)

###

all_training <- rbind(calib, new_data) %>% 
    select(-message)

table1 <- CreateTableOne(colnames(all_training), strata = 'value', data = all_training)
print(table1, smd = TRUE)
