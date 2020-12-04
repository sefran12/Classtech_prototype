#### OTHER STREAMS OF DATA ####

library(readr)

slack_interactions_agri2020_2 <- read_csv("processed_data/slack_interactions_agri2020_2.csv")

slack_interactions <- slack_interactions_agri2020_2 %>% 
    mutate(is_interaction = !is.na(files)|!is.na(upload)|!is.na(attachments)|str_detect(text, 'htt')) %>% 
    select(text, real_name, is_interaction) %>% 
    count(real_name, is_interaction) %>% 
    pivot_wider(values_from = n, names_from = is_interaction) %>% 
    `colnames<-`(c('user', 'normal_message', 'link_or_share'))

slack_interactions %>% 
    clipr::write_clip()
