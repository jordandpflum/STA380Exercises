rm(list=ls())

library(dplyr)

social_marketing <- read.csv('social_marketing.csv')

social_marketing_filtered <- social_marketing %>% filter(spam==0, 
                                                         adult<=5
                                                         )







