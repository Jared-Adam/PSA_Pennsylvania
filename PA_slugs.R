# slugs baby lettuce goooo 

# packages ####
library(tidyverse)

# data ####
slugs <- PSA_PA_slugs

# clean this jawn ####
slugs

# cleaning_the_slug <-
  
colnames(slugs)
test_slug <- slugs[1:200,]
test_slug %>% 
  select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  select(-date, -precip) %>%
  group_by(year, month, plot_id, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count)) %>% 
  print(n = Inf)
?summarise
# this is not working correctly... 


slugs %>% 
  filter(month == 'june') %>% 
  print(n = Inf)
