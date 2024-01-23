# Jared Adam 
# PA PSA Damage type assessment of corn
# 1/23/2024
# two collection timings: v3 and v5
# binomial data? 



# packages ####
library(tidyverse)
library(lme4)
library(MASS)
library(performance)

# data ####
damage_type <- PSA_PA_damage

# wrangling ####
damage_type
unique(damage_type$damage_type)
# we need to create a new column for each damage type. 
# this will be accomplished by splitting the damage into new columns 

test <- damage_type[1:300,]
unique(test$damage_type)
# 
# new <- test %>% 
#   separate(damage_type, c('one', 'two', 'three'))
# 
# new_test <- test %>%
#   mutate(ind = row_number()) %>% 
#   separate_rows(damage_type, sep =",") %>% 
#   mutate(damage_type = ifelse(is.na(damage_type), 0 , damage_type)) %>% 
#   count(ind, damage_type) %>% 
#   spread(damage_type, n , fill = 0) %>% 
#   dplyr::select(-2)
# 
# test %>%
#   mutate(slug = case_when(damage_type == 's' ~ '1'),
#          bcw = case_when(damage_type == 'bcw' ~ '1')) %>% 
#   print(n = Inf)

# this one works: will need to adapt column names a bit with the whole df 
df <- spread(test, damage_type, damage_type)
df %>% 
  dplyr::select(-na) %>%
  unite(multiple, c('s, bcw' , 's, taw'), sep = " ", remove = FALSE) %>% 
  dplyr::select(-'s, bcw', -'s, taw') %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         multiple = case_when(multiple %in% c('s, bcw' , 's, taw' , 's, bcw s, taw') ~ 1),
         taw = case_when(taw == 'taw' ~ 1)) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0))


# try on full data set 
dmg <- damage_type

dmg <- spread(dmg, damage_type, damage_type)
colnames(dmg[9:28])
new_dmg <- dmg %>% 
  dplyr::select(-location, -tempC, -na) %>% 
  unite(multiple, c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                     "s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb"), sep = "", remove = TRUE) %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         taw = case_when(taw == 'taw' ~ 1),
         sb = case_when(sb == 'sb' ~ 1),
         d = case_when(d == 'd' ~ 1),
         t = case_when(t == 't' ~ 1),
         other = case_when(other == 'other' ~ 1),
         multiple = case_when(multiple %in% c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                                               "s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb") ~ 1),
         ) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0)) %>% 
  dplyr::select(-d, -t) %>% # I think these two are typos and I removed them 
  print(n = 200)
#check before removing 
# unique(dmg_new$d), sum(dmg_new$d)





