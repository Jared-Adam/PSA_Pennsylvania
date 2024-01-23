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


# this one works: will need to adapt column names a bit with the whole df 
df <- spread(test, damage_type, damage_type)
look <- df %>% 
  dplyr::select(-na) %>%
  unite(multiple, c('s, bcw' , 's, taw'), sep = " ", remove = TRUE, na.rm = TRUE) %>% 
  mutate(multiple = case_when(multiple != "" ~ 1)) %>% 
  #dplyr::select(-'s, bcw', -'s, taw') %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         #multiple = case_when(multiple %in% c('s, bcw' , 's, taw' , 's, bcw s, taw') ~ 1),
         taw = case_when(taw == 'taw' ~ 1)) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0))
sum(look$multiple)

# try on full data set 
dmg <- damage_type

dmg <- spread(dmg, damage_type, damage_type)
colnames(dmg[9:28])
new_dmg <- dmg %>% 
  dplyr::select(-location, -tempC, -na) %>% 
  unite(multiple, c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                     "s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb"), sep = "-", remove = TRUE, na.rm = TRUE) %>% 
  mutate(multiple = case_when(multiple != "" ~ 1)) %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         taw = case_when(taw == 'taw' ~ 1),
         sb = case_when(sb == 'sb' ~ 1),
         d = case_when(d == 'd' ~ 1),
         t = case_when(t == 't' ~ 1),
         other = case_when(other == 'other' ~ 1),
         #multiple = case_when(multiple %in% c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                                               #"s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb") ~ 1),
         ) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0)) %>% 
  dplyr::select(-d, -t) %>% # I think these two are typos and I removed them 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y")) %>%
  dplyr::select(-date) %>% 
  relocate(year) %>% 
  mutate(growth_stage = as.factor(growth_stage),
         block = as.factor(block),
         treatment = as.factor(treatment),
         year = as.factor(year)) %>% 
  print(n = 10)
#check before removing 
# unique(dmg_new$d), sum(dmg_new$d)
sum(new_dmg$multiple)
sum(new_dmg$s)
sum(new_dmg$other)
sum(new_dmg$sb)
sum(new_dmg$taw)
sum(new_dmg$bcw)
# these all have numbers 

# models ####
# test model to look at variables before we run the loop
dmg_model <- new_dmg


test_m1 <- glmer(s ~ treatment + growth_stage +
                   (1|year/block/plot_id), data = dmg_model,
                 family = binomial)
summary(test_m1)
r2_nakagawa(test_m1)
hist(residuals(test_m1))

#this is the one 
test_m2 <- glmer(s ~ treatment  +
                   (1|year/growth_stage/block), data = dmg_model,
                 family = binomial)
summary(test_m2)
r2_nakagawa(test_m2)
hist(residuals(test_m2))

# m3 = overfot/ singular. Removing plot 
test_m3 <- glmer(s ~ treatment  +
                   (1|year/growth_stage/block/plot_id), data = dmg_model,
                 family = binomial)
summary(test_m3)
r2_nakagawa(test_m3)
hist(residuals(test_m3))


pest_columns <- c('bcw', 'multiple', 'other', 's', 'sb', 'taw')
summary_list <- list()
for (pest in 1:length(pest_columns)) {
  print(pest)
  new_col <- pest_columns[pest]
  new_df <- subset(dmg_model, select = c('year', 'growth_stage', 'block', 'treatment', new_col))
  colnames(new_df) <- c('year', 'growth_stage', 'block', 'treatment', 'new_col')
  model <- glmer(new_col ~ treatment +
                   (1|year/growth_stage/block), data = new_df,
                 family = binomial)
  summary_model <- summary(model)
  summary_list[[pest]] <- summary_model
}

#multiple failed 
mult_m1 <- glmer(multiple ~ treatment  +
                   (1|year/growth_stage/block), data = dmg_model,
                 family = binomial)

