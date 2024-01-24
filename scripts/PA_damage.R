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
library(emmeans)

# data ####
damage_type <- PSA_PA_damage


# wrangling ####
damage_type

unique(damage_type$treatment)
(NA %in% damage_type$treatment)
# all of 2023 does not have a treatment number.. whoops

damage_type <- damage_type %>% 
  mutate(treatment = case_when(plot_id %in% c(101,203,304,401,503) ~ 1,
                               plot_id %in% c(103,204,302,403,501) ~ 2,
                               plot_id %in% c(102,201,303,402,502) ~ 3, 
                               plot_id %in% c(104,202,301,404,504) ~ 4))

unique(damage_type$treatment)
(NA %in% damage_type$treatment)

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
# sum(dmg_model$multiple)



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

###
##
#
# model other (singularity in the loop)
other <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, other))
other_m1 <- glmer(other ~ treatment +
                   (1|year), data = dmg_model,
                 family = binomial)
summary(other_m1)
r2_nakagawa(other_m1)
model_performance(other_m1)

#
##
###
# I am confused: my emmeans results shows a 0 in the treatment. But I cannot find it here... 
unique(dmg_model$treatment)
subset(dmg_model, !(0 %in% dmg_model$treatment))
dmg_model <- dmg_model %>% 
  filter(treatment != 0)

pest_columns <- c('bcw','s','sb', 'taw', 'multiple')
summary_list <- list()
r2_list <- list()
emms_mod <- list()
for (pest in 1:length(pest_columns)) {
  print(pest)
  new_col <- pest_columns[pest]
  new_df <- subset(dmg_model, select = c('year', 'growth_stage', 'block', 'treatment', new_col))
  colnames(new_df) <- c('year', 'growth_stage', 'block', 'treatment', 'new_col')
  model <- glmer(new_col ~ treatment +
                   (1|year/growth_stage/block), data = new_df,
                 family = binomial)
  emms_mod[[pest]] <- emmeans(model, pairwise ~ as.factor(treatment),type = "response")
  summary_model <- summary(model)
  summary_list[[pest]] <- summary_model
  r2_model <- r2_nakagawa(model)
  r2_list[[pest]] <- r2_model
}

summary_list
r2_list
emms_mod

saved_emmeans <- (emms_mod[[1]][emms_mod$emmeans])

# #multiple failed: test cuz my wrangling was not perfect
# mult_m1 <- glmer(multiple ~ treatment  +
#                    (1|year/growth_stage/block), data = dmg_model,
#                  family = binomial)


