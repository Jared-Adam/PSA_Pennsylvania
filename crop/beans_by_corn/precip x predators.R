# precipitation x predators
# 3/10/2024
# from a question in my talk

# packages 
library(tidyverse)
library(lme4)
library(emmeans)
library(performance)

# data ####
bean_pf
corn_pf
PSA_PA_slugs
weather <- PSA_PA_weather

# pf wrangling ####

# KARN #

cpf_wide <- corn_pf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(cpf_wide)
cpf_wide <- cpf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lyn = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph= Staphylinidae + Staphylinidaa) %>% 
  dplyr::select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(cpf_wide)

cpf_clean <- cpf_wide %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                                   plot %in% c(103,204,302,403,501) ~ 2,
                                   plot %in% c(102,201,303,402,502) ~ 3, 
                                   plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%
  print(n = Inf)
colnames(cpf_clean)

c_clean <- cpf_clean %>% 
  rename(Lyniphiidae = Lyn,
         Staphylinidae = Staph, 
         Tetragnathidae = Tetrgnathidae) %>% 
  mutate(Carabidae_new = Carabidae + Cicindelidae,
         Gryll = Gryllidae +Gyrillidae) %>% 
  dplyr::select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
  rename(Carabidae = Carabidae_new,
         Gryllidae = Gryll) %>%
  print(n = Inf) 



# BEAN#

bpf_wide <- bean_pf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(bpf_wide)
bpf_wide <- bpf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiide + Lyniphiidae + Linyphiidae) %>% 
  dplyr::select(-Liniphiide, -Lyniphiidae, -Linyphiidae, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(bpf_wide)

bpf_clean <- bpf_wide %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                                   plot %in% c(103,204,302,403,501) ~ 2,
                                   plot %in% c(102,201,303,402,502) ~ 3, 
                                   plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%  
  dplyr::select(-crop) %>% 
  mutate(crop = 'beans',
         crop = as.factor(crop)) %>% 
  relocate(year, date, crop, trt, plot) %>% 
  print(n = Inf)
colnames(bpf_clean)


b_clean <- bpf_clean %>% 
  rename('Coleoptera larvae' = Coleoptera)%>% 
  rename(Lyniphiidae = Lin) %>%
  mutate(Carabidae_new = Carabidae + Pterostichus +Cicindelidae) %>% 
  dplyr::select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Carabidae = Carabidae_new) %>% 
  print(n = Inf)

# ALL 

pf_all <- rbind(b_clean, c_clean) %>% 
  replace(is.na(.), 0) %>% 
  mutate_at(vars(6:25), as.numeric)
colnames(pf_all)


pf_clean <- pf_all %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae, -Coreidae) %>% 
  mutate(predators = Lycosidae + Formicidae + Thomisidae + `Coleoptera larvae` + Staphylinidae + Opiliones + Gryllidae +
           Tetragnathidae + Chilopoda  + Gnaphosidae + Agelenidae + Lyniphiidae + Carabidae + Araneae + Salticidae) %>% 
  dplyr::select(year, crop, trt, predators) %>% 
  ungroup() %>% 
  group_by(crop, year, trt) %>% 
  summarise(pred = sum(predators))


# extracting the precip data anmd necessary pf data ####
c_clean
b_clean
unique(c_clean$date)
unique(pf_all$date)

pf_precip <- pf_all %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae, -Coreidae) %>% 
  mutate(predators = Lycosidae + Formicidae + Thomisidae + `Coleoptera larvae` + Staphylinidae + Opiliones + Gryllidae +
           Tetragnathidae + Chilopoda  + Gnaphosidae + Agelenidae + Lyniphiidae + Carabidae + Araneae + Salticidae) %>% 
  mutate(month = format(date, '%m')) %>% 
  group_by(month, year) %>% 
  summarise(pf_sum = sum(predators)) %>% 
  arrange(year)

weather

pf_weather <- weather %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y"),
         month = format(date, '%m')) %>%  
  filter(year != '2021') %>% 
  dplyr::select(-date, -`Avg Air Temp (?F)`, -`Max Air Temp (?F)`,-`Min Air Temp (?F)`) %>% 
  rename(precip = `Total Precipitation`) %>% 
  relocate(year, month) %>% 
  mutate_at(vars(1:2), as.factor) %>% 
  group_by(year, month) %>% 
  summarise(pre_sum = sum(precip)) %>% 
  filter(!(year == '2023' & month == '05')) %>% 
  filter(!(year == "2023" & month == "08")) %>% 
  filter(!(year == "2022" & month == "06")) %>% 
  arrange(year)

# binding them together 

pf_w_precip <- cbind(pf_precip, pf_weather) %>% 
  dplyr::select(-month...5, -year...4) %>% 
  rename(year = year...2, 
         month = month...1) %>% 
  mutate(month = as.factor(month))

#model 
pre_m1 <- glm.nb(pf_sum ~ pre_sum, data = pf_w_precip)
summary(pre_m1)
hist(residuals(pre_m1))
