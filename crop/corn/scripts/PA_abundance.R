# Jared Adam
# abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)

# data ####
pf <- corn_pf

# wrangling test ####
# need to pivot wider to get my taxa names as columns with counts 
pf <- pf %>% 
  arrange(date, plot)
test <- pf[1:95, ]

# pivot <- test %>% 
#   select(-split, -life_stage, -sp, -genus) %>% 
#   group_by(date, plot, family) %>% 
#   summarise(n_dist_families = n_distinct(family)) %>% 
#   pivot_wider(names_from = family, 
#               values_from = n_dist_families)

pivot<-test %>% 
  select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length))

# whole data set 
pf_wide <- pf %>% 
  select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)
  
colnames(pf_wide)
pf_wide <- pf_wide %>% 
  mutate(Lin = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph = Staphylinidae + Staphylinidaa) %>% 
  select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
  replace(is.na(.),0) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(pf_wide)

pf_clean <- pf_wide %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                         plot %in% c(103,204,302,403,501) ~ 2,
                         plot %in% c(102,201,303,402,502) ~ 3, 
                         plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>% 
  print(n = Inf)


# Permanova ####
family_names <- pf_clean[6:24]

dist <- vegdist(family_names, 'bray')

perm_1 <- adonis2(dist ~ trt, permutations = 999, method = "bray", data = pf_clean)
perm_1

# year is significant
# trt is not sig 
# I will use this model to highlight both
perm_2 <- adonis2(dist ~ trt + year , permutations = 999, method = 'bray', data = pf_clean)
perm_2

perm_3 <- adonis2(dist ~ year , permutations = 999, mathod = 'bray', data = pf_clean)
perm_3

