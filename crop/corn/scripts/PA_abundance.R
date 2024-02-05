# Jared Adam
# abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)

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
