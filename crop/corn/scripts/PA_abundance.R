# Jared Adam
# abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)

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
pf_wide <- pf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph = Staphylinidae + Staphylinidaa) %>% 
  select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
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
colnames(pf_clean)

# Permanova ####
#
##
###
# PF 2022 # 
pf_2022 <- filter(pf_clean, year == 2022)

family_names_22 <- pf_2022[6:24]
dist_22 <- vegdist(family_names_22, 'bray')

perm_2_1 <- adonis2(dist_22 ~ trt, permutations = 999, method = 'bray', data = pf_2022)
perm_2_1

# date is significant
# this makes sense
perm_2_2 <- adonis2(dist_22 ~ trt + date, permutations = 999, method = 'bray', data = pf_2022)
perm_2_2

# PF 2023 #
pf_2023 <- filter(pf_clean, year == 2023)

family_names_23 <- pf_2023[6:24]
dist_23 <- vegdist(family_names_23, 'bray')

perm_3_1 <- adonis2(dist_23 ~ trt, permutations = 999, method = 'bray', data = pf_2023)
perm_3_1

#date is significant 
perm_3_2 <- adonis2(dist_23 ~ trt + date, permutations = 999, method = 'bray', data = pf_2023)
perm_3_2

# PF 22 and 23 #

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

# how about date? 
pf_year <- pf_clean %>% 
  mutate(date = as.factor(date))
perm_4 <- adonis2(dist ~ trt * date, permutations = 999, method = 'bray', data = pf_year)
perm_4

###
##
#

#
##
###
# SWEEP 


###
##
#


# NMDS ####

ord_2 <- metaMDS(family_names, k = 2)
ord_2$stress

ord_3 <- metaMDS(family_names, k = 3)
ord_3$stress

p1 <- ordiplot3d(ord_3, scaling = 'symmetric', agnle = 15, type = 'n')
points(p1, 'points', pch =16, col = 'red', cex = 0.7)
#text(p1, 'arrows', col = 'blue', pos = 3)
sp <- scores(ord_3, choices = 1:3, dispaly = 'species',  scaling = 'symmetric')
text(p1$xyz.convert(sp), rownames(sp), cex = 0.7, xpd = TRUE)


huh <-

