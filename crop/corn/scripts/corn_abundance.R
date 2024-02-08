# Jared Adam
# CORN abundance data: PF and Sweep nets
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
#   dplyr::select(-split, -life_stage, -sp, -genus) %>% 
#   group_by(date, plot, family) %>% 
#   summarise(n_dist_families = n_distinct(family)) %>% 
#   pivot_wider(names_from = family, 
#               values_from = n_dist_families)

pivot<-test %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length))
# wrangling ####
# whole data set 
pf_wide <- pf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(pf_wide)
pf_wide <- pf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lyn = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph= Staphylinidae + Staphylinidaa) %>% 
  dplyr::select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
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

# PF 2022  ####

#
##
###

cpf_2022 <- filter(pf_clean, year == 2022)
colnames(cpf_2022)
unique(cpf_2022$date)
cpf_2022 <- cpf_2022 %>% 
  dplyr::mutate(timing = case_when(date == "2022-05-28" ~ 1,
                                   date == "2022-07-01" ~ 2)) %>% 
  dplyr::mutate(timing = as.factor(timing)) %>% 
  dplyr::rename(Lyniphiidae = Lyn, 
                Staphylinidae = Staph,
                Tetragnathidae = Tetrgnathidae) %>% 
  relocate(year, date, timing)
colnames(cpf_2022)

c_family_names_22 <- cpf_2022[7:25]
c_dist_22 <- vegdist(c_family_names_22, 'bray')

perm_2_1 <- adonis2(c_dist_22 ~ trt, permutations = 999, method = 'bray', data = cpf_2022)
perm_2_1

# date is significant
# this makes sense
perm_2_2 <- adonis2(c_dist_22 ~ trt + date, permutations = 999, method = 'bray', data = cpf_2022)
perm_2_2

# NMDS 

# 3 D is better 
cord_22_3 <- metaMDS(c_family_names_22, k = 3)
cord_22_3$stress

# plot
scrs_c22 <- scores(cord_22_3, display = "sites")
c_22_scrs <- cbind(as.data.frame(scrs_c22), trt = cpf_2022$trt)
# 

c_22.fig <- plot_ly(c_22_scrs, x = ~NMDS1, y = ~NMDS2, z= ~NMDS3, color = ~trt)
c_22.fig <- c_22.fig %>% 
  add_markers()
c_22.fig

# 

# ordiplot3d(cord_22_3)
# tc_1 <- with(cpf_2022, ordiplot3d(cord_22_3, col = timing, pch = 16, angle = 50))
# with(cpf_2022, ordihull(tc_1, groups = cpf_2022$timing, draw = "poly", 
#                         col = 3:4, 
#                         label = F,
#                         border = F,
#                         alpha = 50))
# text(tc_1$xyz.convert(c_22_fsc), rownames(c_22_fsc), cex = 1.2)
# legend(x = -3, y = -0.5, legend = c("2022-05-28", "2022-07-01"), col = 1:2, pch = 16, cex = 2)
# legend(0.5, -0.5, "Stress: 0.1284837",
#        xjust = 0.5,
#        yjust = 3, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# legend(0.5, -0.5, "Timing p-value: 0.0001***",
#        xjust = 0.5,
#        yjust = 4.5, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# title(main ="NMDS of corn 2022 population distributions by timing",
#       cex.main = 2)

###
##
#


# PF 2023 ####

#
##
###

cpf_2023 <- filter(pf_clean, year == 2023)
colnames(cpf_2023)
unique(cpf_2023$date)

cpf_2023 <- cpf_2023 %>% 
  dplyr::mutate(timing = case_when(date == "2023-06-26" ~ 1,
                                   date == "2023-07-28" ~ 2)) %>% 
  dplyr::mutate(timing = as.factor(timing)) %>% 
  dplyr::rename(Lyniphiidae = Lyn, 
                Staphylinidae = Staph,
                Tetragnathidae = Tetrgnathidae) %>% 
  relocate(year, date, timing)
colnames(cpf_2023)
unique(cpf_2023$year)


c_family_names_23 <- cpf_2023[7:25]
c_dist_23 <- vegdist(c_family_names_23, 'bray')

perm_3_1 <- adonis2(c_dist_23 ~ trt, permutations = 999, method = 'bray', data = cpf_2023)
perm_3_1

#date is significant 
perm_3_2 <- adonis2(c_dist_23 ~ trt + date, permutations = 999, method = 'bray', data = cpf_2023)
perm_3_2

# NMDS

# 3 D is better 
cord_23_3 <- metaMDS(c_family_names_23, k = 3)
cord_23_3$stress

# plot 

c_23_fsc <- as.data.frame(scores(cord_23_3, 'species'))
c_23_fsc$species <- rownames(c_23_fsc)


# ordiplot3d(cord_23_3)
# tc_2 <- with(cpf_2023, ordiplot3d(cord_23_3, col = timing, pch = 16, angle = 50))
# with(cpf_2023, ordihull(tc_2, groups = cpf_2023$timing, draw = "poly", 
#                         col = 1:3, 
#                         label = F,
#                         border = F,
#                         alpha = 50))
# text(tc_2$xyz.convert(c_23_fsc), rownames(c_23_fsc), cex = 1.2)
# legend(x = -2, y = -0.5, legend = c("2023-06-26", "2023-07-28"), col = 1:2, pch = 16, cex = 2)
# legend(0.5, -0.5, "Stress: 0.162313",
#        xjust = 0.2,
#        yjust = 3, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# legend(0.5, -0.5, "Timing p-value: 0.0001***",
#        xjust = 0.2,
#        yjust = 4.5, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# title(main ="NMDS of corn 2023 population distributions by timing",
#       cex.main = 2)

###
##
#


# PF 22 and 23 ####

#
##
###

pf_clean
colnames(pf_clean)
cpf_clean <- pf_clean %>% 
  dplyr::rename(Lyniphiidae = Lyn, 
                Staphylinidae = Staph,
                Tetragnathidae = Tetrgnathidae)
colnames(cpf_clean)

c_family_names <- cpf_clean[6:24]

c_dist <- vegdist(c_family_names, 'bray')

perm_1 <- adonis2(c_dist ~ trt, permutations = 999, method = "bray", data = cpf_clean)
perm_1

# year is significant
# trt is not sig 
# I will use this model to highlight both
perm_2 <- adonis2(c_dist ~ trt + year + date , permutations = 999, method = 'bray', data = cpf_clean)
perm_2

perm_3 <- adonis2(c_dist ~ year , permutations = 999, mathod = 'bray', data = cpf_clean)
perm_3

# how about date? 
cpf_year <- cpf_clean %>% 
  mutate(date = as.factor(date))
perm_4 <- adonis2(c_dist ~ trt * date, permutations = 999, method = 'bray', data = cpf_year)
perm_4


# NMDS

# 3 D is better 
cord_3 <- metaMDS(c_family_names, k = 3)
cord_3$stress

# plot 
c_fsc <- as.data.frame(scores(cord_3, 'species'))
c_fsc$species <- rownames(c_fsc)


# ordiplot3d(cord_3)
# tc_3 <- with(cpf_clean, ordiplot3d(cord_3, col = year, pch = 16, angle = 50))
# with(cpf_clean, ordihull(tc_3, groups = cpf_clean$year, draw = "poly", 
#                         col = 1:3, 
#                         label = F,
#                         border = F,
#                         alpha = 50))
# #text(tc_3$xyz.convert(c_fsc), rownames(c_fsc), cex = 1.2)
# legend(x = -2, y = -0.5, legend = c("2022", "2023"), col = 1:2, pch = 16, cex = 2)
# legend(0.5, -0.5, "Stress: 0.1681593",
#        xjust = 0.2,
#        yjust = 3, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# legend(0.5, -0.5, "Timing p-value: 0.0001***",
#        xjust = 0.2,
#        yjust = 4.5, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# legend(0.5, -0.5, "Year p-value: 0.0001***",
#        xjust = 0.2,
#        yjust = 6.5, 
#        x.intersp = -0.5,
#        y.intersp = 0.1, 
#        adj = c(0,0.5), 
#        cex = 1.5)
# title(main ="NMDS of corn 2022 x 2023 population distributions by year",
#       cex.main = 2)
# 

###
##
#




# anova ####
pf_clean
