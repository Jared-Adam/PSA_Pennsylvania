# Jared Adam
# BEANS abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)

# data ####
bpf <- bean_pf
unique(bpf$crop)

# wrangling test ####
# need to pivot wider to get my taxa names as columns with counts 
bpf <- bpf %>% 
  arrange(date, plot)
bpf_test <- bpf[1:95, ]

bpf_pivot <- bpf_test %>% 
  select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length))
# wrangling ####
# whole data set 
bpf_wide <- bpf %>% 
  select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(bpf_wide)
bpf_wide <- bpf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiide + Lyniphiidae + Linyphiidae) %>% 
  select(-Liniphiide, -Lyniphiidae, -Linyphiidae, -na) %>% 
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
  select(-crop) %>% 
  mutate(crop = 'beans',
         crop = as.factor(crop)) %>% 
  relocate(year, date, crop) %>% 
  print(n = Inf)
colnames(pf_clean)

# PF 2022 ####

#
##
###


bpf_2022 <- filter(bpf_clean, year == 2022)
unique(bpf_2022$date)
colnames(bpf_2022)
bpf_2022 <- bpf_2022 %>% 
  mutate(timing = case_when(date == "2022-05-28" ~ 1,
                            date == "2022-07-01" ~ 2,
                            date == "2022-08-18" ~ 3)) %>% 
  mutate(timing = as.factor(timing)) %>% 
  dplyr::rename(Linyphiidae = Lin) %>% 
  relocate(year, date, timing)

bean_family_names_22 <- bpf_2022[7:25]
bdist_22 <- vegdist(bean_family_names_22, 'bray')

bperm_2_1 <- adonis2(bdist_22 ~ trt, permutations = 999, method = 'bray', data = bpf_2022)
bperm_2_1

# date is significant
# this makes sense
bperm_2_2 <- adonis2(bdist_22 ~ trt + date, permutations = 999, method = 'bray', data = bpf_2022)
bperm_2_2


# PF 22 #

# 3 D is better 
bord_22_3 <- metaMDS(bean_family_names_22, k = 3)
bord_22_3$stress

# plot

b_22_fsc <- as.data.frame(scores(bord_22_3, "species"))
b_22_fsc$species <- rownames(b_22_fsc)


ordiplot3d(bord_22_3)
tpl <- with(bpf_2022, ordiplot3d(bord_22_3, col = timing, pch = 16, angle = 50))
with(bpf_2022, ordihull(tpl, groups = bpf_2022$timing, draw = "poly", 
                        col = 1:3, 
                        label = F,
                        border = F,
                        alpha = 50))
text(tpl$xyz.convert(b_22_fsc), rownames(b_22_fsc), cex = 1.2)
legend(x = 'right', legend = c("2022-05-28", "2022-07-01", "2022-08-18"), col = 1:3, pch = 16, cex = 2)
legend(0, -0.5, "Stress: 0.118642",
       xjust = 0.5,
       yjust = 3, 
       x.intersp = -0.5,
       y.intersp = 0.1, 
       adj = c(0,0.5), 
       cex = 1.5)
title(main ="NMDS of 2022 population distributions by timing",
      cex.main = 2)


###
##
#

# PF 2023 ####
#
##
###


bpf_2023 <- filter(bpf_clean, year == 2023)
colnames(bpf_2023)
unique(bpf_2023$date)
bpf_2023 <- bpf_2023 %>% 
  dplyr::mutate(timing = case_when(date == "2023-06-26" ~ 1,
                                   date == "2023-07-28" ~ 2)) %>% 
  dplyr::mutate(timing = as.factor(timing)) %>% 
  dplyr::rename(Linyphiidae = Lin) %>% 
  relocate(year, date, timing)
unique(bpf_2023$timing)
bean_family_names_23 <- bpf_2023[7:25]
bdist_23 <- vegdist(bean_family_names_23, 'bray')

bperm_3_1 <- adonis2(bdist_23 ~ trt, permutations = 999, method = 'bray', data = bpf_2023)
bperm_3_1

#date is significant 
bperm_3_2 <- adonis2(bdist_23 ~ trt + date, permutations = 999, method = 'bray', data = bpf_2023)
bperm_3_2

# NMDS

# 3 D is better 
bord_23_3 <- metaMDS(bean_family_names_23, k = 3)
bord_23_3$stress


# plot 
b_23_fsc <- as.data.frame(scores(bord_23_3, 'species'))
b_23_fsc$species <- rownames(b_23_fsc)

ordiplot3d(bord_23_3)
tpl_2 <- with(bpf_2023, ordiplot3d(bord_23_3, col = timing, pch = 16, angle = 50))
with(bpf_2023, ordihull(tpl_2, groups = bpf_2023$timing, draw = "poly", 
                        col = 1:3, 
                        label = F,
                        border = F,
                        alpha = 50))
text(tpl_2$xyz.convert(b_23_fsc), rownames(b_23_fsc), cex = 1.2)
legend(x = -3, y = -0.5, legend = c("2023-06-26", "2023-07-28"), col = 1:3, pch = 16, cex = 2)
legend(0.5, -0.5, "Stress: 0.1732693",
       xjust = 0.5,
       yjust = 3, 
       x.intersp = -0.5,
       y.intersp = 0.1, 
       adj = c(0,0.5), 
       cex = 1.5)
title(main ="NMDS of 2023 population distributions by timing",
      cex.main = 2)


###
##
#


# 22 and 23 ####
colnames(bpf_clean)
unique(bpf_clean$date)
bpf_clean <- bpf_clean %>% 
  dplyr::rename(Linyphiidae = Lin)

bean_family_names <- bpf_clean[6:24]

beans_dist <- vegdist(bean_family_names, 'bray')

bpf_year <- bpf_clean %>% 
  mutate(date = as.factor(date))

bperm_1 <- adonis2(beans_dist ~ trt + year , permutations = 999, method = 'bray', data = bpf_year)
bperm_1

bperm_2 <- adonis2(beans_dist ~ year , permutations = 999, mathod = 'bray', data = bpf_year)
bperm_2

#date is significant 
bperm_3 <- adonis2(beans_dist ~ year + date + trt, permutations = 999, method = 'bray', data = bpf_year)
bperm_3

# NMDS

# these are for 22 and 23 
# 3 D is better 
bord_3 <- metaMDS(bean_family_names, k = 3)
bord_3$stress

# plot 

b_all_fsc <- as.data.frame(scores(bord_3, 'species'))
b_all_fsc$species <- rownames(b_all_fsc)

ordiplot3d(bord_3)
tpl_3 <- with(bpf_clean, ordiplot3d(bord_3, col = year, pch = 16, angle = 50))
with(bpf_clean, ordihull(tpl_3, groups = bpf_clean$year, draw = "poly", 
                        col = 1:3, 
                        label = F,
                        border = F,
                        alpha = 50))
#text(tpl$xyz.convert(b_all_fsc), rownames(b_all_fsc), cex = 1.2)
legend(x = -3, y = -0.5, legend = c("2022", "2023"), col = 1:3, pch = 16, cex = 2)
legend(0.5, -0.5, "Stress: 0.1471073",
       xjust = 0.5,
       yjust = 3, 
       x.intersp = -0.5,
       y.intersp = 0.1, 
       adj = c(0,0.5), 
       cex = 1.5)
title(main ="NMDS of 2022 and 2023 population distributions by year",
      cex.main = 2)

# for after corn is done 
legend(0.5, -0.5, "Timing p-value: 0.0001***",
       xjust = 0.5,
       yjust = 4.5, 
       x.intersp = -0.5,
       y.intersp = 0.1, 
       adj = c(0,0.5), 
       cex = 1.5)



###
##
#

# practice/ test plots ####
# test plot from stack overflow 
# 
# data(dune,dune.env)
# SITE_ID <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t")
# dune.env$SITE_ID <- SITE_ID
# ?cca
# ord <- cca(dune ~ A1 + Moisture, dune.env)
# ordiplot3d(ord)
# pl4 <- with(dune.env, ordiplot3d(ord, col = Management, pch=16,angle=50))
# 
# with(dune.env, ordihull(pl4, dune.env$Management, draw = "poly", col = 1:4,label=T,
#                         alpha = 50))
# 
# 
# sp <- scores(pl4, choices=1:3, display="sites", scaling="symmetric")
# spp <- as.data.frame(cbind(dune.env$SITE_ID,sp))
# with(dune.env, ordilabel(pl4,labels=spp$V1,col="black", fill=NA, border=NA, pos = 2))


##
#this gets species names but no trts
fsc

bang <- ordiplot3d(bord_3, display = 'species')
text(bang$xyz.convert(fsc), rownames(fsc))
with(bang, ordiplot3d(bord_3, group = hull.data$trt, pch = 16, angle = 50))
for (i in seq (1,4)) ordihull(bang, groups = hull.data$trt, show.group = i, col = i, 
                              lty = 'dotted')

bang <- ordiplot3d(bord_3, display = 'species')
text(tpl$xyz.convert(fsc), rownames(fsc))
for(i in unique(hull.data$trt)) ordihull(bang, groups = hull.data$trt, show.group = i, 
                                        col = i,
                                        draw = 'polygon')



plot <- ordiplot3d(bord_3, display = 'sites')
for(i in unique(hull.data$trt)) ordihull(plot, groups = hull.data$trt,
                                         draw = 'poly', 
                                         col = i)



# anova ####
bpf_clean