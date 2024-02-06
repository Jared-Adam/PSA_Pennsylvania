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

bean_family_names_22 <- bpf_2022[6:24]
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

ordiplot3d(bord_22_3)
tpl <- with(bpf_2022, ordiplot3d(bord_22_3, col = trt, pch = 16, angle = 50))
with(bpf_2022, ordihull(tpl, groups = bpf_2022$trt, draw = "poly", 
                        col = 1:4, 
                        label = F,
                        border = F,
                        alpha = 50))
text(tpl$xyz.convert(fsc), rownames(fsc), cex = 1.2)
legend(x = 'right', legend = levels(bpf_2022$trt), col = 1:4, pch = 16, cex = 2)
legend(0, -0.5, "Stress: 0.118642",
       xjust = 0.5,
       yjust = 3, 
       x.intersp = -0.5,
       y.intersp = 0.1, 
       adj = c(0,0.5), 
       cex = 1.5)
title(main ="NMDS of 2022 population distributions by treatment",
      cex.main = 2)


###
##
#

# PF 2023 ####
#
##
###


bpf_2023 <- filter(bpf_clean, year == 2023)

bean_family_names_23 <- bpf_2023[6:24]
bdist_23 <- vegdist(bean_family_names_23, 'bray')

bperm_3_1 <- adonis2(bdist_23 ~ trt, permutations = 999, method = 'bray', data = bpf_2023)
bperm_3_1

#date is significant 
bperm_3_2 <- adonis2(bdist_23 ~ trt + date, permutations = 999, method = 'bray', data = bpf_2023)
bperm_3_2

###
##
#
#
##
### 

# 22 and 23 #
beans_dist <- bpf_clean[6:24]

bpf_year <- bpf_clean %>% 
  mutate(date = as.factor(date))

bperm_1 <- adonis2(beans_dist ~ trt + year , permutations = 999, method = 'bray', data = bpf_year)
bperm_1

bperm_2 <- adonis2(beans_dist ~ year , permutations = 999, mathod = 'bray', data = bpf_year)
bperm_2

#date is significant 
bperm_3 <- adonis2(beans_dist ~ trt * date, permutations = 999, method = 'bray', data = bpf_year)
bperm_3

###
##
#

# NMDS PF ####
#
##
###

# 3D is substantially better than 2D for all of these 

###
##
#
#
##
###

# PF 23 # 

# 3 D is better 
bord_23_3 <- metaMDS(family_names_23, k = 3)
bord_23_3$stress

###
##
#
#
##
###

# these are for 22 and 23 
# 3 D is better 
bord_3 <- metaMDS(family_names, k = 3)
bord_3$stress

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


