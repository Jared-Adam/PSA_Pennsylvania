# Jared Adam 
# how are they populations changing among year and crop
  # in the same field?
# started on 2/7/2024
# using 2022 corn and 2023 beans

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)

# data ####
beans <- bean_pf
corn <- corn_pf


# wrangling ####
# pivot, add year, subset by year, cbind together, functional groups? 
#
##
###
# beans 
colnames(beans)
b_wide <- beans %>% 
  select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(b_wide)
b_wide <- b_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiide + Lyniphiidae + Linyphiidae) %>% 
  select(-Liniphiide, -Lyniphiidae, -Linyphiidae, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(b_wide)

b_clean <- b_wide %>% 
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
colnames(b_clean)

b_23 <- b_clean %>% 
  filter(year == 2023) %>% 
  print(n = Inf)
b_23 <- b_23 %>% 
  rename('Coleoptera larvae' = Coleoptera)%>% 
  rename(Lyniphiidae = Lin) %>%
  mutate(Carabidae_new = Carabidae + Pterostichus +Cicindelidae) %>% 
  select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Carabidae = Carabidae_new) %>% 
  print(n = Inf)
colnames(b_23)

###
##
#
#
##
###
# corn 
colnames(corn)
c_wide <- corn %>% 
  select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(c_wide)
c_wide <- c_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lyn = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph= Staphylinidae + Staphylinidaa) %>% 
  select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(c_wide)

c_clean <- c_wide %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                         plot %in% c(103,204,302,403,501) ~ 2,
                         plot %in% c(102,201,303,402,502) ~ 3, 
                         plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%
  print(n = Inf)
colnames(c_clean)

c_22 <- c_clean %>% 
  filter(year == 2022) %>% 
  rename(Lyniphiidae = Lyn,
         Staphylinidae = Staph, 
         Tetragnathidae = Tetrgnathidae) %>% 
  mutate(Carabidae_new = Carabidae + Cicindelidae,
         Gryll = Gryllidae +Gyrillidae) %>% 
  select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
  rename(Carabidae = Carabidae_new,
         Gryllidae = Gryll) %>%
  print(n = Inf) 

###
##
#
# 
##
###
?mutate_at
bc <- rbind(b_23, c_22)
bc <- bc %>% 
  arrange(year, plot, crop) %>% 
  replace(is.na(.),0) %>% 
  mutate_at(6:25, as.numeric) %>% 
  mutate(date = as.factor(date)) %>% 
  print(n = Inf)

###
##
#

# functional groups? ####

# permanova ####
#
##
###
bc

bc_fams <- bc[6:25]
bc_dist <- vegdist(bc_fams, method = "bray")

# crop and date are sig
p1 <- adonis2(bc_dist ~ crop + date + trt, perm = 999, method = "bray", data = bc)
p1

# nmds ####
#
##
###

nmds <- metaMDS(bc_fams, k=3)
nmds$stress
stressplot(nmds)

###
##
#

# plot ####

fsc <- as.data.frame(scores(nmds, "species"))
fsc$species <- rownames(fsc)

ordiplot3d(nmds)
bc_p <- with(bc, ordiplot3d(nmds, col = crop, pch = 16, angle = 50))
with(bc, ordihull(bc_p, groups = bc$crop, draw = "poly", 
                        col = 1:3, 
                        label = F,
                        border = F,
                        alpha = 50))
text(bc_p$xyz.convert(fsc), rownames(fsc), cex = 1.2)
legend(x = 'right', legend = levels(bc$crop), col = 1:3, pch = 16, cex = 2)

# loop for anova of pops x crop ####
#
##
###
bc
bc_fams
colnames(bc_fams)

#test anova to check dist 
test_aov <- aov(Lycosidae ~ crop + trt, bc)
summary(test_aov)
TukeyHSD(test_aov)
hist(residuals(test_aov))
plot(bc$crop, bc$Lycosidae)
plot(bc$crop, bc$Carabidae)
# loop 

sp_list <- bc_fams
test_list <- list()
summary_list <- list()
tukey_list <- list()
crop_p <- list()
trt_p <- list()
for(i in 1:20){
  print(i)
  sps <- colnames(sp_list[i])
  print(sps)
  bc_loop <- subset(bc, select = c("crop", "trt", sps))
  colnames(bc_loop) <- c("crop", "trt", "sps")
  
  model <- aov(sps ~ crop, bc_loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  # pvalue extraction
  # crop_pval <- summary_list$`Pr(>F)`[1]
  # trt_pval <- summary_list$`Pr(>F)`[3]
  # crop_p[[i]] <- crop_pval
  # trt_p[[i]] <- trt_pval
}
summary_list
tukey_list[1]
plot(tukey_list[[1]])
tukey_list[17]
plot(tukey_list[[17]])

# wut ####
se_df <- bc %>% 
  group_by(crop, trt) %>% 
  summarise(mean = rowMeans(select(6:25))) %>% 
  select(crop, trt, mean)

bc %>% 
  group_by(crop, trt) %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(6:23))) %>% 
  select(crop, trt, sum)


  sps_grouped <- bc_loop %>% 
    group_by(crop, trt) %>% 
    summarise(mean = mean(sps),
              sd = sd(sps),
              n = n()) %>% 
    mutate(se = sd/sqrt(n))
  
