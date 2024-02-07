# Jared Adam 
# going to test out RGL here 
# do I need species names? 
# do I need polygons? 

# packages ####
library(tidyverse)
library(rgl)
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

b_rgl <- b_clean%>% 
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

c_rgl <- c_clean  %>% 
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

all_arth_bc <- rbind(b_rgl, c_rgl)%>% 
  arrange(year, plot, crop) %>% 
  replace(is.na(.),0) %>% 
  mutate_at(6:25, as.numeric) %>% 
  print(n = Inf)

# youtube exmaple ####
library(readxl)

# nmds ####

?inner_join

rgl_pops <- all_arth_bc[6:25]

rgl_nmds <- metaMDS(rgl_pops, k=3)

# trts
scrs <- scores(rgl_nmds, display = "sites")
rgl_trts <- cbind(as.data.frame(scrs), trt = all_arth_bc$trt)

# species names 
rgl_fsc <- as.data.frame(scores(rgl_nmds, 'species'))
rgl_fsc$species <- rownames(rgl_fsc)

Check <- rgl_trts[rgl_trts$trt == "1",][chull(rgl_trts[rgl_trts$trt == "1", c("NMDS1", "NMDS2", "NMDS3")]),]
Brown <- rgl_trts[rgl_trts$trt == "2",][chull(rgl_trts[rgl_trts$trt == "2", c("NMDS1", "NMDS2", "NMDS3")]),]
Green <- rgl_trts[rgl_trts$trt == "3",][chull(rgl_trts[rgl_trts$trt == "3", c("NMDS1", "NMDS2", "NMDS3")]),]
GrBr <- rgl_trts[rgl_trts$trt == "4",][chull(rgl_trts[rgl_trts$trt == "4", c("NMDS1", "NMDS2", "NMDS3")]),]

hull_rgl <- rbind(Check, Brown, Green, GrBr)

# so, now I have species names and treatments values (this is their location in the nmds) 
hull_rgl
rgl_fsc


test_rgl <- inner_join(all_arth_bc, rgl_trts, by = c('trt'), relationship = 'many-to-many') %>% 
  mutate(color = case_when( trt == "1" ~ 'red',
                            trt == "2" ~ 'brown',
                            trt == "3" ~ 'green',
                            trt == "4" ~ 'orange'))


plot3d(x = test_rgl$NMDS1, y = test_rgl$NMDS2, z = test_rgl$NMDS3,
       col = test_rgl$color, size = 6, type = 'p',
       xlab = 'NMDS Axis 1', 
       ylab = 'NMDS Axis 2',
       zlab = 'NMDS Axis 3'
       )

# I want to do this by crop, not trt: no sig with trt
rgl_pops
scrs <- scores(rgl_nmds, display = "sites")
rgl_crop <- cbind(as.data.frame(scrs), crop = all_arth_bc$crop)
rgl_crop <- rgl_crop %>% 
  mutate(color = case_when(crop == "beans" ~ "blue", 
                           crop == "corn" ~ "red"))
rgl_crop %>% count(color)

plot3d(rgl_crop, 
       col = rgl_crop$color,
       type = 's',
       xlab = 'NMDS Axis 1', 
       ylab = 'NMDS Axis 2',
       zlab = 'NMDS Axis 3')




