# Jared Adam 
# how are they populations changing among year and crop
  # in the same field?
# started on 2/7/2024
# using 2022 corn and 2023 beans

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(plotly)
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

# permanova corn 22 beans 23 ####
#
##
###
bc

bc_fams <- bc[6:25]
bc_dist <- vegdist(bc_fams, method = "bray")

# crop and date are sig
p1 <- adonis2(bc_dist ~ crop + date + trt, perm = 999, method = "bray", data = bc)
p1

# nmds corn 22 beans 23####
#
##
###

nmds <- metaMDS(bc_fams, k=3)
nmds$stress
stressplot(nmds)

###
##
#

# plot corn 22 beans 23 ####

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

# loop for anova of pops x crop corn 22 beans 23 ####
#
##
###
bc
bc_fams
colnames(bc_fams)

# going to split into more functional groups first 
# spiders
# carabids 
# other beetles 
# beetle larvae 
# non-insect arthropods
# other
colnames(bc)

func_bc <- bc %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Lyniphiidae + Araneae + Salticidae,
         Non_Insect_Arth = Diplopoda + Chilopoda, Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Other_insects = Dermaptera + Coreidae) %>% 
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Lyniphiidae, -Araneae, -Salticidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Coreidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae,
         Coleoptera_larvae = 'Coleoptera larvae')




#test anova to check dist 
test_aov <- aov(Lycosidae ~ crop + trt, bc)
summary(test_aov)
TukeyHSD(test_aov)
hist(residuals(test_aov))
plot(bc$crop, bc$Lycosidae)
plot(bc$crop, bc$Carabidae)

func_test <- aov(Aranaeomorphae ~ crop + trt, func_bc)
summary(func_test)
TukeyHSD(func_test)
plot(func_bc$crop, func_bc$Aranaeomorphae)
# loop 



sp_list <- func_bc[6:14]
summary_list <- list()
tukey_list <- list()
# crop_p <- list()
# trt_p <- list()
for(i in 1:9){
  print(i)
  sps <- colnames(sp_list[i])
  print(sps)
  bc_loop <- subset(func_bc, select = c("crop", "trt", sps))
  colnames(bc_loop) <- c("crop", "trt", "sps")
  
  model <- aov(sps ~ crop, bc_loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  print(ggplot(bc_loop, aes(x = crop, y = sps, fill = crop))+
    geom_bar(position = 'dodge', stat= 'identity'))


}
colnames(sp_list)
summary_list
# groups w sig values : 1, 3, 5, 6, 7, 8
# Formicidae, Ensifera, Carabidae, Aranaeomorphae, Non_Insect_Arth, Other_Coleoptera
tukey_list[6]
plot(tukey_list[[6]])
tukey_list[5]
plot(tukey_list[[5]])

# main groups of interest: 5 = carabidae beans > corn | 6 = aranaeomorphae corn > beans






# total arthropods by crop and trt for 2022 and 2023 ####
colnames(func_bc)
tot_arth <- func_bc %>% 
  group_by(crop, trt) %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(6:12))) %>% 
  print(n = Inf)

se_df <- tot_arth %>% 
  group_by(crop, trt) %>% 
  summarise(mean = mean(sum),
            sd = sd(sum),
            n = n(),
            se = sd/ sqrt(n)) %>% 
  select(crop, trt, mean, sd, se)

tot_aov <- aov(sum ~ crop + trt, tot_arth)
summary(tot_aov)
TukeyHSD(tot_aov)
hist(residuals(tot_aov))

tot_se_df <- tot_arth %>% 
  group_by(crop) %>% 
  summarise(mean = mean(sum), 
            sd = sd(sum), 
            n = n(), 
            se = sd/ sqrt(n))

ggplot(tot_se_df, aes(x = reorder(crop, mean), y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 1.9, y=16.8, label = "***", size = 6)+
  labs(title = "Total arthropod by crop",
       x = 'Crop and Year',
       y = 'Mean Arthropod population')+
  scale_x_discrete(labels=c('Corn:2022', 'Beans:2023'))

  sps_grouped <- bc_loop %>% 
    group_by(crop, trt) %>% 
    summarise(mean = mean(sps),
              sd = sd(sps),
              n = n()) %>% 
    mutate(se = sd/sqrt(n))
  
# total arthropods by crop for all years ####
b_clean 
c_clean

all_b <- b_clean %>% 
  rename('Coleoptera larvae' = Coleoptera)%>% 
  rename(Lyniphiidae = Lin) %>%
  mutate(Carabidae_new = Carabidae + Pterostichus +Cicindelidae) %>% 
  select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Carabidae = Carabidae_new) %>% 
  print(n = Inf)

all_c <- c_clean %>% 
  rename(Lyniphiidae = Lyn,
         Staphylinidae = Staph, 
         Tetragnathidae = Tetrgnathidae) %>% 
  mutate(Carabidae_new = Carabidae + Cicindelidae,
         Gryll = Gryllidae +Gyrillidae) %>% 
  select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
  rename(Carabidae = Carabidae_new,
         Gryllidae = Gryll) %>%
  print(n = Inf) 

all_arth_bc <- rbind(all_b, all_c)%>% 
  arrange(year, plot, crop) %>% 
  replace(is.na(.),0) %>% 
  mutate_at(6:25, as.numeric) %>% 
  print(n = Inf)

func_tot <- all_arth_bc %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Lyniphiidae + Araneae + Salticidae,
         Non_Insect_Arth = Diplopoda + Chilopoda, Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Other_insects = Dermaptera + Coreidae) %>% 
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Lyniphiidae, -Araneae, -Salticidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Coreidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae,
         Coleoptera_larvae = 'Coleoptera larvae')

tot_all_years <- func_tot %>% 
  group_by(crop, trt) %>% 
  arrange(crop, trt) %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(6:12))) %>% 
  print(n = Inf)

all_years_bc_aov <- aov(sum ~ crop + trt, tot_all_years)
summary(all_years_bc_aov)
TukeyHSD(all_years_bc_aov)

tot_se_years_bc_df <- tot_all_years %>% 
  group_by(crop) %>% 
  summarise(mean = mean(sum), 
            sd = sd(sum), 
            n = n(), 
            se = sd/ sqrt(n))

ggplot(tot_se_years_bc_df, aes(x = reorder(crop, mean), y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 1.8, y=18, label = "***", size = 6)+
  labs(title = "Total arthropod by crop",
       subtitle = "Years: 2022-2023",
       x = 'Crop',
       y = 'Mean Arthropod population')+
  scale_x_discrete(labels=c('Corn', 'Beans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# loop to look at pops by crop ####

#test aov 
test_spider <- aov(Aranaeomorphae ~ crop, func_tot)
summary(test_spider)
TukeyHSD(test_spider)

test_carab <- aov(Carabidae ~ crop, func_tot)
summary(test_carab)


all_sp_list <- func_tot[6:14]
all_summary_list <- list()
all_tukey_list <- list()
# crop_p <- list()
# trt_p <- list()
for(i in 1:9){
  print(i)
  spss <- colnames(all_sp_list[i])
  print(spss)
  all_loop <- subset(func_tot, select = c("crop", "trt", spss))
  colnames(all_loop) <- c("crop", "trt", "spss")
  
  model <- aov(spss ~ crop, all_loop)
  
  aov_summary <- summary(model)
  all_summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  all_tukey_list[[i]] <- aov_tukey
  
  # print(ggplot(all_loop, aes(x = crop, y = sps, fill = crop))+
  #         geom_bar(position = 'dodge', stat= 'identity'))
  # 
  
}
colnames(all_sp_list)
all_summary_list[[5]]
all_summary_list[[6]]
# groups w sig values : 
  # 1,3,5,6,7,8
# formicidae, ensifera, carabidae, araneomorphae, non, other_c, other_i

# Carabid plot 
cartabid_plot <- func_tot %>% 
  dplyr::select(year, crop, Carabidae) %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Carabidae),
            sd = sd(Carabidae), 
            n = n(),
            se = sd/sqrt(n))

ggplot(cartabid_plot, aes(x = reorder(crop, mean), y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 1.8, y=7.5, label = "***", size = 12)+
  labs(title = "Total Carabidae by crop",
       subtitle = "Years: 2022-2023",
       x = 'Crop',
       y = 'Mean Carabidae population')+
  scale_x_discrete(labels=c('Corn', 'Beans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Arane plot
arane_plot <- func_tot %>% 
  dplyr::select(year, crop, Aranaeomorphae) %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Aranaeomorphae),
            sd = sd(Aranaeomorphae), 
            n = n(),
            se = sd/sqrt(n))

ggplot(arane_plot, aes(x = reorder(crop, mean), y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 1.8, y=6.5, label = "*", size = 12)+
  labs(title = "Total Aranaeomorphae by crop",
       subtitle = "Years: 2022-2023",
       x = 'Crop',
       y = 'Mean Aranaeomorphae population')+
  scale_x_discrete(labels=c('Corn', 'Beans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






# permanova all years ####
#
##
###

all_arth_bc
all_years_arth <- all_arth_bc[6:25]
all_years_scores <- vegdist(all_years_arth, method = "bray")

# crop is significant 
all_years_p1 <- adonis2(all_years_scores ~ crop + trt, perm = 999, method = "bray", data = all_arth_bc)
all_years_p1
###
##
#

# nmds all years ####

#
##
###

nmds_all_years <- metaMDS(all_years_arth, k = 3)
nmds_all_years$stress
stressplot(nmds_all_years)

scores_all <- scores(nmds_all_years, display = "sites")
scores_all <- cbind(as.data.frame(scores_all), crop = all_arth_bc$crop)


# plot

fig <- plot_ly(scores_all, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~crop, colors = c("#1B9E77","#D95F02"))
fig <- fig %>% add_markers()
fig


###
##
#

# plot all years ####

all_fsc <- as.data.frame(scores(nmds_all_years, 'species'))
all_fsc$species <- rownames(all_fsc)

ordiplot3d(nmds_all_years)
all_bc_p <- with(all_arth_bc, ordiplot3d(nmds_all_years, col = crop, pch = 16, angle = 50))
with(all_arth_bc, ordihull(all_bc_p, groups = all_arth_bc$crop, draw = "poly", 
                  col = 1:3, 
                  label = F,
                  border = F,
                  alpha = 50))
text(all_bc_p$xyz.convert(all_fsc), rownames(all_fsc), cex = 1.2)
legend(x = 'right', legend = levels(all_arth_bc$crop), col = 1:3, pch = 16, cex = 2)


# plotly ####
# rgl_pops <- all_arth_bc[6:25]
# 
# rgl_nmds <- metaMDS(rgl_pops, k=3)
# 
# # trts
# scrs <- scores(rgl_nmds, display = "sites")
# rgl_trts <- cbind(as.data.frame(scrs), trt = all_arth_bc$trt)
# 
# # species names 
# rgl_fsc <- as.data.frame(scores(rgl_nmds, 'species'))
# rgl_fsc$species <- rownames(rgl_fsc)
# 
# Check <- rgl_trts[rgl_trts$trt == "1",][chull(rgl_trts[rgl_trts$trt == "1", c("NMDS1", "NMDS2", "NMDS3")]),]
# Brown <- rgl_trts[rgl_trts$trt == "2",][chull(rgl_trts[rgl_trts$trt == "2", c("NMDS1", "NMDS2", "NMDS3")]),]
# Green <- rgl_trts[rgl_trts$trt == "3",][chull(rgl_trts[rgl_trts$trt == "3", c("NMDS1", "NMDS2", "NMDS3")]),]
# GrBr <- rgl_trts[rgl_trts$trt == "4",][chull(rgl_trts[rgl_trts$trt == "4", c("NMDS1", "NMDS2", "NMDS3")]),]
# 
# hull_rgl <- rbind(Check, Brown, Green, GrBr)
# 
# # so, now I have species names and treatments values (this is their location in the nmds) 
# hull_rgl
# rgl_fsc


