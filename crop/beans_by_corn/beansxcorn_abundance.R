# Jared Adam 
# how are they populations changing among year and crop
  # in the same field?
# started on 2/7/2024
# using 2022 corn and 2023 beans

# packages ####
library(tidyverse)
library(MASS)
library(multcomp)
library(emmeans)
library(vegan)
library(vegan3d)
library(plotly)
library(ggpubr)
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
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(b_wide)
b_wide <- b_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiide + Lyniphiidae + Linyphiidae) %>% 
  dplyr::select(-Liniphiide, -Lyniphiidae, -Linyphiidae, -na) %>% 
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
  dplyr::select(-crop) %>% 
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
  dplyr::select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
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
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
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
  dplyr::select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
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
  dplyr::select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
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
unique(bc$date)

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
# Df SumOfSqs      R2       F Pr(>F)    
# crop      1   5.0688 0.23054 31.3926  0.001 ***
#   date      2   4.7570 0.21636 14.7307  0.001 ***
#   trt       3   0.3737 0.01700  0.7715  0.738    
# Residual 73  11.7870 0.53610                   
# Total    79  21.9866 1.00000                   
# ---

# nmds corn 22 beans 23####
#
##
###

nmds <- metaMDS(bc_fams, k=3)
nmds$stress
# [1] 0.1561735
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
  dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Lyniphiidae, -Araneae, -Salticidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Coreidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae,
         Coleoptera_larvae = 'Coleoptera larvae')



# no mas

#test anova to check dist 
# test_aov <- aov(Lycosidae ~ crop + trt, bc)
# summary(test_aov)
# TukeyHSD(test_aov)
# hist(residuals(test_aov))
# plot(bc$crop, bc$Lycosidae)
# plot(bc$crop, bc$Carabidae)
# 
# func_test <- aov(Aranaeomorphae ~ crop + trt, func_bc)
# summary(func_test)
# TukeyHSD(func_test)
# plot(func_bc$crop, func_bc$Aranaeomorphae)
# # loop 
# 
# 
# 
# sp_list <- func_bc[6:14]
# summary_list <- list()
# tukey_list <- list()
# # crop_p <- list()
# # trt_p <- list()
# for(i in 1:9){
#   print(i)
#   sps <- colnames(sp_list[i])
#   print(sps)
#   bc_loop <- subset(func_bc, select = c("crop", "trt", sps))
#   colnames(bc_loop) <- c("crop", "trt", "sps")
#   
#   model <- aov(sps ~ crop, bc_loop)
#   
#   aov_summary <- summary(model)
#   summary_list[[i]] <- aov_summary
#   
#   aov_tukey <- TukeyHSD(model)
#   tukey_list[[i]] <- aov_tukey
#   
#   print(ggplot(bc_loop, aes(x = crop, y = sps, fill = crop))+
#     geom_bar(position = 'dodge', stat= 'identity'))
# 
# 
# }
# colnames(sp_list)
# summary_list
# # groups w sig values : 1, 3, 5, 6, 7, 8
# # Formicidae, Ensifera, Carabidae, Aranaeomorphae, Non_Insect_Arth, Other_Coleoptera
# tukey_list[6]
# plot(tukey_list[[6]])
# tukey_list[5]
# plot(tukey_list[[5]])

# main groups of interest: 5 = carabidae beans > corn | 6 = aranaeomorphae corn > beans

# carabidae numbers by crop for paper 
func_bc  %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Carabidae), 
            sd = sd(Carabidae), 
            n = n(), 
            se = sd/sqrt(n))
# crop   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 beans  5.48  7.37    40 1.17 
# 2 corn   1     1.72    40 0.273


# araneomorphae numbers by crop for paper

func_bc  %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Aranaeomorphae ), 
            sd = sd(Aranaeomorphae ), 
            n = n(), 
            se = sd/sqrt(n))
# crop   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 beans  2.17  1.87    40 0.295
# 2 corn   5.65  3.21    40 0.508


# lycosidae number by crop for paper
lyc <- aov(Lycosidae ~ crop, data = bc)
TukeyHSD(lyc)

bc %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Lycosidae), 
            sd = sd(Lycosidae), 
            n = n(), 
            se = sd/sqrt(n))
# crop   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 beans  1.72  1.77    40 0.280
# 2 corn   4.4   3.18    40 0.502


# total arthropods by crop and trt for corn 2022 and beans 2023 ####
colnames(func_bc)
tot_arth <- func_bc %>% 
  group_by(crop, trt) %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(6:12))) %>% 
  print(n = Inf)

#
##
###
# for paper #
year_crop <- func_bc %>% 
  group_by(crop,year) %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(6:12))) %>% 
  print(n = Inf)

# all
year_crop %>% 
  group_by(year, crop) %>% 
  summarise(
    sum = sum(sum)
  )
# year  crop    sum
# <fct> <fct> <dbl>
#   1 2022  corn    305
# 2 2023  beans   660



tot_aov <- aov(sum ~ crop + trt, tot_arth)
summary(tot_aov)
TukeyHSD(tot_aov)
hist(residuals(tot_aov))

se_df <- tot_arth %>% 
  group_by(crop, trt) %>% 
  summarise(mean = mean(sum),
            sd = sd(sum),
            n = n(),
            se = sd/ sqrt(n)) %>% 
  dplyr::select(crop, trt, mean, sd, se)

tot_arth_plot <- tot_arth %>% 
  pivot_longer(
    cols = where(is.numeric)) %>% 
  mutate(block = case_when(plot %in% c('101', '102', '103', '104') ~ 1,
                           plot %in% c('201', '202', '203', '204') ~ 2,
                           plot %in% c('301', '302', '303', '304') ~ 3, 
                           plot %in% c('401', '402', '403', '404') ~ 4,
                           plot %in% c('501', '502', '503', '503') ~ 5))

p <- glmer(value ~ trt*crop +
             (1|block/crop), 
           family = poisson, 
           data = tot_arth_plot)

nb <- glmer.nb(value ~ trt*crop +
            (1|block/crop),
          data = tot_arth_plot)

lrtest(p, nb)


m0 <- glmer.nb(value ~ 
           (1|block/crop),
         data = tot_arth_plot)

m1 <- glmer.nb(value ~ trt +
                 (1|block/crop),
               data = tot_arth_plot)

m2 <- glmer.nb(value ~ trt + crop +
                 (1|block/crop),
               data = tot_arth_plot)

m3 <- glmer.nb(value ~ trt*crop +
                 (1|block/crop),
               data = tot_arth_plot)


anova(m0, m1, m2, m3)
hist(residuals(m3))
check_model(m3)

bc_emm <- cld(emmeans(m3, ~ trt + crop), Letters = letters)


tot_se_df <- tot_arth %>% 
  group_by(crop) %>% 
  summarise(mean = mean(sum), 
            sd = sd(sum), 
            n = n(), 
            se = sd/ sqrt(n))
# crop   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 beans 16.5  13.5     40 2.13 
# 2 corn   7.62  4.53    40 0.716


# plots for corn 22 and beans 23 ####
# total arthropods

tot_se_df %>% 
  mutate(crop = case_when(crop == 'corn' ~ "Corn", 
                          crop == 'beans' ~ "Soybean"),
         crop = factor(crop, levels = c("Corn", "Soybean"))) %>% 
  ggplot(aes(x = trt, y = mean, fill = crop))+
    geom_bar(position = position_dodge(1), stat = 'identity')+
    geom_errorbar(aes(ymin=mean-se, ymax = mean+se), 
                  color = 'black', alpha = 1, size = 1, width = 0.5,
                  position = position_dodge(1))+
  scale_x_discrete(limits = c('1','2','4','3'),
                   labels = c('No CC', '14-28 DPP', '3-7 DPP', '1-3 DAP'))+
  scale_fill_manual(values = c("#D95F02","#1B9E77"), 
                    name = "Crop")+
    labs(title = "Total Arthropod x Treatment and Crop",
         subtitle = 'Years: 2022 Corn - 2023 Soybean',
         x = 'Treatment',
         y = 'Mean Arthropod population',
         caption = "DPP: Days pre plant
DAP: Days after plant")+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x =.75, y = 27, label = 'a', size = 10)+ #1c
  annotate('text', x =1.25, y = 27, label = 'bc', size = 10)+ #1b
  annotate('text', x =1.75, y = 27, label = 'ab', size = 10)+ #2c
  annotate('text', x =2.25, y = 27, label = 'abc', size = 10)+ #2b
  annotate('text', x =2.75, y = 27, label = 'abc', size = 10)+ #4c
  annotate('text', x =3.25, y = 27, label = 'abc', size = 10)+ #4b
  annotate('text', x =3.75, y = 27, label = 'ab', size = 10)+#3c
  annotate('text', x =4.25, y = 27, label = 'c', size = 10) #3b



tot_arth_plot %>% 
  mutate(crop = case_when(crop == 'corn' ~ "Corn", 
                           crop == 'beans' ~ "Soybean"),
          crop = fct_relevel(crop, "Corn", "Soybean")) %>% 
  ggplot(aes(trt, log10(value), fill = crop))+
  geom_bar(stat = 'identity', position = position_dodge(1))+
  scale_x_discrete(limits = c('1','2','4','3'))
  
  
  
  
  
# total arthropods by crop for all years ####
b_clean 
c_clean

all_b <- b_clean %>% 
  rename('Coleoptera larvae' = Coleoptera)%>% 
  rename(Lyniphiidae = Lin) %>%
  mutate(Carabidae_new = Carabidae + Pterostichus +Cicindelidae) %>% 
  dplyr::select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Carabidae = Carabidae_new) %>% 
  filter(date != '2022-08-18') %>% 
  print(n = Inf)
unique(all_b$date)



all_c <- c_clean %>% 
  rename(Lyniphiidae = Lyn,
         Staphylinidae = Staph, 
         Tetragnathidae = Tetrgnathidae) %>% 
  mutate(Carabidae_new = Carabidae + Cicindelidae,
         Gryll = Gryllidae +Gyrillidae) %>% 
  dplyr::select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
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
  dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Lyniphiidae, -Araneae, -Salticidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Coreidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae,
         Coleoptera_larvae = 'Coleoptera larvae') %>% 
  mutate(date = as.factor(date))

tot_all_years <- func_tot %>% 
  group_by(crop, trt) %>% 
  arrange(crop, trt) %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(6:12))) %>% 
  print(n = Inf)

unique(tot_all_years$date)

all_model <- glm.nb(sum ~ crop + year, data = tot_all_years)
summary(all_model)
hist(residuals(all_model))
cld(emmeans(all_model, ~ crop + year), Letters = letters)
# crop  year emmean     SE  df asymp.LCL asymp.UCL .group
# corn  2023   1.61 0.0995 Inf      1.41      1.80  a    
# corn  2022   1.81 0.0975 Inf      1.62      2.00  a    
# beans 2023   2.62 0.0914 Inf      2.44      2.80   b   
# beans 2022   2.82 0.0904 Inf      2.64      2.99   b   



tot_se_years_bc_df <- tot_all_years %>% 
  group_by(crop) %>% 
  summarise(mean = mean(sum), 
            sd = sd(sum), 
            n = n(), 
            se = sd/ sqrt(n))
# crop   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
# 1 beans 15.0  10.6     80 1.18 
# 2 corn   5.65  4.28    80 0.478

tot_all_years %>% 
  group_by(year) %>% 
  summarise(tot = sum(sum),
    mean = mean(sum), 
            sd = sd(sum), 
            n = n(), 
            se = sd/ sqrt(n))
# year    tot  mean    sd     n    se
# <fct> <dbl> <dbl> <dbl> <int> <dbl>
# 1 2022    841  10.5  6.17    80 0.690
# 2 2023    807  10.1 11.6     80 1.30 
# 1648

tot_all_years %>% 
  group_by(year) %>% 
  summarise(arn = sum(Aranaeomorphae),
            car = sum(Carabidae))
# year    arn   car
# <fct> <dbl> <dbl>
# 1 2022    530   208
# 2 2023    119   282

all_arth_bc %>% 
  mutate(Aranaeomorphae = Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Lyniphiidae + Araneae + Salticidae) %>% 
  group_by(year) %>% 
  summarise(lyc = sum(Lycosidae), 
            aran = sum(Aranaeomorphae)) 
# year    lyc  aran
# <fct> <dbl> <dbl>
# 1 2022    454    76
# 2 2023     85    34


# beetle genera

genus <- beans %>% 
  dplyr::select(-split, -life_stage, -sp) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = genus, 
              values_from = genus,
              values_fn = list(genus = length)) %>% 
  filter(family == 'Carabidae') %>% 
  dplyr::select(-Gladicosa, -Schizocosa, -na, -Pardosa, -Misumenoides, -Pachygnatha, -Trabeops,
                -Petalops, -Piratula, -Allocosa, -Varacosa, -unknown, -`NA`) %>% 
  replace(is.na(.),0) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:6, as.factor)  %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                                   plot %in% c(103,204,302,403,501) ~ 2,
                                   plot %in% c(102,201,303,402,502) ~ 3, 
                                   plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%  
  dplyr::select(-crop) %>% 
  mutate(crop = 'beans',
         crop = as.factor(crop)) %>% 
  relocate(year, date, crop) %>% 
  filter(date != '2022-8-18') %>% 
  print(n = Inf)


genus %>% 
  mutate(cicin = Cicindela + Cicindelia,
         Chlaenius = Chlaenius + Chalenius) %>% 
  group_by(year) %>% 
  summarise(pte = sum(Pterostichus),
            poe = sum(Poecilus),
            chl = sum(Chlaenius),
            poly = sum(Polyderis),
            har = sum(Harpalus),
            cic = sum(cicin),
            ago = sum(Agonum),
            not = sum(Notibia),
            ama = sum(Amara),
            bad = sum(Badister),
            bem = sum(Bembidion), 
            sten = sum(Stenolophus)
            )
# year    pte   poe   chl  poly   har   cic   ago   not   ama   bad   bem  sten
# <fct> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
# 1 2022     24   100    29     0     0     0     0     0    10     1     1     0
# 2 2023     75   110    13     1     1     0     1     6     0     0     0     0
# 372


ggplot(tot_se_years_bc_df, aes(x = reorder(crop, mean), y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 2, y=17, label = "***", size = 12)+
  labs(title = "Total arthropod by crop",
       subtitle = "Years: 2022-2023",
       x = 'Crop',
       y = 'Mean arthropod population / plot')+
  scale_x_discrete(labels=c('Corn', 'Soybeans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26))

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
  all_loop <- subset(func_tot, dplyr::select = c("crop", "trt", spss))
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

# new models ####

# Carabid plot 
cartabid_plot <- func_tot %>% 
  dplyr::select(year, crop, Carabidae) %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Carabidae),
            sd = sd(Carabidae), 
            n = n(),
            se = sd/sqrt(n))

carab_all <- glm.nb(Carabidae ~ crop , data = func_tot)
hist(residuals(carab_all))
cld(emmeans(carab_all, ~crop ), Letters = letters)
# crop  emmean    SE  df asymp.LCL asymp.UCL .group
# corn   0.253 0.145 Inf   -0.0316     0.537  a    
# beans  1.576 0.118 Inf    1.3452     1.808   b 

car<-ggplot(cartabid_plot, aes(x =crop, y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 2, y=6, label = "*", size = 12)+
  labs(title = "Carabid")+
       #x = 'Crop')+
  scale_x_discrete(limits = c('corn', 'beans'),
                   labels=c('Corn', 'Soybeans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26))

# Arane plot
arane_plot <- func_tot %>% 
  dplyr::select(year, crop, Aranaeomorphae) %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Aranaeomorphae),
            sd = sd(Aranaeomorphae), 
            n = n(),
            se = sd/sqrt(n))


aran_all <- glm.nb(Aranaeomorphae ~ crop , data = func_tot)
hist(residuals(aran_all))
cld(emmeans(aran_all, ~crop ), Letters = letters)
# crop  emmean    SE  df asymp.LCL asymp.UCL .group
# corn    1.17 0.119 Inf     0.937      1.40  a    
# beans   1.59 0.113 Inf     1.364      1.81   b  


ar <- ggplot(arane_plot, aes(x = crop, y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 2, y=6, label = "*", size = 12)+
  labs(title = "Araneomorph")+
      # x = 'Crop')+
  scale_x_discrete(limits = c('corn', 'beans'),
                   labels=c('Corn', 'Soybeans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26))


# lycosidae only
all_lyc <- all_arth_bc %>% 
  mutate(Aranaeomorphae = Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Lyniphiidae + Araneae + Salticidae,
         Non_Insect_Arth = Diplopoda + Chilopoda, Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Other_insects = Dermaptera + Coreidae) %>% 
  dplyr::select(-Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
                -Lyniphiidae, -Araneae, -Salticidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
                -Elateridae, -Opiliones, -Dermaptera, -Coreidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae,
         Coleoptera_larvae = 'Coleoptera larvae') %>% 
  mutate(date = as.factor(date))

lyc_plot <- all_lyc %>% 
  dplyr::select(year, crop, Lycosidae) %>% 
  group_by(crop) %>% 
  summarise(mean = mean(Lycosidae),
            sd = sd(Lycosidae), 
            n = n(),
            se = sd/sqrt(n))


lyc_mod <- glm.nb(Lycosidae ~ crop , data = all_lyc)
hist(residuals(lyc_mod))
cld(emmeans(lyc_mod, ~crop ), Letters = letters)
# crop  emmean    SE  df asymp.LCL asymp.UCL .group
# corn   0.875 0.138 Inf     0.605      1.15  a    
# beans  1.467 0.129 Inf     1.214      1.72   b  


lyc <- ggplot(lyc_plot, aes(x = crop, y = mean, fill = crop))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), color = 'black', alpha = 1, size = 1, width = 0.5)+
  annotate("text", x = 2, y=6, label = "*", size = 12)+
  labs(title = "Lycosidae",
       x = 'Crop')+
  scale_x_discrete(limits = c('corn', 'beans'),
                   labels=c('Corn', 'Soybeans'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02"))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26))





fig <- ggarrange(ar,car, lyc, labels = c("1", "2", "3"), font.label = list(size = 20, color = 'cornsilk4'))
annotate_figure(fig,
                bottom = text_grob("Crop", size = 32),
                left = text_grob("Average abundance / plot", size = 32, rot = 90))

# permanova all years ####
#
##
###

all_arth_bc
all_years_arth <- all_arth_bc[6:25]
all_years_scores <- vegdist(all_years_arth, method = "bray")

# crop is significant 
all_years_p1 <- adonis2(all_years_scores ~ crop + trt + date, perm = 999, method = "bray", data = all_arth_bc)
all_years_p1
# Df SumOfSqs      R2      F Pr(>F)    
# crop       1    2.695 0.05996 12.274  0.001 ***
#   trt        3    0.598 0.01331  0.908  0.561    
# date       1    7.843 0.17447 35.716  0.001 ***
#   Residual 154   33.816 0.75227                  
# Total    159   44.951 1.00000 


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


# all year stats and numbers ####
b_clean


