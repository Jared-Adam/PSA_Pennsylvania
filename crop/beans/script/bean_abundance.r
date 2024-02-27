# Jared Adam
# BEANS abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(plotly)
# data ####
bpf <- bean_pf
unique(bpf$crop)

# wrangling test ####
# need to pivot wider to get my taxa names as columns with counts 
bpf <- bpf %>% 
  arrange(date, plot)
bpf_test <- bpf[1:95, ]

bpf_pivot <- bpf_test %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length))
# wrangling ####
# whole data set 
bpf_wide <- bpf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(bpf_wide)
bpf_wide <- bpf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiide + Lyniphiidae + Linyphiidae) %>% 
  dplyr::select(-Liniphiide, -Lyniphiidae, -Linyphiidae, -na) %>% 
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
  dplyr::select(-crop) %>% 
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
b_22_scrs <- scores(bord_22_3, display = "sites")
b_22_trt <- cbind(as.data.frame(b_22_scrs), timing = bpf_2022$timing)

# b_22_fsc <- as.data.frame(scores(bord_22_3, "species"))
# b_22_fsc$species <- rownames(b_22_fsc)

plot_22 <- plot_ly(b_22_trt, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~timing, 
                   colors = c("#D95F02", "#1B9E77","#E7298A"))
plot_22 <- plot_22 %>% 
  add_markers()
plot_22

# loop 

bpf_2022

bpf_2022_tot <- bpf_2022 %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Linyphiidae,
         Carabid = Carabidae + Pterostichus, Cicindelidae,
         Non_Insect_Arth = Diplopoda + Chilopoda, Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Other_insects = Dermaptera) %>% 
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Linyphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae)


sp_list <- bpf_2022_tot[7:15]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(bpf_2022_tot, select = c("timing", "trt", spss))
  colnames(loop) <- c("timing", "trt", "spss")
  
  model <- aov(spss ~ timing + trt, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
colnames(sp_list)
# spider = 5 carabid = 6
tukey_list[[5]]
tukey_list[[6]]

unique(bpf_2022_tot$timing)
unique(bpf_2022_tot$crop)
carab_22 <- bpf_2022_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd / sqrt(n))
plot(carab_22$timing, carab_22$mean)
# diff       lwr       upr     p adj
# 2-1  0.9 -4.797151  6.597151 0.9233304
# 3-1 13.8  8.102849 19.497151 0.0000009
# 3-2 12.9  7.202849 18.597151 0.0000037


spider_22 <- bpf_2022_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Aranaeomorphae),
            sd = sd(Aranaeomorphae),
            n = n(), 
            se = sd / sqrt(n))
plot(spider_22$timing, spider_22$mean)
# diff       lwr      upr     p adj
# 2-1 2.30 -8.103933 12.70393 0.8556045
# 3-1 5.45 -4.953933 15.85393 0.4223682
# 3-2 3.15 -7.253933 13.55393 0.7470513

unique(bpf_2022$date)
ggplot(carab_22, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77","#E7298A"))+
  scale_x_discrete(labels = c("2022-05-28", "2022-07-01", "2022-08-18"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Carabidae population over time",
    subtitle = "Year: 2022",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1.2, y = 6, label = "a", size = 6)+
  annotate("text", x = 2.2, y = 7, label = "a", size = 6)+
  annotate("text", x = 3.2, y = 18.5, label = "b", size = 6)
  
  
  
  
ggplot(spider_22, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77","#E7298A"))+
  scale_x_discrete(labels = c("2022-05-28", "2022-07-01", "2022-08-18"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Araneomorphae population over time",
    subtitle = "Year: 2022",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
  
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

b_23_scrs <- scores(bord_23_3, display = "sites")
b_23_trt <- cbind(as.data.frame(b_23_scrs), trt = bpf_2023$trt)


b_23_fsc <- as.data.frame(scores(bord_23_3, 'species'))
b_23_fsc$species <- rownames(b_23_fsc)

b_23_date_scrs <- scores(bord_23_3, display = "sites")
b_23_date <- cbind(as.data.frame(b_23_date_scrs), time = bpf_2023$timing)


plot_23 <- plot_ly(b_23_trt, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~trt)
plot_23 <- plot_23 %>% 
  add_markers()
plot_23


fig.23 <- plot_ly(b_23_date, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~time,
                  colors = c("#D95F02", "#1B9E77"))
fig.23 <- fig.23 %>% 
  add_markers()
fig.23


# df for loop 

bpf_2023_tot <- bpf_2023 %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Linyphiidae,
         Carabid = Carabidae + Pterostichus, Cicindelidae,
         Non_Insect_Arth = Diplopoda + Chilopoda, Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Other_insects = Dermaptera) %>% 
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Linyphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae)

sp_list <- bpf_2023_tot[7:15]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(bpf_2023_tot, select = c("timing", "trt", spss))
  colnames(loop) <- c("timing", "trt", "spss")
  
  model <- aov(spss ~ timing + trt, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
colnames(sp_list)
tukey_list[[5]]
tukey_list[[6]]

carab_23 <- bpf_2023_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd/ sqrt(n))
# diff      lwr      upr     p adj
# 2-1  5.6 1.131917 10.06808 0.0155144

aran_23 <- bpf_2023_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/sqrt(n))
# diff        lwr     upr     p adj
# 2-1 0.75 -0.4380902 1.93809 0.2084307


unique(bpf_2023$date)
ggplot(carab_23, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  scale_x_discrete(labels = c("2023-06-26", "2023-07-28"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Carabidae population over time",
    subtitle = "Year: 2023",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1.2, y = 4, label = "a", size = 6)+
  annotate("text", x = 2.2, y = 9, label = "b", size = 6)
  
  
  
  
ggplot(aran_23, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  scale_x_discrete(labels = c("2023-06-26", "2023-07-28"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Araneomorphae population over time",
    subtitle = "Year: 2023",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
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

b_scrs <- scores(bord_3, display = "sites")
b_trt <- cbind(as.data.frame(b_scrs), trt = bpf_year$trt)


b_fsc <- as.data.frame(scores(bord_3, 'species'))
b_fsc$species <- rownames(b_fsc)

b_years <- scores(bord_3 , display = "sites")
b_years <- cbind(as.data.frame(b_years), year = bpf_year$year)

plot_year <- plot_ly(b_years, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~year,
                     colors = c("#D95F02", "#1B9E77"))
plot_year <- plot_year %>% 
  add_markers()
plot_year

##
#

