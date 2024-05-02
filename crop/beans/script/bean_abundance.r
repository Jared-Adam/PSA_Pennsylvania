# Jared Adam
# BEANS abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(plotly)
library(hrbrthemes)
library(viridis)
library(flextable)
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

# too many 2022 samples here 
# bea_total <- bpf_clean %>%
#   mutate(total = sum(c_across(Lycosidae:Lin))) %>% 
#   group_by(year) %>% 
#   summarise(mean = mean(total),
#             sd = sd(total),
#             n = n(), 
#             se = sd/sqrt(n))
# ggplot(bea_total, aes(x = year, y = mean)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_errorbar(aes(ymin = mean-se, ymax = mean+se))

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
  dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
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
# timing / dates
# diff       lwr       upr     p adj
# 2-1  0.9 -4.797151  6.597151 0.9233304
# 3-1 13.8  8.102849 19.497151 0.0000009
# 3-2 12.9  7.202849 18.597151 0.0000037

car_22 <- glm.nb(Carabid ~ timing, data= bpf_2022_tot)
hist(residuals(car_22))
cld(emmeans(car_22, ~timing), Letters = letters)
# timing emmean    SE  df asymp.LCL asymp.UCL .group
# 1        1.31 0.167 Inf     0.981      1.64  a    
# 2        1.53 0.159 Inf     1.215      1.84  a    
# 3        2.86 0.131 Inf     2.605      3.12   b   




spider_22 <- bpf_2022_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Aranaeomorphae),
            sd = sd(Aranaeomorphae),
            n = n(), 
            se = sd / sqrt(n))
plot(spider_22$timing, spider_22$mean)
# timing / dates
# diff       lwr      upr     p adj
# 2-1 2.30 -8.103933 12.70393 0.8556045
# 3-1 5.45 -4.953933 15.85393 0.4223682
# 3-2 3.15 -7.253933 13.55393 0.7470513

ara_22 <- glm.nb(Aranaeomorphae ~ timing, data = bpf_2022_tot)
hist(residuals(ara_22))
cld(emmeans(ara_22, ~timing), Letters = letters)
# timing emmean    SE  df asymp.LCL asymp.UCL .group
# 1        1.86 0.212 Inf      1.45      2.28  a    
# 2        2.17 0.207 Inf      1.76      2.57  a    
# 3        2.48 0.203 Inf      2.08      2.87  a  




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

aran_car_22 <- bpf_2022_tot  %>% 
  mutate(trt = factor(trt, levels = trt_order)) %>% 
  group_by(date) %>% 
  summarise('Araneomorphae Sum' = mean(Aranaeomorphae),
            asd = sd(Aranaeomorphae),
            n = n(),
            ase = asd/sqrt(n),
            'Carabidae sum' = mean(Carabid),
            csd = sd(Carabid),
            cse = csd/ sqrt(n)
  )


bean_22_table <- flextable(aran_car_22)
bean_22_table <- autofit(bean_22_table)
bean_22_table <- add_header_lines(bean_22_table, 
                                  values = 'Soybean: 2022 Pitfall totals by date')
theme_zebra(bean_22_table) %>% 
  save_as_docx(path = 'bean_22_table.docx')


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
  dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
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

car_23 <- glm.nb(Carabid ~ timing, data = bpf_2023_tot)
hist(residuals(car_23))
cld(emmeans(car_23, ~timing),Letters = letters)
# timing emmean    SE  df asymp.LCL asymp.UCL .group
# 1       0.916 0.281 Inf     0.366      1.47  a    
# 2       2.092 0.255 Inf     1.592      2.59   b  


aran_23 <- bpf_2023_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/sqrt(n))
# diff        lwr     upr     p adj
# 2-1 0.75 -0.4380902 1.93809 0.2084307

ara_23 <- glm.nb(Aranaeomorphae ~ timing, data = bpf_2023_tot)
hist(residuals(ara_23))
cld(emmeans(ara_23, ~timing),Letters = letters)
# timing emmean    SE  df asymp.LCL asymp.UCL .group
# 1       0.588 0.194 Inf     0.208     0.967  a    
# 2       0.936 0.171 Inf     0.601     1.272  a  


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

aran_car_23 <- bpf_2023_tot %>% 
  group_by(date) %>% 
  summarise('Araneomorphae Sum' = mean(Aranaeomorphae),
            asd = sd(Aranaeomorphae),
            n = n(),
            ase = asd/sqrt(n),
            'Carabidae sum' = mean(Carabid),
            csd = sd(Carabid),
            cse = csd/ sqrt(n)
            )


bean_23_table <- flextable(aran_car_23)
bean_23_table <- autofit(bean_23_table)
bean_23_table <- add_header_lines(bean_23_table, 
                                  values = 'Soybean: 2023 Pitfall totals by date')
theme_zebra(bean_23_table) %>% 
  save_as_docx(path = 'bean_23_table.docx')



# 22 and 23 ####
colnames(bpf_clean)
unique(bpf_clean$date)
bpf_clean <- bpf_clean %>% 
  dplyr::rename(Linyphiidae = Lin)
bpf_clean <- bpf_clean %>% 
  filter(date != '2022-08-18')

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
# Df SumOfSqs      R2      F Pr(>F)    
# year      1   4.2432 0.21504 31.973  0.001 ***
#   date      2   5.2960 0.26839 19.953  0.001 ***
#   trt       3   0.5052 0.02560  1.269  0.169    
# Residual 73   9.6878 0.49096                  
# Total    79  19.7322 1.00000   


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


# df for loop by year 

bpf_tot <- bpf_clean %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Agelenidae +
           Linyphiidae,
         Carabid = Carabidae + Pterostichus, Cicindelidae,
         Non_Insect_Arth = Diplopoda + Chilopoda, Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Other_insects = Dermaptera) %>% 
  dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Agelenidae, 
         -Linyphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Dermaptera, -Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae) %>% 
  mutate(date = as.factor(date))


sp_list <- bpf_tot[6:14]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(bpf_tot, select = c("year", "date", spss))
  colnames(loop) <- c("year", "date", "spss")
  
  model <- aov(spss ~ year + date, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
colnames(sp_list)
tukey_list[[5]] #spider 
tukey_list[[6]] #carabid

carab_tot <- bpf_tot %>% 
  group_by(year, date) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd/sqrt(n))

car <- glm.nb(Carabid ~ date, data = bpf_tot)
hist(residuals(car))
cld(emmeans(car, ~date), Letters = letters)
# date       emmean    SE  df asymp.LCL asymp.UCL .group
# 2023-06-26  0.916 0.234 Inf     0.459      1.37  a    
# 2022-05-28  1.308 0.219 Inf     0.879      1.74  a    
# 2022-07-01  1.526 0.213 Inf     1.108      1.94  ab   
# 2023-07-28  2.092 0.202 Inf     1.696      2.49   b   



#### NOT USING THIS #
# $year
# diff       lwr      upr     p adj
# 2023-2022 1.15 -1.192027 3.492027 0.3311938
# 
# $date
# diff       lwr      upr     p adj
# 2022-07-01-2022-05-28  0.90 -3.468329 5.268329 0.9486284
# 2023-06-26-2022-05-28 -2.35 -6.718329 2.018329 0.4951801
# 2023-07-28-2022-05-28  3.25 -1.118329 7.618329 0.2145644
# 2023-06-26-2022-07-01 -3.25 -7.618329 1.118329 0.2145644
# 2023-07-28-2022-07-01  2.35 -2.018329 6.718329 0.4951801
# 2023-07-28-2023-06-26  5.60  1.231671 9.968329 0.0064222

ggplot(carab_tot, aes(x = date, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  facet_wrap(~year, scales = "free_x")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Carabidae population x date and year",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        # plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 28),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


aran_tot <- bpf_tot %>% 
  group_by(year) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/ sqrt(n))

ar <- glm.nb(Aranaeomorphae ~ date, data = bpf_tot)
hist(residuals(ar))
cld(emmeans(ar, ~date), Letters = letters)
# date       emmean    SE  df asymp.LCL asymp.UCL .group
# 2023-06-26  0.588 0.198 Inf     0.199     0.976  a    
# 2023-07-28  0.936 0.176 Inf     0.590     1.282  a    
# 2022-05-28  1.864 0.139 Inf     1.592     2.136   b   
# 2022-07-01  2.169 0.131 Inf     1.912     2.427   b 



# $year
# diff       lwr       upr p adj
# 2023-2022 -5.425 -7.106049 -3.743951     0
# 
# $date
# diff        lwr      upr     p adj
# 2022-07-01-2022-05-28  2.300 -0.8354784 5.435478 0.2255744
# 2023-06-26-2022-05-28  0.775 -2.3604784 3.910478 0.9154931
# 2023-07-28-2022-05-28  1.525 -1.6104784 4.660478 0.5799064
# 2023-06-26-2022-07-01 -1.525 -4.6604784 1.610478 0.5799064
# 2023-07-28-2022-07-01 -0.775 -3.9104784 2.360478 0.9154931
# 2023-07-28-2023-06-26  0.750 -2.3854784 3.885478 0.9226384


ggplot(aran_tot, aes(x = year, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Araneomorphae population x year",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        # plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        # strip.text = element_text(size = 28),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
###
##
#

# table for paper 

total_bean <- bpf_tot  %>% 
  group_by(date) %>% 
  summarise('Araneomorphae Sum' = mean(Aranaeomorphae),
            asd = sd(Aranaeomorphae),
            n = n(),
            ase = asd/sqrt(n),
            'Carabidae sum' = mean(Carabid),
            csd = sd(Carabid),
            cse = csd/ sqrt(n)
  )

bean_PF_table <- flextable(total_bean)
bean_PF_table <- autofit(bean_PF_table)
bean_PF_table <- add_header_lines(bean_PF_table, 
                                  values = 'Soybean: Pitfall totals by year')
theme_zebra(bean_PF_table) %>% 
  save_as_docx(path = 'bean_PF_table.docx')
# density ####

dbpt <- bpf_tot %>% 
  group_by(trt) %>% 
  summarise(sum_for = sum(Formicidae),
            sum_enf = sum(Ensifera),
            sum_aran = sum(Aranaeomorphae),
            sum_car = sum(Carabid)) %>% 
  print(n = Inf)

dbpt_plot <- dbpt %>% 
  pivot_longer(
    cols = where (is.numeric)) %>% 
  mutate(name = as.factor(name))

ggplot(dbpt_plot, aes(x = value, group = name, fill = name))+
  geom_density(adjust = 1, position = 'stack', alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2", labels = c("Araneomorphae", "Carabidae", "Ensifera", "Formicidae"))+
  labs(title = "Soybean: Predator density",
       subtitle = "Years: 2022-2023",
       y = "Density",
       x = "Abundance Counts",
       fill = "Predator:")+
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
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

name.labs <- c("Formicidae", "Ensifera", "Araneomorphae", "Carabidae")
names(name.labs) <- c("sum_for", "sum_enf", "sum_aran", "sum_car")

ggplot(dbpt_plot, aes(x = value, fill = name))+
  geom_density(adjust = 1, alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(~name, labeller = labeller(name = name.labs))+
  labs(title = "Soybean: Predator density x predator",
       subtitle = "Years: 2022-2023",
       y = "Density",
       x = "Abundance Counts",
       fill = "Predator:")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# iteration -- density plots by time ####
dbpt_time <- bpf_tot %>% 
  group_by(date, trt) %>% 
  summarise(sum_for = sum(Formicidae),
            sum_enf = sum(Ensifera),
            sum_aran = sum(Aranaeomorphae),
            sum_car = sum(Carabid)) %>% 
  print(n = Inf)

dbpt_time <- dbpt_time %>% 
  pivot_longer(
    cols = where (is.numeric)) %>% 
  mutate(name = as.factor(name))

dbpt_time %>% 
  group_nest(date) %>% 
  mutate(plot = map2(.x = data, 
                     .y = date,
                     .f = ~{
                       ggplot(.x, aes(x = value, fill = name))+
                         geom_density(adjust = 1, alpha = 0.7)+
                         scale_fill_brewer(palette = "Dark2", labels = c("Araneomorphae", "Carabidae", "Ensifera", "Formicidae"))+
                         labs(title = "Soybeans: Predator Abundance",
                              subtitle = .y,
                              y = "Density",
                              x = "Abundance Counts",
                              fill = "Predator:")+
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
                               plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
                     })) %>% 
  walk2(.x = .$plot, 
        .y = .$date,
        .f = ~ggsave(paste0("soybean_predators_", .y, ".png"), plot = .x, height = 12, width = 16, units = 'in'))
##








# pub plots ####

ggplot(carab_tot, aes(x = date, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, fill = "black")+
  facet_wrap(~year, scales = "free_x")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Carabidae population x date and year",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        # plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 28),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

ggplot(aran_tot, aes(x = year, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, fill = "black")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Soybean: Araneomorphae population x year",
    x = "Timing",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        # plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        # strip.text = element_text(size = 28),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# density 

ggplot(dbpt_plot, aes(x = value, fill = name))+
  geom_density(adjust = 1, alpha = 0.7, fill = "black")+
  facet_grid(~name, labeller = labeller(name = name.labs))+
  labs(title = "Soybean: Predator density x predator",
       subtitle = "Years: 2022-2023",
       y = "Density",
       x = "Abundance Counts",
       fill = "Predator:")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
