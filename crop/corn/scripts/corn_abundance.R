# Jared Adam
# CORN abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(plotly)
library(multcomp)
library(hrbrthemes)
library(viridis)
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

by_total <- pf_clean %>%
  mutate(total = sum(c_across(Lycosidae:Staph))) %>% 
  group_by(year) %>% 
  summarise(mean = mean(total),
            sd = sd(total),
            n = n(), 
            se = sd/sqrt(n))
ggplot(by_total, aes(x = year, y = mean)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se))
  
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
# c_22_scrs <- cbind(as.data.frame(scrs_c22), trt = cpf_2022$trt)
c_22_scrs <- cbind(as.data.frame(scrs_c22), time = cpf_2022$timing)

# 

c_22.fig <- plot_ly(c_22_scrs, x = ~NMDS1, y = ~NMDS2, z= ~NMDS3, color = ~time,
                    colors = c("#D95F02", "#1B9E77"))
c_22.fig <- c_22.fig %>% 
  add_markers()
c_22.fig

# 

# df for loop by year 

cpf_22__tot <- cpf_2022 %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Araneae +
           Lyniphiidae + Salticidae,
         Carabid = Carabidae + Cicindelidae,
         Non_Insect_Arth = Diplopoda + Chilopoda + Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Gryllidae = Gryllidae + Gyrillidae) %>% 
 dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Araneae, -Salticidae, 
         -Lyniphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Carabidae, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae)


sp_list <- cpf_22__tot[7:15]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(cpf_22__tot, select = c("timing", spss))
  colnames(loop) <- c("timing", "spss")
  
  model <- aov(spss ~ timing, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
colnames(sp_list)
tukey_list[[6]]
tukey_list[[7]]

carab_tot <- cpf_22__tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd/sqrt(n))
# diff       lwr       upr     p adj
# 2-1 -0.8 -1.887198 0.2871978 0.1445771

aran_tot <- cpf_22__tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/ sqrt(n))
# diff      lwr      upr     p adj
# 2-1  2.1 0.132338 4.067662 0.0371008

unique(cpf_22__tot$date)
ggplot(carab_tot, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  scale_x_discrete(labels = c("2022-05-28", "2022-07-01"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Carabidae population over time",
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
        plot.subtitle = element_text(size = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(aran_tot, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  scale_x_discrete(labels = c("2022-05-28", "2022-07-01"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Araneomorphae population over time",
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
        plot.subtitle = element_text(size = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1.2, y = 6, label = "a", size = 6)+
  annotate("text", x = 2.2, y = 8, label = "b", size = 6)


# table for the paper 
cpf_22__tot %>% 
  group_by(date, trt) %>% 
  summarise(sumC = sum(Carabid),
            sumA = sum(Aranaeomorphae))


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

c_23_fsc <- as.data.frame(scores(cord_23_3, 'sites'))
c_23_fsc <- cbind(as.data.frame(c_23_fsc),time = cpf_2023$timing)
# c_23_fsc$species <- rownames(c_23_fsc)

c_23.fig <- plot_ly(c_23_fsc, x = ~NMDS1, y = ~NMDS2, z= ~NMDS3, color = ~time,
                    colors = c("#D95F02", "#1B9E77"))
c_23.fig <- c_23.fig %>% 
  add_markers()
c_23.fig


# df for loop by year 

cpf_23_tot <- cpf_2023 %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Araneae +
           Lyniphiidae + Salticidae,
         Carabid = Carabidae + Cicindelidae,
         Non_Insect_Arth = Diplopoda + Chilopoda + Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Gryllidae = Gryllidae + Gyrillidae) %>% 
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Araneae, -Salticidae, 
         -Lyniphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Carabidae, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae)


sp_list <- cpf_23_tot[7:15]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(cpf_23_tot, select = c("timing", spss))
  colnames(loop) <- c("timing", "spss")
  
  model <- aov(spss ~ timing, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
colnames(sp_list)
tukey_list[[6]]
tukey_list[[7]]

carab_tot <- cpf_23_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  print(n = Inf)
# diff        lwr      upr     p adj
# 2-1 0.85 -0.1272317 1.827232 0.0863142

aran_tot <- cpf_23_tot %>% 
  group_by(timing) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/ sqrt(n)) %>% 
  print(n = Inf)
# diff        lwr       upr     p adj
# 2-1  0.2 -0.5488674 0.9488674 0.5919007

unique(cpf_23_tot$date)
ggplot(carab_tot, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  scale_x_discrete(labels = c("2023-06-26", "2023-07-28"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Carabidae population over time",
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
        plot.subtitle = element_text(size = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(aran_tot, aes(x = timing, y = mean, fill = timing))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  scale_x_discrete(labels = c("2023-06-26", "2023-07-28"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Araneomorphae population over time",
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
        plot.subtitle = element_text(size = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

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
# Df SumOfSqs      R2       F Pr(>F)    
# trt       3   0.8458 0.03654  1.5191  0.084 .  
# year      1   5.8449 0.25252 31.4933  0.001 ***
#   date      1   2.7221 0.11760 14.6672  0.001 ***
#   Residual 74  13.7337 0.59334                   
# Total    79  23.1464 1.00000 

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

# plot speciees
c_fsc <- as.data.frame(scores(cord_3, 'species'))
c_fsc$species <- rownames(c_fsc)

# plot year
scrs_yr <- scores(cord_3, display = "sites")
# c_22_scrs <- cbind(as.data.frame(scrs_c22), trt = cpf_2022$trt)
scrs_yr <- cbind(as.data.frame(scrs_yr), year = cpf_year$year)

c.fig <- plot_ly(scrs_yr, x = ~NMDS1, y = ~NMDS2, z= ~NMDS3, color = ~year,
                 colors = c("#D95F02", "#1B9E77"))
c.fig <- c.fig %>% 
  add_markers()
c.fig

# df for loop by year 

cpf_tot <- cpf_clean %>% 
  mutate(Aranaeomorphae = Lycosidae + Thomisidae + Tetragnathidae + Gnaphosidae + Araneae +
           Lyniphiidae + Salticidae,
         Carabid = Carabidae + Cicindelidae,
         Non_Insect_Arth = Diplopoda + Chilopoda + Opiliones,
         Other_Coleoptera = Staphylinidae + Elateridae,
         Gryllidae = Gryllidae + Gyrillidae) %>% 
  dplyr::select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Araneae, -Salticidae, 
         -Lyniphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Carabidae, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae) %>% 
  mutate(date = as.factor(date))


sp_list <- cpf_tot[6:14]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(cpf_tot, select = c("year", "date", spss))
  colnames(loop) <- c("year", "date", "spss")
  
  model <- aov(spss ~ year + date, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
summary_list
colnames(sp_list)
tukey_list[[6]] #spider
tukey_list[[7]] #carabid

carab_tot <- cpf_tot %>% 
  group_by(year) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  print(n = Inf)

# $year
# diff        lwr      upr     p adj
# 2023-2022 0.575 -0.1441065 1.294106 0.1154125
# 
# $date
# diff        lwr       upr     p adj
# 2022-07-01-2022-05-28 -0.800 -2.1412715 0.5412715 0.4036056
# 2023-06-26-2022-05-28 -0.825 -2.1662715 0.5162715 0.3760321
# 2023-07-28-2022-05-28  0.025 -1.3162715 1.3662715 0.9999575
# 2023-06-26-2022-07-01 -0.025 -1.3662715 1.3162715 0.9999575
# 2023-07-28-2022-07-01  0.825 -0.5162715 2.1662715 0.3760321
# 2023-07-28-2023-06-26  0.850 -0.4912715 2.1912715 0.3494089

ggplot(carab_tot, aes(x = year, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Carabidae population by year",
    # subtitle = "Years: 2022-2023",
    x = "Year",
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


aran_tot <- cpf_tot %>% 
  group_by(year, date) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/ sqrt(n)) %>% 
  print(n = Inf)

# $year
# diff      lwr      upr p adj
# 2023-2022 -4.85 -5.88566 -3.81434     0 # sig here
# 
# $date
# diff       lwr      upr     p adj
# 2022-07-01-2022-05-28  2.10  0.168296 4.031704 0.0277577 # sig here
# 2023-06-26-2022-05-28  0.95 -0.981704 2.881704 0.5709258
# 2023-07-28-2022-05-28  1.15 -0.781704 3.081704 0.4052903
# 2023-06-26-2022-07-01 -1.15 -3.081704 0.781704 0.4052903
# 2023-07-28-2022-07-01 -0.95 -2.881704 0.981704 0.5709258
# 2023-07-28-2023-06-26  0.20 -1.731704 2.131704 0.9929104


ggplot(aran_tot, aes(x = date, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~year, scales = "free_x")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Araneomorphae population x date and year",
    # subtitle = "Years: 2022-2023",
    x = "Sampling Date",
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

###
##
#

# table for paper 
trt_order <- c('No CC', "14-28 DPP", '3-7 DPP', '1-3 DAP')
total_corn <- cpf_tot %>% 
  mutate(trt = case_when(trt == '1' ~ 'No CC',
                         trt == '2' ~ '14-28 DPP',
                         trt == '3' ~ '3-7 DPP',
                         trt == '4' ~ '1-3 DAP',
                         .default = as.factor(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_order)) %>% 
  group_by(year, trt) %>% 
  summarise('Araneomorphae Sum' = sum(Aranaeomorphae),
            'Carabidae sum' = sum(Carabid)) %>% 
  rename(Year = year, 
         Treatment = trt)
  
corn_PF_table <- flextable(total_corn)
corn_PF_table <- autofit(corn_PF_table)
corn_PF_table <- add_header_lines(corn_PF_table, 
                 values = 'Corn: Pitfall totals by year')
theme_zebra(corn_PF_table) %>% 
  save_as_docx(path = 'corn_PF_table.docx')


# density plot test ####

dpt_trt <- cpf_tot %>% 
  mutate(Ensifera = Ensifera + Gyrillidae) %>% 
  group_by(trt) %>% 
  summarise(sum_for = sum(Formicidae),
            sum_enf = sum(Ensifera),
            sum_aran = sum(Aranaeomorphae),
            sum_car = sum(Carabid)) %>% 
  print(n = Inf)

density_tot <- dpt_trt %>% 
  pivot_longer(
    cols = where (is.numeric)) %>% 
  mutate(name = as.factor(name))

# video 
density_tot %>% 
  ggplot(aes(value, fill = name))+
  geom_histogram()

density_tot %>% 
  ggplot(aes(value, fill = name))+
  geom_density() # area inside the curve = 1

density_tot %>% 
  ggplot(aes(value, fill = name))+
  geom_density(adjust = 2) # fineness of the curve fit/ resolution

ggplot(density_tot, aes(x = value, group = name, fill = name))+
  geom_density(alpha = 0.5)+
  theme_ipsum()

ggplot(density_tot, aes(x = value, group = name, fill = name))+
  geom_density(adjust = 1, position = 'stack', alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2", labels = c("Araneomorphae", "Carabidae", "Ensifera", "Formicidae"))+
  labs(title = "Corn: Predator density",
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

ggplot(test_1, aes(x = value, fill = name))+
  geom_density(adjust = 1, alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(~name, labeller = labeller(name = name.labs))+
  labs(title = "Corn: Predator density x predator",
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

###
##
#

# iteration -- density by month ####

dpt_time <- cpf_tot %>% 
  mutate(Ensifera = Ensifera + Gyrillidae) %>% 
  group_by(date,trt) %>% 
  summarise(sum_for = sum(Formicidae),
            sum_enf = sum(Ensifera),
            sum_aran = sum(Aranaeomorphae),
            sum_car = sum(Carabid)) %>% 
  print(n = Inf)

dpt_time <- dpt_time %>% 
  pivot_longer(
    cols = where (is.numeric)) %>% 
  mutate(name = as.factor(name))

unique(dpt_time$date)
# Levels: 2022-05-28 2022-07-01 2023-06-26 2023-07-28

dates <- c('2022-05-28', '2022-07-01', '2023-06-26', '2023-07-28')

##
## this worked: What I am currently using: 3/13/2024
dpt_time %>% 
  group_nest(date) %>% 
  mutate(plot = map2(.x = data, 
                     .y = date,
                     .f = ~{
                       ggplot(.x, aes(x = value, fill = name))+
                         geom_density(adjust = 1, alpha = 0.7)+
                         scale_fill_brewer(palette = "Dark2", labels = c("Araneomorphae", "Carabidae", "Ensifera", "Formicidae"))+
                         labs(title = "Corn: Predator Abundance",
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
        .f = ~ggsave(paste0("corn_predators_", .y, ".png"), plot = .x))
##




dpt_nest <- dpt_time %>%
  group_nest(date)

plot_fxn <- function(data_df){
  ggplot(data_df, aes(x = value, fill = name))+
    geom_density(adjust = 1, alpha = 0.7)
}

###
# this worked 

dpt_plot <- dpt_nest %>% 
  mutate(plot = map(data, ~ggplot(., aes(x = value, fill = name)) + geom_density()))

help <- dpt_plot %>% 
  pull(plot)
###

###
new_df <- map2(
  .x = dpt_nest$plot,
  .y = dpt_nest$data,
  .f = function(.x, .y){
    plot = .x
  }
)

dpt_time %>% 
  group_split(date) %>% 
  map(ggplot(.x, aes(x = value, fill = name))+
                geom_density())

#

# pub plot ####

# spider and carabid

ggplot(carab_tot, aes(x = year, y = mean))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Carabidae population by year",
    # subtitle = "Years: 2022-2023",
    x = "Year",
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

ggplot(aran_tot, aes(x = date, y = mean))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~year, scales = "free_x")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Araneomorphae population x date and year",
    # subtitle = "Years: 2022-2023",
    x = "Sampling Date",
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




# density 

ggplot(test_1, aes(x = value, fill = name))+
  geom_density(adjust = 1, alpha = 0.7, fill = "black")+
  facet_grid(~name, labeller = labeller(name = name.labs))+
  labs(title = "Corn: Predator density x predator",
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

# old density ####
# 2022 = 266
# 2023 = 306
sum(266 + 306)
dpt_done <- dpt %>% 
  ungroup() %>% 
  mutate(tot = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop_for = sum_for/572, 
         prop_enf = sum_enf/572,
         prop_aran = sum_aran/572, 
         prop_car = sum_car/572) %>% 
  dplyr::select(-sum_for, -sum_enf, -sum_aran, -sum_car, -sum_gryl, -tot)

?pivot_longer
test <- dpt_done %>% 
  pivot_longer(
    cols = where(is.numeric)) %>% 
  mutate(name = as.factor(name)) %>% 
  print(n = Inf)

