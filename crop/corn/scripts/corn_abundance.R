# Jared Adam
# CORN abundance data: PF and Sweep nets
# started on 2/5/2024

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(plotly)
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
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Araneae, -Salticidae, 
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
  select(-Lycosidae, -Thomisidae, -Tetragnathidae, -Gnaphosidae, -Araneae, -Salticidae, 
         -Lyniphiidae, -Diplopoda, -Chilopoda, -Staphylinidae, 
         -Elateridae, -Opiliones, -Carabidae, -Cicindelidae) %>% 
  rename(Ensifera = Gryllidae,
         Caelifera = Acrididae)


sp_list <- cpf_tot[6:14]
summary_list <- list()
tukey_list <- list()

for(i in 1:9){
  print(i)
  spss <- colnames(sp_list[i])
  print(spss)
  loop <- subset(cpf_tot, select = c("year", spss))
  colnames(loop) <- c("year", "spss")
  
  model <- aov(spss ~ year, loop)
  
  aov_summary <- summary(model)
  summary_list[[i]] <- aov_summary
  
  aov_tukey <- TukeyHSD(model)
  tukey_list[[i]] <- aov_tukey
  
  
}
colnames(sp_list)
tukey_list[[6]]
tukey_list[[7]]

carab_tot <- cpf_tot %>% 
  group_by(year) %>% 
  summarise(mean = mean(Carabid), 
            sd = sd(Carabid), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  print(n = Inf)
# diff        lwr      upr     p adj
# 2023-2022 0.575 -0.1585217 1.308522 0.1226656

aran_tot <- cpf_tot %>% 
  group_by(year) %>% 
  summarise(mean = mean(Aranaeomorphae), 
            sd = sd(Aranaeomorphae), 
            n = n(), 
            se = sd/ sqrt(n)) %>% 
  print(n = Inf)
# diff       lwr       upr p adj
# 2023-2022 -4.85 -5.925771 -3.774229     0

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
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        # plot.subtitle = element_text(size = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(aran_tot, aes(x = year, y = mean, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#D95F02", "#1B9E77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),color = 'black', alpha = 1, size = 1, width = 0.5)+
  labs(
    title = "Corn: Araneomorphae population by year",
    # subtitle = "Years: 2022-2023",
    x = "Year",
    y = "Mean population"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        # plot.subtitle = element_text(size = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 1.2, y = 6.5, label = "a", size = 6)+
  annotate("text", x = 2.2, y = 1.5, label = "b", size = 6)
###
##
#

# density plot test ####
install.packages("hrbrthemes")
install.packages("viridis")
library(hrbrthemes)
library(viridis)


dpt <- cpf_tot %>% 
  group_by(trt) %>% 
  summarise(sum_for = sum(Formicidae),
            sum_enf = sum(Ensifera),
            sum_gryl = sum(Gyrillidae),
            sum_aran = sum(Aranaeomorphae),
            sum_car = sum(Carabid)) %>% 
  print(n = Inf)

# 2022 = 266
# 2023 = 306
sum(266 + 306)
dpt_done <- dpt %>% 
  ungroup() %>% 
  mutate(tot = rowSums(across(where(is.numeric)))) %>% 
  mutate(prop_for = sum_for/572, 
         prop_enf = sum_enf/572,
         prop_gyr = sum_gryl/572,
         prop_aran = sum_aran/572, 
         prop_car = sum_car/572) %>% 
  dplyr::select(-sum_for, -sum_enf, -sum_aran, -sum_car, -sum_gryl, -tot)

?pivot_longer
test <- dpt_done %>% 
  pivot_longer(
    cols = where(is.numeric)) %>% 
  mutate(name = as.factor(name)) %>% 
  print(n = Inf)



df %>%
  ggplot(aes(x = var, group = id, fill = factor(id), weight = weight)) + 
  geom_density(position = 'stack',alpha = .5)


ggplot(test, aes(x = name, group = name, fill = name, weight = value))+
  geom_density(position = "stack", alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2", labels = c("Araneomorphae", "Carabidae", "Ensifera", "Formicidae", "Gyrillidae"))+
  labs(title = "Corn: Predator density",
       subtitle = "Years: 2022-2023",
       y = "Density",
       fill = "Predator")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12))
    

install.packages('ggplot2movies')
library(ggplot2movies)
?movies
ggplot(movies, aes(x=rating, y=..density..)) + 
  geom_density(aes(fill=mpaa), position="stack")
