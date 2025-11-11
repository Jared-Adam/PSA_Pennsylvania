# Jared Adam 
# 2/13/2024
# going to regress predator populations by slugs here
# this will hold total bean and corn pops x slugs

# data from 2022 and 2023 bc associated pitfall traps 
# will be total predators x total slugs 

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(ggpubr)
library(RColorBrewer)
library(MASS)
library(emmeans)
library(ggpmisc)
library(lmtest)

# data ####
corn_pf
bean_pf
corn_slug <- PSA_PA_slugs
bean_slug <- slugs_beans_all

# slug wrangling ####

# KARN SLOOOOOG # 
cs <- corn_slug %>% 
  dplyr::select(-location, -shingle_id, -time, -row, -temp) %>% 
  rename(plot = plot_id) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
         treatment = as.factor(treatment),
         block = as.factor(block))%>% 
  group_by(season, year, month, plot, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  replace(is.na(.),0) %>% 
  mutate(crop = "corn") %>% 
  mutate(crop = as.factor(crop)) %>% 
print(n = Inf)


# BEAN SLOOOOOG #
bs <- bean_slug %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  mutate(block = as.factor(block)) %>%
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y'))  %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
         treatment = as.factor(treatment))%>%
  rename(season = seaon) %>% 
  group_by(season, year, month, plot, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)
bs <- bs[1:160,]
bs <- bs %>% 
  replace(is.na(.),0) %>% 
  mutate(crop = "bean") %>% 
  mutate(crop = as.factor(crop)) %>% 
  print(n = Inf)

final_slug <- rbind(bs, cs) %>% 
  filter(year != "2021")
unique(final_slug$year)


sum_slug <- final_slug %>% 
  ungroup() %>% 
  group_by(crop, year, treatment) %>% 
  summarise(slugs = sum(total_slug))

  
  
  
  
  
# pf wrangling ####

# KARN #

cpf_wide <- corn_pf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(cpf_wide)
cpf_wide <- cpf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lyn = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph= Staphylinidae + Staphylinidaa) %>% 
  dplyr::select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(cpf_wide)

cpf_clean <- cpf_wide %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                                   plot %in% c(103,204,302,403,501) ~ 2,
                                   plot %in% c(102,201,303,402,502) ~ 3, 
                                   plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%
  print(n = Inf)
colnames(cpf_clean)

c_clean <- cpf_clean %>% 
  rename(Lyniphiidae = Lyn,
         Staphylinidae = Staph, 
         Tetragnathidae = Tetrgnathidae) %>% 
  mutate(Carabidae_new = Carabidae + Cicindelidae,
         Gryll = Gryllidae +Gyrillidae) %>% 
  dplyr::select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
  rename(Carabidae = Carabidae_new,
         Gryllidae = Gryll) %>%
  print(n = Inf) 



# BEAN#

bpf_wide <- bean_pf %>% 
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
  relocate(year, date, crop, trt, plot) %>% 
  print(n = Inf)
colnames(bpf_clean)


b_clean <- bpf_clean %>% 
  rename('Coleoptera larvae' = Coleoptera)%>% 
  rename(Lyniphiidae = Lin) %>%
  mutate(Carabidae_new = Carabidae + Pterostichus +Cicindelidae) %>% 
  dplyr::select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
  rename(Carabidae = Carabidae_new) %>% 
  print(n = Inf)


pf_all <- rbind(b_clean, c_clean) %>% 
  replace(is.na(.), 0) %>% 
  mutate_at(vars(6:25), as.numeric)
colnames(pf_all)


pf_clean <- pf_all %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae, -Coreidae) %>% 
  mutate(predators = Lycosidae + Formicidae + Thomisidae + `Coleoptera larvae` + Staphylinidae + Opiliones + Gryllidae +
         Tetragnathidae + Chilopoda  + Gnaphosidae + Agelenidae + Lyniphiidae + Carabidae + Araneae + Salticidae) %>% 
  dplyr::select(year, crop, trt, predators) %>% 
  ungroup() %>% 
  group_by(crop, year, trt) %>% 
  summarise(pred = sum(predators))

# checking sum functions ####
#matches above
b_clean %>% 
  ungroup() %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae) %>% 
  mutate(total = rowSums(pick(where(is.numeric)))) %>% 
  group_by(year, crop) %>% 
  summarise(sum = sum(total))

b_clean %>% 
  ungroup() %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae) %>% 
  rowwise(crop) %>% 
  mutate(total = sum(c_across(where(is.numeric))))

# look to see which columns are driving these numbers
b_clean %>% 
  filter(year == "2022") %>% 
  ungroup() %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae) %>%
  group_by(crop, year) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  mutate(total = sum(c_across(where(is.numeric)))) %>% 
  relocate(crop, year, total)

bpf_clean %>% 
  filter(year == "2022") %>% 
  ungroup() %>% 
  dplyr::select(-Elateridae, -Diplopoda, -Dermaptera, -Acrididae) %>%
  group_by(crop, year) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))

# stats on this ####

# slugs : 
sum_slug
# pf : 
pf_clean

# all #
all_plot <- cbind(sum_slug, pf_clean) %>% 
  rename(crop = crop...1, 
         year = year...2) %>% 
  dplyr::select(-crop...5, -year...6, -treatment)

# model tests

poisson_model.2 <- glm(slugs ~ pred,
                       data = all_plot,
                       family = poisson)

nb_model_trt.2 <- glm.nb(slugs ~ pred, 
                         data = all_plot) 

lrtest(poisson_model.2,nb_model_trt.2)
# the negative binomial has the higher likelihood score, so we will use that



# beans # 
sum_sb <- sum_slug %>% 
  filter(crop == "bean") %>% 
  rename(trt = treatment)
pf_sb <- pf_clean %>% 
  filter(crop == "beans")
s_plot <- cbind(sum_sb, pf_sb) %>% 
  rename(crop = crop...1,
         year = year...2,
         trt = trt...3) %>% 
  dplyr::select(-crop...5, - year...6, -trt...7)



bm1 <- glm.nb(slugs ~ pred, data = s_plot)
summary(bm1)
hist(residuals(bm1))
# X2 p value derived from deviance = 
bX2 = (34.3557-8.3931)


# Karn #
sum_c <- sum_slug %>% 
  filter(crop == "corn") %>% 
  rename(trt = treatment)
pf_c <- pf_clean %>% 
  filter(crop == "corn")
c_plot <- cbind(sum_c, pf_c) %>% 
  rename(crop = crop...1,
         year = year...2, 
         trt = trt...3) %>% 
  dplyr::select(-crop...5, -year...6, -trt...7)

cm1 <- glm.nb(slugs ~ pred, data = c_plot)
summary(cm1)
hist(residuals(cm1))
cX2 = (13.7146-8.0861)



all_m <- glm(slugs ~ pred, data = all_plot)
summary(all_m)
hist(residuals(all_m))


# plots ####
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n=4, name = "Dark2")
brewer.pal(n=4, name = "Dark2")




# slugs : 
sum_slug
# pf : 
pf_clean

# beans # 
sum_sb <- sum_slug %>% 
  filter(crop == "bean") %>% 
  rename(trt = treatment)
pf_sb <- pf_clean %>% 
  filter(crop == "beans")
s_plot <- cbind(sum_sb, pf_sb) %>% 
  rename(crop = crop...1,
         year = year...2,
         trt = trt...3) %>% 
  dplyr::select(-crop...5, - year...6, -trt...7)

bean_regression <- s_plot %>%  
  ggplot(aes(x = pred, y = slugs))+
  geom_point(size = 10,aes(shape = trt)) +
  scale_shape_manual(limits = c("1", "2", "4","3"),
                     values = c(15,16,17,18),
                     labels=c("No CC", "Early", "Late", "Green"))+
  guides(shape=guide_legend("Treatment termination"))+
  geom_smooth(method = "lm", size = 1.5, se = TRUE, color = "black")+
  stat_poly_eq(label.x = "right", label.y = "top", size = 8)+
  labs(title = "Soybean",
       x = "Predator population",
       y = "Slug population"
       #        caption = "DPP: Days pre plant
       # DAP: Days after plant"
  )+
  annotate("text", x = 480, y = 225, label = "p value < .001", size = 8, fontface = 'italic')+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        axis.ticks = element_blank())



ggplot(s_plot, aes(x = pred, y = slugs))+
  geom_point(size = 10,aes(color = trt)) +
  scale_color_manual(limits = c("1", "2", "4","3"),
    values = c("#E7298A", "#D95F02", "#7570B3", "#1B9E77"),
                     labels=c("No CC", "Early", "Late", "Green"))+
  guides(color=guide_legend("Treatment termination"))+
  geom_smooth(method = "lm", size = 1.5, se = TRUE, color = "black")+
  stat_poly_eq(label.x = "right", label.y = "top", size = 12)+
  labs(title = "Soybean: Slug x Predator Populations",
       subtitle = "Years: 2022-2023",
       x = "Predator population",
       y = "Slug population"
#        caption = "DPP: Days pre plant
# DAP: Days after plant"
)+
  annotate("text", x = 470, y = 240, label = "p value < .001", size = 12, fontface = 'italic')+
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
  

# Karn #
sum_c <- sum_slug %>% 
  filter(crop == "corn") %>% 
  rename(trt = treatment)
pf_c <- pf_clean %>% 
  filter(crop == "corn")
c_plot <- cbind(sum_c, pf_c) %>% 
  rename(crop = crop...1,
         year = year...2, 
         trt = trt...3) %>% 
  dplyr::select(-crop...5, -year...6, -trt...7)

corn_regression <- c_plot %>% 
  ggplot(aes(x = pred, y = slugs))+
  geom_point(size = 10,aes(shape = trt)) +
  scale_shape_manual(limits = c("1", "2", "4","3"),
                     values = c(15,16,17,18),
                     labels=c("No CC", "Early", "Late", "Green"))+
  guides(shape=guide_legend("Treatment termination"))+
  geom_smooth(method = "lm", size = 1.5, se = TRUE, color = "black")+
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  labs(title = "Corn",
       x = "Predator population",
       y = "Slug population"
       #        caption = "DPP: Days pre plant
       # DAP: Days after plant"
  )+
  annotate("text", x = 78, y = 625, label = "p value < .01", size = 8, fontface = 'italic')+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        axis.ticks = element_blank())




ggplot(c_plot, aes(x = pred, y = slugs))+
  geom_point(size = 10, aes(color = trt))+
  scale_color_manual(limits = c("1", "2", "4","3"),
                     values = c("#E7298A", "#D95F02", "#7570B3", "#1B9E77"),
                     labels=c("No CC", "Early", "Late", "Green"))+
  geom_smooth(method = "lm", size = 1.5, se = TRUE, color = "black")+
  stat_poly_eq(size = 12)+
  guides(color=guide_legend("Treatment termination"))+
  labs(title = "Corn: Slug x Predator Populations",
       subtitle = "Years: 2022-2023",
      x = "Predator population",
      y = "Slug population"
#       caption = "DPP: Days pre plant
# DAP: Days after plant"
)+
  annotate("text", x = 80, y = 630, label = "p value < .01", size = 12, fontface = "italic")+
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



# combine regressions 
reg_fig <- ggarrange(corn_regression + rremove('xy.title') + rremove('legend'), bean_regression + rremove('xy.title'),
          ncol = 1,
          labels = c("1", "2"),
          font.label = list(size = 20, color = 'cornsilk4'))

annotate_figure(reg_fig,
                bottom = text_grob("Predator abundance", size = 32),
                left = text_grob("Slug abundance", size = 32, rot = 90))


# corn and bean lines on the same plot 

final_fig_df <- rbind(c_plot, s_plot) %>% 
  mutate(shape = case_when(crop == 'corn' & trt == '1' ~ 'c1',
                           crop == 'corn' & trt == '2' ~ 'c2', 
                           crop == 'corn' & trt == '3' ~'c3',
                           crop == 'corn' & trt == '4' ~ 'c4',
                           crop == 'bean' & trt == '1' ~ 'b1', 
                           crop == 'bean' & trt == '2' ~ 'b2', 
                           crop == 'bean' & trt == '3' ~ 'b3', 
                           crop == 'bean' & trt == '4' ~ 'b4')) %>% 
  mutate(shape = as.factor(shape))

final_fig <- final_fig_df %>% 
  ggplot(aes(x = pred, y = slugs, linetype = crop))+
  geom_point(size = 10, aes(shape = shape))+
  scale_shape_manual(limits = c("c1", "c2", "c4","c3", 'b1', 'b2', 'b4', 'b3'),
                     values = c(0,1,2,5,15,16,17,18),
                     labels=c(" Corn: No CC", "Corn: Early", " Corn: Late", "Corn: Green", 
                              " Soybean: No CC", "Soybean: Early", " Soybean: Late", "Soybean: Green"))+
  guides(shape=guide_legend("Crop: Treatment termination"))+
  guides(linetype = "none")+
  geom_smooth(method = "lm", size = 1.5, se = TRUE, color = 'black')+
  scale_linetype_manual(name = 'crop',
                        breaks = c('corn', 'bean'),
                        values = c(2,1))+
  theme_bw()+
  theme(legend.position = c(0.85, 0.75),
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        axis.ticks = element_blank())+
  labs(title = "Corn and Soybean",
       x = "Predator population",
       y = "Slug population")+
  annotate("text", x = 450, y = 300, label = 'Soybean: p < 0.001, R2 = 0.83', size = 8)+
  annotate("text", x = 440, y = 240, label = 'Corn: p < 0.01, R2 = 0.29', size = 8)
  
ggsave("2025-11-10_SxPred.png", plot = final_fig, dpi = 1000, width = 18, height = 10, units = "in")



# all #
all_plot <- cbind(sum_slug, pf_clean) %>% 
  rename(crop = crop...1, 
         year = year...2) %>% 
  dplyr::select(-crop...5, -year...6, -treatment)

ggplot(all_plot, aes(x = pred, y = slugs))+
  geom_point(size = 5, aes(color = trt))+
  scale_color_manual(limits = c("1", "2", "4","3"),
                     values = c("#E7298A", "#D95F02", "#7570B3", "#1B9E77"),
                     labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  guides(color=guide_legend("Treatment"))+
  geom_smooth(method = "lm", linewidth = 1.5, color = "black")+
  stat_poly_eq(label.x = "right", label.y = "top", size = 8)+
  labs(title = "Total Slug by predator populations",
       subtitle = "Years: 2022-2023",
       x = "Predator population",
       y = "Slug population")+
  # annotate("text", x = 465, y = 500, label = "p = 0.0311 *", size = 8)+
  theme(legend.position = "bottom",
        legend.key.size = unit(.25, 'cm'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
  

