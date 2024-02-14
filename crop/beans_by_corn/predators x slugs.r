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
colnames(pf_wide)

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
  select(-Carabidae, -Cicindelidae, -Gryllidae, -Gyrillidae) %>% 
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
  select(-Carabidae, -Pterostichus, -Cicindelidae) %>% 
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

# plots ####
library(ggpubr)
library(RColorBrewer)

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
  select(-crop...5, - year...6, -trt...7)

ggplot(s_plot, aes(x = pred, y = slugs))+
  geom_point(size = 5,aes(color = trt)) +
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"),
                     labels=c("Check", "Brown", "Green", "GrBr"))+
  guides(color=guide_legend("Treatment"))+
  geom_smooth(method = "lm", size = 1.5, se = TRUE, color = "black")+
  labs(title = "Beans: Total Slug by predator populations",
       subtitle = "Years: 2022-2023",
       x = "Predator population",
       y = "Slug population")+
  annotate("text", x = 400, y = 150, label = "p = 0.00162 **", size = 8)+
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    axis.line = element_line(size = 1.25),
    axis.ticks = element_line(size = 1.25),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 18), 
    legend.text = element_text(size =16)
  )

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
  select(-crop...5, -year...6, -trt...7)

ggplot(c_plot, aes(x = pred, y = slugs, color = trt))+
  geom_point(size = 5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"),
                     labels=c("Check", "Brown", "Green", "GrBr"))+
  guides(color=guide_legend("Treatment"))+
  labs(title = "Corn: Total Slug by predator populations",
       subtitle = "Years: 2022-2023",
      x = "Predator population",
      y = "Slug population")+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.line = element_line(size = 1.25),
    axis.ticks = element_line(size = 1.25),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size =12)
  )

# all #
all_plot <- cbind(sum_slug, pf_clean) %>% 
  rename(crop = crop...1, 
         year = year...2) %>% 
  select(-crop...5, -year...6, -treatment)

ggplot(all_plot, aes(x = pred, y = slugs))+
  geom_point(size = 5, aes(color = trt))+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"),
                     labels=c("Check", "Brown", "Green", "GrBr"))+
  guides(color=guide_legend("Treatment"))+
  geom_smooth(method = "lm", linewidth = 1.5, color = "black")+
  labs(title = "Total Slug by predator populations",
       subtitle = "Years: 2022-2023",
       x = "Predator population",
       y = "Slug population")+
  annotate("text", x = 400, y = 350, label = "p = 0.0311 *", size = 8)+
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    axis.line = element_line(size = 1.25),
    axis.ticks = element_line(size = 1.25),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = 18), 
    legend.text = element_text(size =16)
  )

# stats on this ####
bm <- glm(slugs ~ pred, data = s_plot)
summary(bm)
hist(residuals(bm))

cm <- glm(slugs ~ pred, data = c_plot)
summary(cm)
hist(residuals(cm))


all_m <- glm(slugs ~ pred, data = all_plot)
summary(all_m)
hist(residuals(all_m))
