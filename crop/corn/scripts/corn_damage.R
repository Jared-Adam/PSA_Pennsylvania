# Jared Adam 
# PA PSA Damage type assessment of corn
# 1/23/2024
# two collection timings: v3 and v5
# binomial data? 



# packages ####
library(tidyverse)
library(lme4)
library(MASS)
library(performance)
library(emmeans)
library(ggpubr)
# data ####
damage_type <- PSA_PA_damage



damage_type

unique(damage_type$treatment)
(NA %in% damage_type$treatment)
# all of 2023 does not have a treatment number.. whoops

damage_type <- damage_type %>% 
  mutate(treatment = case_when(plot_id %in% c(101,203,304,401,503) ~ 1,
                               plot_id %in% c(103,204,302,403,501) ~ 2,
                               plot_id %in% c(102,201,303,402,502) ~ 3, 
                               plot_id %in% c(104,202,301,404,504) ~ 4))

unique(damage_type$treatment)
(NA %in% damage_type$treatment)

unique(damage_type$damage_type)
# we need to create a new column for each damage type. 
# this will be accomplished by splitting the damage into new columns 

# test ####
test <- damage_type[1:300,]
unique(test$damage_type)


# this one works: will need to adapt column names a bit with the whole df 
df <- spread(test, damage_type, damage_type)
look <- df %>% 
  dplyr::select(-na) %>%
  unite(multiple, c('s, bcw' , 's, taw'), sep = " ", remove = TRUE, na.rm = TRUE) %>% 
  mutate(multiple = case_when(multiple != "" ~ 1)) %>% 
  #dplyr::select(-'s, bcw', -'s, taw') %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         #multiple = case_when(multiple %in% c('s, bcw' , 's, taw' , 's, bcw s, taw') ~ 1),
         taw = case_when(taw == 'taw' ~ 1)) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0))
sum(look$multiple)

# wrangling ####
# try on full data set 
dmg <- damage_type

dmg <- spread(dmg, damage_type, damage_type)
colnames(dmg[9:28])
new_dmg <- dmg %>% 
  dplyr::select(-location, -tempC, -na) %>% 
  unite(multiple, c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                     "s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb"), sep = "-", remove = TRUE, na.rm = TRUE) %>% 
  mutate(multiple = case_when(multiple != "" ~ 1)) %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         taw = case_when(taw == 'taw' ~ 1),
         sb = case_when(sb == 'sb' ~ 1),
         d = case_when(d == 'd' ~ 1),
         t = case_when(t == 't' ~ 1),
         other = case_when(other == 'other' ~ 1),
         #multiple = case_when(multiple %in% c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                                               #"s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb") ~ 1),
         ) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0)) %>% 
  dplyr::select(-d, -t) %>% # I think these two are typos and I removed them 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y")) %>%
  dplyr::select(-date) %>% 
  relocate(year) %>% 
  mutate(growth_stage = as.factor(growth_stage),
         block = as.factor(block),
         treatment = as.factor(treatment),
         year = as.factor(year)) %>% 
  print(n = 10)
#check before removing 
# unique(dmg_new$d), sum(dmg_new$d)
sum(new_dmg$multiple)
sum(new_dmg$s)
sum(new_dmg$other)
sum(new_dmg$sb)
sum(new_dmg$taw)
sum(new_dmg$bcw)
# these all have numbers 

# models ####
# test model to look at variables before we run the loop
dmg_model <- new_dmg
# sum(dmg_model$multiple)



test_m1 <- glmer(s ~ treatment + growth_stage +
                   (1|year/block/plot_id), data = dmg_model,
                 family = binomial)
summary(test_m1)
r2_nakagawa(test_m1)
hist(residuals(test_m1))

#this is the one 
test_m2 <- glmer(s ~ treatment  +
                   (1|year/growth_stage/block), data = dmg_model,
                 family = binomial)
summary(test_m2)
r2_nakagawa(test_m2)
hist(residuals(test_m2))

# m3 = overfot/ singular. Removing plot 
test_m3 <- glmer(s ~ treatment  +
                   (1|year/growth_stage/block/plot_id), data = dmg_model,
                 family = binomial)
summary(test_m3)
r2_nakagawa(test_m3)
hist(residuals(test_m3))

###
##
#
# model other, slug, and taw (singularity in the loop)
other <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, other))
other_m1 <- glmer(other ~ treatment +
                   (1|year), data = dmg_model,
                 family = binomial)
other_saved <- emmeans(other_m1, pairwise ~ treatment, type = 'response')
other_saved <- as.data.frame(other_saved$emmeans)
summary(other_m1)
r2_nakagawa(other_m1)
model_performance(other_m1)

# slug model # 
slug <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, s))
slug_m1 <- glmer(s ~ treatment +
                   (1|year/block), data = dmg_model,
                 family = binomial)
slug_saved <- emmeans(slug_m1, pairwise ~ treatment, type = 'response')
slug_saved <- as.data.frame(slug_saved$emmeans)
summary(slug_m1)
r2_nakagawa(slug_m1)

taw <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, taw))
taw_m1 <- glmer(taw ~ treatment+
                    (1|year/block), data = dmg_model,
                  family = binomial)
taw_saved <- emmeans(taw_m1, pairwise ~ treatment, type = 'response')
taw_saved <- as.data.frame(taw_saved$emmeans)
summary(taw_m1)
r2_nakagawa(taw_m1)

#
##
###

# old code to find 0 in trt
# unique(dmg_model$treatment)
# subset(dmg_model, !(0 %in% dmg_model$treatment))
# dmg_model <- dmg_model %>% 
#   filter(treatment != 0)

pest_columns <- c('bcw','sb', 'multiple')
summary_list <- list()
r2_list <- list()
emms_mod <- list()
for (pest in 1:length(pest_columns)) {
  print(pest)
  new_col <- pest_columns[pest]
  new_df <- subset(dmg_model, select = c('year', 'growth_stage', 'block', 'treatment', new_col))
  colnames(new_df) <- c('year', 'growth_stage', 'block', 'treatment', 'new_col')
  model <- glmer(new_col ~ treatment +
                   (1|year/growth_stage/block), data = new_df,
                 family = binomial)
  emms_mod[[pest]] <- emmeans(model, pairwise ~ as.factor(treatment),type = "response")
  summary_model <- summary(model)
  summary_list[[pest]] <- summary_model
  r2_model <- r2_nakagawa(model)
  r2_list[[pest]] <- r2_model
}

summary_list
r2_list
emms_mod

# idk how to loop this, so doing it manually 
bcw_saved <- emms_mod[[1]]
bcw_saved <- as.data.frame(bcw_saved$emmeans)
bcw_saved['pest'] <- ("bcw")

# s_saved <- emms_mod[[2]]
# s_saved <- as.data.frame(s_saved$emmeans)
# s_saved['pest'] <- ('slug')

sb_saved <- emms_mod[[2]]
sb_saved <- as.data.frame(sb_saved$emmeans)
sb_saved['pest'] <- ('stink bug')

# taw_saved <- emms_mod[[4]]
# taw_saved <- as.data.frame(taw_saved$emmeans)
# taw_saved['pest'] <- ('taw')

multiple_saved <- emms_mod[[3]]
multiple_saved <- as.data.frame(multiple_saved$emmeans)
multiple_saved['pest'] <- ('multiple')

# other_saved <- as.data.frame(other_saved$emmeans)
# other_saved['pest'] <- ('other')

all_emmeans <- rbind(bcw_saved, sb_saved, multiple_saved) #, other_saved
as_tibble(all_emmeans)
all_emmeans$pest <- as.factor(all_emmeans$pest)

# plot ####

# all years
#facet order
all_emmeans$pest_f <- factor(all_emmeans$pest, levels =c('slug', 'stink bug', 'bcw', 'taw',
                                               'multiple')) #'other', 

ggplot(all_emmeans, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, linewidth = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = asymp.LCL, ymax = asymp.UCL), 
                alpha = .6, width = 0, linewidth = 1)+
  facet_wrap(~pest_f, scales = "free")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))

# each plot individually then added together 
# template will be slug

slug_saved
slug_fig <- ggplot(slug_saved, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  labs(
    title = "Slug damage",
    subtitle = "Years: 2021-2023",
    #x = "Treatment",
    y = "Damage probability"
  )+
  annotate("text", x = 2, y = 0.68, label = "***", size = 8)+
  annotate("text", x = 3, y = 0.62, label = "**", size = 8)+
  annotate("text", x = 4, y = 0.61, label = "*", size = 8)+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

bcw_saved
bcw_fig <- ggplot(bcw_saved, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  labs(
    title = "Black Cutworm damage",
    subtitle = "Years: 2021-2023",
    #x = "Treatment",
    y = "Damage probability"
  )+
  annotate("text", x = 2, y = 0.045, label = "**", size = 8)+
  # annotate("text", x = 3, y = 0.62, label = "p = 1.59e-15 ***", size = 6)+
  annotate("text", x = 4, y = 0.036, label = "***", size = 8)+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

taw_saved
taw_fig <- ggplot(taw_saved, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  labs(
    title = "True Armyworm damage",
    subtitle = "Years: 2021-2023",
    #x = "Treatment",
    y = "Damage probability"
  )+
  # annotate("text", x = 2, y = 0.68, label = "p = 7.54e-10 ***", size = 6)+
  # annotate("text", x = 3, y = 0.62, label = "p = 1.59e-15 ***", size = 6)+
  annotate("text", x = 4, y = 0.05, label = "*", size = 8)+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


sb_saved
sb_fig <- ggplot(sb_saved, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  labs(
    title = "Stink bug damage",
    subtitle = "Years: 2022-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  annotate("text", x = 2, y = 0.005, label = "***", size = 8)+
  # annotate("text", x = 3, y = 0.62, label = "p = 1.59e-15 ***", size = 6)+
  annotate("text", x = 4, y = 0.006, label = "*", size = 8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


multiple_saved
mult_fig <- ggplot(multiple_saved, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  labs(
    title = "Multiple damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  # annotate("text", x = 2, y = 0.005, label = "p = 0.000644 ***", size = 6)+
  annotate("text", x = 3, y = 0.105, label = "***", size = 8)+
  # annotate("text", x = 4, y = 0.006, label = "p = 0.017202 *", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

other_saved
other_fig <- ggplot(other_saved, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  labs(
    title = "Other damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  # annotate("text", x = 2, y = 0.68, label = "p = 7.54e-10 ***", size = 6)+
  # annotate("text", x = 3, y = 0.62, label = "p = 1.59e-15 ***", size = 6)+
  # annotate("text", x = 4, y = 0.61, label = "p = 0.01319 *", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(slug_fig, bcw_fig, taw_fig, sb_fig, mult_fig, other_fig)
