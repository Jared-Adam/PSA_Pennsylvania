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
library(rempsyc)
library(multcomp)
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
  mutate(treatment = case_when(plot_id %in% c("101",'203','304','401', '503') ~ "1",
                               plot_id %in% c('102', '201','303','402','502') ~ "3",
                               plot_id %in% c('103','204', '302', '403', '501') ~ "2",
                               plot_id %in% c('104', '202', '301', '404', '504') ~ "4")) %>% 
  mutate(growth_stage = as.factor(growth_stage),
         block = as.factor(block),
         treatment = as.factor(treatment),
         year = as.factor(year)) %>% 
  mutate(damage_score = case_when(damage_score == "na" ~ "0",
                                  is.na(damage_score) ~ "0",
                                  .default = as.character(damage_score))) %>% 
  replace(is.na(.),0) %>% 
  mutate(damage_score = as.numeric(damage_score)) %>% 
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

# damage severity models ####
new_dmg
unique(new_dmg$damage_score)
dmg_sev <- new_dmg %>% 
  mutate(plot_id = as.factor(plot_id))

avg_dmg <- dmg_sev %>%
  dplyr::select(treatment, year, growth_stage, block, plot_id, damage_score) 

sm0 <- glmer(damage_score ~ 
               (1|year/block/growth_stage), 
             data = avg_dmg, 
             family = poisson)

sm1 <- glmer(damage_score ~ treatment +
                  (1|year/block/growth_stage), 
                data = avg_dmg, 
             family = poisson)

sm2 <- glmer(damage_score ~ treatment + growth_stage +
                  (1|year/block/growth_stage), 
                data = avg_dmg, 
             family = poisson)

sm3 <- glmer(damage_score ~ treatment*growth_stage +
                  (1|year/block/growth_stage), 
                data = avg_dmg, 
                family = poisson)



anova(sm0, sm1, sm2, sm3)
summary(sm3)
binned_residuals(sm3)
check_model(sm3)
r2_nakagawa(sm3)
# Conditional R2: 0.094
# Marginal R2: 0.027

sm_em <- emmeans(sm3, ~treatment*growth_stage)
dmg_score_plot <- cld(sm_em, Letters = letters)


d_s.table <- as.data.frame(summary(sm3)$coefficients)
d_s.table <-cbind(row.names(d_s.table), d_s.table)
d_s.table <- as_tibble(d_s.table) %>% 
  mutate(`row.names(d_s.table)` = case_when(`row.names(d_s.table)` == 'treatment2' ~ '14-28 DPP',
                          `row.names(d_s.table)` == 'treatment4' ~ '3-7 DPP',
                          `row.names(d_s.table)` == 'treatment3' ~ '1-3 DAP',
                          `row.names(d_s.table)` == 'growth_stageV5' ~ 'V5',
                          `row.names(d_s.table)` == 'treatment2:growth_stageV5' ~ '14-28 DPP:V5',
                          `row.names(d_s.table)` == 'treatment3:growth_stageV5' ~ '1-3 DAP:V5',
                          `row.names(d_s.table)` == 'treatment4:growth_stageV5' ~ '3-7 DPP:V5',
                          .default = as.character(`row.names(d_s.table)`)))
names(d_s.table) <- c("Term", "B", "SE", "t", "p")
d_s.table <- flextable(d_s.table)
d_s.table <- autofit(d_s.table)
theme_zebra(d_s.table) %>% 
  save_as_docx(path = 'damage_coef_summarytable.docx')



# avg df plot 

avg_dam_p <- dmg_sev %>% 
  group_by(treatment, year, growth_stage) %>% 
  summarise(mean = mean(damage_score),
            sd = sd(damage_score), 
            n = n(), 
            se = sd/sqrt(n))

trt_ord <- c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP")
avg_for_paper <- dmg_sev %>% 
  mutate(treatment = case_when(treatment == '1' ~ 'No CC',
                               treatment == '2' ~ '14-28 DPP',
                               treatment == '3' ~ '1-3 DAP',
                               treatment == '4' ~ '3-7 DPP')) %>% 
  mutate(treatment = factor(treatment, levels = trt_ord)) %>% 
  group_by(treatment, growth_stage) %>% 
  summarise(mean = mean(damage_score),
            sd = sd(damage_score), 
            n = n(), 
            se = sd/ sqrt(n))
names(avg_for_paper) <- c("Treatment", "Growth Stage", "Mean","Sd", "n", "SE")
avg_dmg_table <- flextable(avg_for_paper)
avg_dmg_table <- autofit(avg_dmg_table)
theme_zebra(avg_dmg_table) %>% 
  save_as_docx(path = 'average.dmg.trt.gs.docx')



ggplot(avg_dam_p, aes(x = treatment, y = mean))+
  geom_bar(stat= 'identity', position = 'dodge')+
  facet_wrap(~year)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se))

# sum df plot
sum_dmg <- dmg_sev %>% 
  group_by(treatment, year, growth_stage, plot_id) %>% 
  summarise(sum = sum(damage_score))

a1 <- aov(sum ~ year , data = sum_dmg)
TukeyHSD(a1)  

# dmg severity plot ####

# by growth stage
ggplot(avg_dam_p, aes(x = treatment, y = mean, fill = treatment))+
  facet_wrap(~growth_stage)+
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs(title = 'Corn: Average Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment',
       y = 'Average Damage Score x Treatment (x / n)',
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

ggplot(avg_dam_p, aes(x = treatment, y = mean, fill = treatment))+
  facet_wrap(~growth_stage)+
  geom_boxplot(alpha = 0.5, fill = "black")+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs(title = 'Corn: Average Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment',
       y = 'Average Damage Score x Treatment (x / n)',
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))



sum_dmg

year.labs <- c("2021  a", "2022  b", "2023  c")
names(year.labs) <- c("2021", "2022", "2023")

ggplot(sum_dmg, aes(x = treatment, y = sum, fill = treatment))+
  geom_violin(alpha = 0.7)+
  geom_boxplot(width = 0.1, fill = 'white')+
  facet_wrap(~year, labeller = labeller(year = year.labs))+
  geom_point(size = 2)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs(title = 'Corn: Total Damage Score x Treatment and Year',
       x = 'Treatment',
       y = 'Total Damage Score x Plot (0-4)',
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))



ggplot(sum_dmg, aes(x = treatment, y = sum, fill = treatment))+
  geom_violin(alpha = 0.5, fill = 'black')+
  geom_boxplot(width = 0.1, fill = 'white')+
  facet_wrap(~year, labeller = labeller(year = year.labs))+
  geom_point()+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs(title = 'Corn: Total Damage Score x Treatment and Year',
       x = 'Treatment',
       y = 'Total Damage Score x Plot (0-4)',
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


# slug models and plot ####

slug_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, s)
unique(slug_model$treatment)
m0 <- glmer(s ~ +
        (1|year/block/plot_id/growth_stage), data = slug_model,
      family = binomial)

m1 <- glmer(s ~ treatment +
              (1|year/block/plot_id/growth_stage), data = slug_model,
            family = binomial)

m2 <- glmer(s ~ treatment+growth_stage +
              (1|year/block/plot_id/growth_stage), data = slug_model,
            family = binomial)

m3 <- glmer(s ~ treatment*growth_stage +
              (1|year/block/plot_id/growth_stage), data = slug_model,
            family = binomial)

sl.table <- as.data.frame(summary(m3)$coefficients)
#CI <- confint(m3)
sl.table <-cbind(row.names(sl.table), sl.table)
names(sl.table) <- c("Term", "B", "SE", "t", "p")
sl.table <- as_tibble(sl.table) %>% 
  mutate(Term = case_when(Term == 'treatment2' ~ '14-28 DPP',
         Term == 'treatment4' ~ '3-7 DPP',
         Term == 'treatment3' ~ '1-3 DAP',
         Term == 'growth_stageV5' ~ 'V5',
         Term == 'treatment2:growth_stageV5' ~ '14-28 DPP:V5',
         Term == 'treatment3:growth_stageV5' ~ '1-3 DAP:V5',
         Term == 'treatment4:growth_stageV5' ~ '3-7 DPP:V5',
         .default = as.character(Term)))
sl.table <- flextable(sl.table)
sl.table <- autofit(sl.table)
sl.table <- add_header_lines(sl.table,
                              values = 'Slug: Summary')
theme_zebra(sl.table) %>% 
  save_as_docx(path = 'slug_summary_table.docx')


anova(m0, m1, m2, m3)
summary(m3)
r2_nakagawa(m3)
m3_em <- emmeans(m3, ~treatment+growth_stage)
cld(m3_em, Letters = letters)


slug_em <- as.data.frame(m3_em)
ggplot(slug_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Slug Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

ggplot(slug_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75), color = "black")+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Slug Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# bcw models and plot ####
bcw_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, bcw) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

bm0 <- glmer(bcw ~ +
              (1|year/block/plot_id/growth_stage), data = bcw_model,
            family = binomial)

bm1 <- glmer(bcw ~ treatment +
              (1|year/block/plot_id/growth_stage), data = bcw_model,
            family = binomial)

bm2 <- glmer(bcw ~ treatment+growth_stage +
              (1|year/block/plot_id/growth_stage), data = bcw_model,
            family = binomial)

bm3 <- glmer(bcw ~ treatment*growth_stage +
              (1|year/block/plot_id/growth_stage), data = bcw_model,
            family = binomial)

bcw.table <- as.data.frame(summary(bm3)$coefficients)
#CI <- confint(m3)
bcw.table <-cbind(row.names(bcw.table), bcw.table)
names(bcw.table) <- c("Term", "B", "SE", "t", "p")
bcw.table <- as_tibble(bcw.table) %>% 
  mutate(Term = case_when(Term == 'treatment2' ~ '14-28 DPP',
                          Term == 'treatment4' ~ '3-7 DPP',
                          Term == 'treatment3' ~ '1-3 DAP',
                          Term == 'growth_stageV5' ~ 'V5',
                          Term == 'treatment2:growth_stageV5' ~ '14-28 DPP:V5',
                          Term == 'treatment3:growth_stageV5' ~ '1-3 DAP:V5',
                          Term == 'treatment4:growth_stageV5' ~ '3-7 DPP:V5',
                          .default = as.character(Term)))
bcw.table <- flextable(bcw.table)
bcw.table <- autofit(bcw.table)
bcw.table <- add_header_lines(bcw.table,
                               values = 'Black Cutworm: Summary')
theme_zebra(bcw.table) %>% 
  save_as_docx(path = 'bcw_summary_table.docx')

anova(bm0, bm1, bm2, bm3)
summary(bm3)
r2_nakagawa(bm3)
bm3_em <- emmeans(bm3, ~treatment+growth_stage)

pwpm(bm3_em)

bcw_em <- as.data.frame(cld(bm3_em, Letters = letters))

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")


ggplot(bcw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Black Cutworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")


ggplot(bcw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75), color = "black")+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Black Cutworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")

# taw models and plot ####
taw_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, taw) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

tm0 <- glmer(taw ~ +
               (1|year/block/plot_id/growth_stage), data = taw_model,
             family = binomial)

tm1 <- glmer(taw ~ treatment +
               (1|year/block/plot_id/growth_stage), data = taw_model,
             family = binomial)

tm2 <- glmer(taw ~ treatment+growth_stage +
               (1|year/block/plot_id/growth_stage), data = taw_model,
             family = binomial)

tm3 <- glmer(taw ~ treatment*growth_stage +
               (1|year/block/plot_id/growth_stage), data = taw_model,
             family = binomial)

taw.table <- as.data.frame(summary(tm3)$coefficients)
#CI <- confint(m3)
taw.table <-cbind(row.names(taw.table), taw.table)
names(taw.table) <- c("Term", "B", "SE", "t", "p")
taw.table <- as_tibble(taw.table) %>% 
  mutate(Term = case_when(Term == 'treatment2' ~ '14-28 DPP',
                          Term == 'treatment4' ~ '3-7 DPP',
                          Term == 'treatment3' ~ '1-3 DAP',
                          Term == 'growth_stageV5' ~ 'V5',
                          Term == 'treatment2:growth_stageV5' ~ '14-28 DPP:V5',
                          Term == 'treatment3:growth_stageV5' ~ '1-3 DAP:V5',
                          Term == 'treatment4:growth_stageV5' ~ '3-7 DPP:V5',
                          .default = as.character(Term)))
taw.table <- flextable(taw.table)
taw.table <- autofit(taw.table)
taw.table <- add_header_lines(taw.table,
                              values = 'True Armyworm: Summary')
theme_zebra(taw.table) %>% 
  save_as_docx(path = 'taw_summary_table.docx')

anova(tm0, tm1, tm2, tm3)
summary(tm3)
r2_nakagawa(tm3)
tm3_em <- emmeans(tm3, ~treatment+growth_stage)

pwpm(tm3_em)

taw_em <- as.data.frame(cld(tm3_em, Letters = letters))

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")


ggplot(taw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: True Armyworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
  # geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")


ggplot(taw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75), color = "black")+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: True Armyworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
  # geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")




# sb models and plot ####
sb_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, sb) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

sbm0 <- glmer(sb ~ +
               (1|year/block/plot_id/growth_stage), data = sb_model,
             family = binomial)

sbm1 <- glmer(sb ~ treatment +
               (1|year/block/plot_id/growth_stage), data = sb_model,
             family = binomial)

sbm2 <- glmer(sb ~ treatment+growth_stage +
               (1|year/block/plot_id/growth_stage), data = sb_model,
             family = binomial)

sbm3 <- glmer(sb ~ treatment*growth_stage +
               (1|year/block/plot_id/growth_stage), data = sb_model,
             family = binomial)

anova(sbm0, sbm1, sbm2, sbm3)
summary(sbm3)
r2_nakagawa(sbm3)
sbm3_em <- emmeans(sbm3, ~treatment+growth_stage)

pwpm(sbm3_em)

sb_em <- as.data.frame(cld(sbm3_em, Letters = letters))

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")


ggplot(sb_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Stink Bug Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
 geom_text(aes(x = treatment, y = -5.4, label = trimws(.group)), size = 10, color = "black")


ggplot(sb_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75), color = "black")+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Stink Bug Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(x = treatment, y = -5.4, label = trimws(.group)), size = 10, color = "black")


# multiple models and plot ####

mult_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, multiple) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

mm0 <- glmer(multiple ~ +
                (1|year/block/plot_id/growth_stage), data = mult_model,
              family = binomial)

mm1 <- glmer(multiple ~ treatment +
                (1|year/block/plot_id/growth_stage), data = mult_model,
              family = binomial)

mm2 <- glmer(multiple ~ treatment+growth_stage +
                (1|year/block/plot_id/growth_stage), data = mult_model,
              family = binomial)

mm3 <- glmer(multiple ~ treatment*growth_stage +
                (1|year/block/plot_id/growth_stage), data = mult_model,
              family = binomial)

mult.table <- as.data.frame(summary(mm3)$coefficients)
#CI <- confint(m3)
mult.table <-cbind(row.names(mult.table), mult.table)
names(mult.table) <- c("Term", "B", "SE", "t", "p")
mult.table <- as_tibble(mult.table) %>% 
  mutate(Term = case_when(Term == 'treatment2' ~ '14-28 DPP',
                          Term == 'treatment4' ~ '3-7 DPP',
                          Term == 'treatment3' ~ '1-3 DAP',
                          Term == 'growth_stageV5' ~ 'V5',
                          Term == 'treatment2:growth_stageV5' ~ '14-28 DPP:V5',
                          Term == 'treatment3:growth_stageV5' ~ '1-3 DAP:V5',
                          Term == 'treatment4:growth_stageV5' ~ '3-7 DPP:V5',
                          .default = as.character(Term)))
mult.table <- flextable(mult.table)
mult.table <- autofit(mult.table)
mult.table <- add_header_lines(mult.table,
                              values = 'Multiple Pest: Summary')
theme_zebra(mult.table) %>% 
  save_as_docx(path = 'mult_summary_table.docx')

anova(mm0, mm1, mm2, mm3)
summary(mm3)
r2_nakagawa(mm3)
mm3_em <- emmeans(mm3, ~treatment+growth_stage)

pwpm(mm3_em)

mm_em <- as.data.frame(cld(mm3_em, Letters = letters))

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")


ggplot(mm_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Multiple Pest Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")

ggplot(mm_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75), color = 'black')+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Multiple Pest Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")

# DO NOT USE: other models and plot ####

# do not use other #

# oth_model <- new_dmg %>% 
#   dplyr::select(year, growth_stage, block, plot_id, treatment, other) %>% 
#   mutate(plot_id = as.factor(plot_id))
# unique(oth_model$treatment)
# 
# om0 <- glmer(other ~ +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# om1 <- glmer(other ~ treatment +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# om2 <- glmer(other ~ treatment+growth_stage +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# om3 <- glmer(other ~ treatment*growth_stage +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# anova(om0, om1, om2, om3)
# summary(om3)
# r2_nakagawa(om3)
# om3_em <- emmeans(om3, ~treatment*growth_stage)
# 
# pwpm(mm3_em)
# 
# om_em <- as.data.frame(cld(om3_em, Letters = letters))
# 
# gs.labs <- c("V3  a", "V5  b")
# names(gs.labs) <- c("V3", "V5")
# 
# 
# ggplot(om_em, aes(color = treatment))+
#   geom_point(aes(x = treatment, y = emmean), size = 10,
#              position = position_dodge(width = .75))+
#   facet_wrap(~growth_stage)+
#   geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
#                 color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
#   scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
#   labs(
#     title = "Corn: Other Pest Damage x Treatment",
#     subtitle = "Years: 2021-2023",
#     x = "Treatment",
#     y = "Damage Emmean",
#     caption = "DPP: Days pre plant
# DAP: Days after plant"
#   )+
#   theme(legend.position = 'none',
#         axis.title = element_text(size = 32),
#         plot.subtitle = element_text(size = 24),
#         plot.title = element_text(size = 28),
#         # axis.line = element_line(size = 1.25),
#         # axis.ticks = element_line(size = 1.25),
#         # axis.ticks.length = unit(.25, "cm"),
#         axis.text.x = element_text(size = 26),
#         axis.text.y = element_text(size = 26),
#         strip.text.x = element_text(size = 26), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
#   #geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")


# old code beloooooowwwww 


# old model code ####




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
# models for damage type  ####
# test model to look at variables before we run the loop
dmg_model <- new_dmg



pest_columns <- c('s','taw', 'bcw','sb', 'multiple', 'other')
summary_list <- list()
r2_list <- list()
emms_mod <- list()
for (pest in 1:length(pest_columns)) {
  print(pest)
  new_col <- pest_columns[pest]
  new_df <- subset(dmg_model, select = c('year', 'growth_stage', 'block', 'plot_id', 'treatment', new_col))
  colnames(new_df) <- c('year', 'growth_stage', 'block','plot_id', 'treatment', 'new_col')
  model <- glmer(new_col ~ treatment*growth_stage +
                   (1|year/block/plot_id/growth_stage), data = new_df,
                 family = binomial)
  emms_mod[[pest]] <- emmeans(model, ~treatment*growth_stage,type = "response")
  summary_model <- summary(model)
  summary_list[[pest]] <- summary_model
  r2_model <- r2_nakagawa(model)
  r2_list[[pest]] <- r2_model
}

summary_list
r2_list
emms_mod
pairs(emms_mod[[1]], simple = "each") # slug
pairs(emms_mod[[2]], simple = "each") # taw
pairs(emms_mod[[3]], simple = "each") # bcw  ***
pairs(emms_mod[[4]], simple = "each") # stink bug
pairs(emms_mod[[5]], simple = "each") # multiple
pairs(emms_mod[[6]], simple = "each") # other

slug_em <- emms_mod[[1]]
slug_em <- as.data.frame(slug_em)
slug_em['pest'] <- ("slug")

taw_em <- emms_mod[[2]]
taw_em <- as.data.frame(taw_em)
taw_em['pest'] <- ("taw")

bcw_em <- emms_mod[[3]]
bcw_em <- as.data.frame(bcw_em)
bcw_em['pest'] <- ("bcw")

sb_em <- emms_mod[[4]]
sb_em <- as.data.frame(sb_em)
sb_em['pest'] <- ("sb")

m_em <- emms_mod[[5]]
m_em <- as.data.frame(m_em)
m_em['pest'] <- ("multiple")

other_em <- emms_mod[[6]]
other_em <- as.data.frame(other_em)
other_em['pest'] <- ("other")


all_emmeans <- rbind(slug_em, taw_em, bcw_em, sb_em, m_em, other_em) 
as_tibble(all_emmeans)
all_emmeans$pest <- as.factor(all_emmeans$pest)

# plot ####

# all years
#facet order
all_emmeans$pest_f <- factor(all_emmeans$pest, levels =c('slug', 'sb', 'bcw', 'taw',
                                               'multiple', 'other')) 

ggplot(all_emmeans, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, linewidth = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = asymp.LCL, ymax = asymp.UCL), 
                alpha = .6, width = 0, linewidth = 1)+
  facet_wrap(~pest_f + growth_stage, scales = "free")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))

# each plot individually then added together 
# template will be slug

slug_em
pairs(emms_mod[[1]])
ggplot(slug_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Slug damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 24),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 20), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

bcw_em
pairs(emms_mod[[3]])
pwpm(emms_mod[[3]])
ggplot(bcw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Black Cutworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

taw_em
pairs(emms_mod[[2]])
ggplot(taw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+
  labs(
    title = "True Armyworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
          axis.text.y = element_text(size = 18),legend.position = "none",
          strip.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(s = 16), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# stink bug not useful
sb_em
pairs(emms_mod[[4]])
ggplot(sb_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Stink bug damage",
    subtitle = "Years: 2022-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


m_em
pairs(emms_mod[[5]])
pwpm(emms_mod[[5]])
ggplot(m_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Multiple damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

other_em
pairs(emms_mod[[6]])
ggplot(other_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Other damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(slug_fig, bcw_fig, taw_fig, sb_fig, mult_fig, other_fig)

# black and white figs for pubs ####
slug_em
pairs(emms_mod[[1]])
ggplot(slug_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Slug damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 24),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 20), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

bcw_em
pairs(emms_mod[[3]])
pwpm(emms_mod[[3]])
ggplot(bcw_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Black Cutworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

taw_em
pairs(emms_mod[[2]])
ggplot(taw_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+
  labs(
    title = "True Armyworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# stink bug not useful
sb_em
pairs(emms_mod[[4]])
ggplot(sb_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Stink bug damage",
    subtitle = "Years: 2022-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


m_em
pairs(emms_mod[[5]])
pwpm(emms_mod[[5]])
ggplot(m_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Multiple damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

other_em
pairs(emms_mod[[6]])
ggplot(other_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Other damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



