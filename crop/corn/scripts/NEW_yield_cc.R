# yield and cc with wallace df 
# time to check and see if our data / outputs match

# packages ####
library(tidyverse)
library(lme4)
library(emmeans)
library(lmtest)
library(multcomp)
library(rempsyc)
library(flextable)
library(performance)
library(ggpubr)

# data ####
wield <- wallace_yield_cb


# yield wrangling ####
colnames(wield)
cield <- wield %>% 
  dplyr::select(Year, YieldBuac, Plot, Block, Crop, CC) %>% 
  mutate(YieldBuac = case_when(YieldBuac == "na" ~ NA, 
                               .default = as.character(YieldBuac))) %>% 
  na.omit() %>% 
  print(n = Inf)
unique(cield$Crop)
unique(cield$Year)
beans <- filter(cield, Crop == "soybean") %>% 
  arrange(Year, Plot, Block) %>% 
  rename(year = Year, 
         yieldbuac = YieldBuac, 
         plot = Plot, 
         block = Block, 
         crop = Crop, 
         cc = CC) %>% 
  mutate(yieldbuac = as.numeric(yieldbuac)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(cc = case_when(cc == "14-21 DPP" ~ "14-28 DPP", 
                        .default = as.factor(cc))) %>% 
  mutate_at(vars(3:6),as.factor) %>%
  print(n = Inf)

beans_avg <- beans %>% 
  group_by(year, cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))
#year for fig 
beans %>% 
  group_by(year) %>% 
  summarise(mean =mean(yieldbuac),
            sd= sd(yieldbuac),
            n = n(),
            se = sd/sqrt(n))
# year   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 2022   48.2  6.57    20  1.47
# 2 2023   56.2 13.8     20  3.08


ggplot(beans_avg, aes(x = cc, y = mean, fill = cc))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~year)

corn <- filter(cield, Crop == "corn") %>% 
  arrange(Year, Plot, Block) %>% 
  rename(year = Year, 
         yieldbuac = YieldBuac, 
         plot = Plot, 
         block = Block, 
         crop = Crop,
         cc = CC) %>% 
  mutate(yieldbuac = as.numeric(yieldbuac)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(cc = case_when(cc == "14-21 DPP" ~ "14-28 DPP", 
                        .default = as.factor(cc))) %>% 
  mutate_at(vars(3:6),as.factor) %>%
  print(n = Inf)

corn_avg <- corn %>% 
  group_by(year, cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

ggplot(corn_avg, aes(x = cc, y = mean, fill = cc))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~year)

corn %>% 
  group_by(cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

# corn stats ####

corn <- corn %>% 
  relocate(yieldbuac)

# explore
c_resp <- names(corn[1])
c_expl <- names(corn[2:6])

c_resp <- set_names(c_resp)
c_expl <- set_names(c_expl)



box_plots <- function(x,y) {
  ggplot(corn, aes(x = .data[[x]], y = .data[[y]]))+
    geom_boxplot()+
    theme_bw()
}

box_plots(x = 'block', y = 'yieldbuac')

clots <- map(c_expl, ~box_plots(.x, 'yieldbuac'))
clots

ggarrange(plotlist = clots)
##

# getting them all on the same grid
ggarrange(plotlist = plots)

c0 <- lmer(yieldbuac ~ (1|block), data = corn)
c1 <- lmer(yieldbuac ~ cc + (1|block), data = corn)
c2 <- lmer(yieldbuac ~ cc + year + (1|block), data = corn)
c3 <- lmer(yieldbuac ~ cc * year + (1|block), data = corn)
summary(c1)
anova(c0,c1,c2,c3)
hist(residuals(c3))
check_model(c3)

cld(emmeans(c3, ~cc|year), Letters = letters)
# year = 2021:
#   cc        emmean   SE df lower.CL upper.CL .group
# No CC        127 7.75 48    111.0      142  a    
# 1-3 DAP      128 7.75 48    112.0      143  a    
# 3-7 DPP      141 7.75 48    125.4      157  a    
# 14-28 DPP    145 7.75 48    129.2      160  a    
# 
# year = 2022:
#   cc        emmean   SE df lower.CL upper.CL .group
# No CC        113 7.75 48     97.2      128  a    
# 1-3 DAP      123 7.75 48    107.6      139  a    
# 3-7 DPP      131 7.75 48    115.6      147  a    
# 14-28 DPP    131 7.75 48    115.8      147  a    
# 
# year = 2023:
#   cc        emmean   SE df lower.CL upper.CL .group
# No CC        111 7.75 48     95.4      127  a    
# 14-28 DPP    142 7.75 48    126.8      158   b   
# 3-7 DPP      145 7.75 48    129.4      161   b   
# 1-3 DAP      169 7.75 48    153.2      184   b   

cld(emmeans(c3, ~cc), Letters= letters)
# cc        emmean   SE   df lower.CL upper.CL .group
# No CC        117 4.48 35.2      108      126  a    
# 3-7 DPP      139 4.48 35.2      130      148   b   
# 14-28 DPP    140 4.48 35.2      130      149   b   
# 1-3 DAP      140 4.48 35.2      131      149   b  



# bean stats ####

beans <- beans %>% 
  relocate(yieldbuac)

# explore
b_resp <- names(beans[1])
b_expl <- names(beans[2:6])

b_resp <- set_names(b_resp)
b_expl <- set_names(b_expl)

box_plots <- function(x,y) {
  ggplot(beans, aes(x = .data[[x]], y = .data[[y]]))+
    geom_boxplot()+
    theme_bw()
}

box_plots(x = 'block', y = 'yieldbuac')

blots <- map(b_expl, ~box_plots(.x, 'yieldbuac'))
blots

ggarrange(plotlist = blots)



b0 <- lmer(yieldbuac ~ (1|block), data = beans)
b1 <- lmer(yieldbuac ~ cc + (1|block), data = beans)
b2 <- lmer(yieldbuac ~ cc + year + (1|block), data = beans)
b3 <- lmer(yieldbuac ~ cc * year + (1|block), data = beans)
summary(b2)
anova(b0,b1,b2,b3)
hist(residuals(b3))

cld(emmeans(b3, ~cc|year), Letters = letters)
# year = 2022:
#   cc        emmean   SE df lower.CL upper.CL .group
# 1-3 DAP     47.4 4.83 32     37.6     57.2  a    
# 3-7 DPP     47.6 4.83 32     37.8     57.4  a    
# No CC       47.6 4.83 32     37.8     57.4  a    
# 14-28 DPP   50.0 4.83 32     40.2     59.8  a    
# 
# year = 2023:
#   cc        emmean   SE df lower.CL upper.CL .group
# No CC       47.6 4.83 32     37.8     57.4  a    
# 3-7 DPP     54.8 4.83 32     45.0     64.6  a    
# 14-28 DPP   58.8 4.83 32     49.0     68.6  a    
# 1-3 DAP     63.6 4.83 32     53.8     73.4  a    

cld(emmeans(b3, ~cc + year), Letters= letters)
# cc        year emmean   SE df lower.CL upper.CL .group
# 1-3 DAP   2022   47.4 4.83 32     37.6     57.2  a    
# 3-7 DPP   2022   47.6 4.83 32     37.8     57.4  a    
# No CC     2022   47.6 4.83 32     37.8     57.4  a    
# No CC     2023   47.6 4.83 32     37.8     57.4  a    
# 14-28 DPP 2022   50.0 4.83 32     40.2     59.8  a    
# 3-7 DPP   2023   54.8 4.83 32     45.0     64.6  a    
# 14-28 DPP 2023   58.8 4.83 32     49.0     68.6  a    
# 1-3 DAP   2023   63.6 4.83 32     53.8     73.4  a 

# plots ####
# corn 
corn <- corn %>% 
  mutate(group = case_when(
    year == '2021' ~ 'a',
    year == '2022' ~'a',
    year == '2023' & cc %in% c("14-28 DPP", "3-7 DPP", "1-3 DAP") ~ 'b',
    year == '2023' & cc == 'No CC' ~ 'a'
  ))


ggplot(corn, aes(x = cc, y = yieldbuac, fill = cc))+
  geom_boxplot(width = 0.5, alpha = 0.7)+
  geom_point(size = 2)+
  facet_wrap(~year)+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  ylab(bquote("Yield"(bu / ac ^-1)))+
  labs(x = 'Treatment termination',
       title = 'Corn: Yield x Treatment',
       subtitle = "Years: 2021-2023"
)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        strip.text = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
geom_text(aes(label = group, y = 196), size = 10)



# 
# corn_plot <- corn %>% 
#   group_by(cc) %>%
#   summarise(mean = mean(yieldbuac),
#             sd = sd(yieldbuac),
#             n = n(), 
#             se = sd/sqrt(n))
# 
# 
# ggplot(corn_plot, aes(x= cc, y = mean, fill = cc))+
#   geom_bar(position = 'dodge' , stat = 'identity', alpha = .7)+
#   scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
#   scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
#   geom_errorbar( aes(x=cc, ymin=mean-se, ymax=mean+se), width=0.4, 
#                  colour="black", alpha=0.9, size=1.3)+
#   ylab(bquote("Mean"(bu / ac ^-1)))+
#   labs(x = 'Treatment',
#        title = 'Corn: Yield x Treatment',
#        subtitle = "Years: 2021-2023",
#        caption = "DPP: Days pre plant
# DAP : Days after plant")+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=26),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 32),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
#   annotate("text", x = 1, y = 130, label = "a", size = 10)+
#   annotate("text", x = 2, y = 155, label = "b", size = 10)+
#   annotate("text", x = 3, y = 155, label = "b", size = 10)+
#   annotate("text", x = 4, y = 155, label = "b", size = 10)



beans <- beans %>% 
  mutate(group = case_when(
    year %in% c('2022', '2023') ~ 'a'
  ))

ggplot(beans, aes(x = cc, y = yieldbuac, fill = cc))+
  geom_boxplot(width = 0.5, alpha = 0.7)+
  geom_point(size = 2)+
  facet_wrap(~year)+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  ylab(bquote("Yield"(bu / ac ^-1)))+
  labs(x = 'Treatment termination',
       title = 'Soy: Yield x Treatment',
       subtitle = "Years: 2021-2023"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        strip.text = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_text(aes(label = group, y = 72), size = 10)





# bean_plot <- beans %>% 
#   group_by(cc) %>%
#   summarise(mean = mean(yieldbuac),
#             sd = sd(yieldbuac),
#             n = n(), 
#             se = sd/sqrt(n))
# 
# ggplot(bean_plot, aes(x= cc, y = mean, fill = cc))+
#   geom_bar(position = 'dodge' , stat = 'identity', alpha = .7)+
#   scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"),
#                    labels = c('No CC', 'Early', 'Late', 'Green'))+
#   scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
#   geom_errorbar( aes(x=cc, ymin=mean-se, ymax=mean+se), width=0.4, 
#                  colour="black", alpha=0.9, size=1.3)+
#   ylab(bquote("Mean"(bu / ac ^-1)))+
#   labs(x = 'Treatment',
#        title = 'Soybean: Yield x Treatment',
#        subtitle = "Years: 2022-2023",
#        caption = "DPP: Days pre plant
# DAP : Days after plant")+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=32),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 32),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

ggplot(beans, aes(x = cc, y = yieldbuac, fill = cc))+
  geom_boxplot(width = 0.5, alpha = 0.7)+
  geom_point(size = 2)+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  ylab(bquote("Yield"(bu / ac ^-1)))+
  labs(x = 'Treatment termination',
       title = 'Soybean: Yield x Treatment',
       subtitle = "Years: 2022-2023"
#        caption = "DPP: Days pre plant
# DAP : Days after plant"
       )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))




# did not use anything below as of 6.12.2024
bean_year <- beans %>% 
  group_by(year) %>%
  summarise(mean = mean(yieldbuac),
            sd = sd(yieldbuac),
            n = n(), 
            se = sd/sqrt(n))

ggplot(beans, aes(x = year, y = yieldbuac, fill = year))+
  geom_boxplot(width = 0.5, alpha = 0.7)+
  geom_point(size = 2)+
  scale_fill_manual(values = c("#7570B3","#E7298A"))+
  ylab(bquote("Yield"(bu / ac ^-1)))+
  labs(x = 'Year',
       title = 'Soybean: Yield x year',
       subtitle = "Years: 2022-2023"
       #        caption = "DPP: Days pre plant
       # DAP : Days after plant"
  )+
  theme(legend.position = "none",
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 1, y = 72, label = 'a', size = 10)+
  annotate('text', x = 2, y = 72, label = 'b', size = 10)


# corn - soy: yield
# 21 -22
corn_21 <- filter(corn, year == "2021") %>% 
  mutate(crop = 'corn')
bean_22 <- filter(beans, year == "2022") %>% 
  mutate(crop = 'beans')

y2122 <- rbind(corn_21, bean_22) %>% 
  mutate(crop = as.factor(crop)) %>% 
  mutate(crop = case_when(crop == 'corn' ~ 'Corn', 
                          crop == 'beans' ~ 'Soybean'))
# results from above models 
# 2021 corn
# cc        emmean   SE   df lower.CL upper.CL .group
# No CC        127 5.33 14.2      115      138  a    
# 1-3 DAP      128 5.33 14.2      116      139  a    
# 3-7 DPP      141 5.33 14.2      130      152  a    
# 14-28 DPP    145 5.33 14.2      133      156  a 

# 2022 beans 
# cc        emmean   SE df lower.CL upper.CL .group
# 1-3 DAP     47.4 3.16 16     40.7     54.1  a    
# 3-7 DPP     47.6 3.16 16     40.9     54.3  a    
# No CC       47.6 3.16 16     40.9     54.3  a    
# 14-28 DPP   50.0 3.16 16     43.3     56.7  a  
ggplot(y2122, aes(x = cc, y = yieldbuac, fill = cc))+
  geom_boxplot(width = 0.5, alpha = 0.7)+
  geom_point(size = 2)+
  facet_wrap(~crop, scale = 'free')+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  ylab(bquote("Yield"(bu / ac ^-1)))+
  labs(x = 'Treatment',
       title = 'Yield x Treatment',
       subtitle = "Years: 2021 Corn - 2022 Soybeans",
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        strip.text = element_text(size = 26))


# 22-23
unique(y2223$crop)
corn_22 <- filter(corn, year == "2022")
bean_23 <- filter(beans, year == "2023")
y2223 <- rbind(corn_22, bean_23) %>% 
  mutate(crop = as.factor(crop)) %>% 
  mutate(crop = case_when(crop == 'corn' ~ 'Corn', 
                          crop == 'soybean' ~ 'Soybean'))
# results from above 
# 2022 corn
# cc        emmean   SE   df lower.CL upper.CL .group
# No CC        113 6.49 15.2       99      127  a    
# 1-3 DAP      123 6.49 15.2      109      137  a    
# 3-7 DPP      131 6.49 15.2      117      145  a    
# 14-28 DPP    131 6.49 15.2      118      145  a 

# 2023 beans
# cc        emmean   SE df lower.CL upper.CL .group
# No CC       47.6 6.05 16     34.8     60.4  a    
# 3-7 DPP     54.8 6.05 16     42.0     67.6  a    
# 14-28 DPP   58.8 6.05 16     46.0     71.6  a    
# 1-3 DAP     63.6 6.05 16     50.8     76.4  a 

ggplot(y2223, aes(x = cc, y = yieldbuac, fill = cc))+
  geom_boxplot(width = 0.5, alpha = 0.7)+
  geom_point(size = 2)+
  facet_wrap(~crop, scale = 'free')+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A"))+
  ylab(bquote("Yield"(bu / ac ^-1)))+
  labs(x = 'Treatment',
       title = 'Yield x Treatment',
       subtitle = "Years: 2022 Corn - 2023 Soybeans",
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        strip.text = element_text(size = 26))



# New CC data and stats 6.12.2024 ####
colnames(wield)
wield %>% 
  dplyr::select(Year, Crop, CC, Block, Plot, Rye.kgha) %>% 
  arrange(Year, Crop) %>% 
  distinct(Year, Crop, CC, Block, Plot, Rye.kgha) %>% 
  mutate(Mg_ha = Rye.kgha*0.001)%>% 
  print(n = Inf) 

cc_new <- wield %>% 
  dplyr::select(Year, Crop, CC, Block, Plot, Rye.kgha) %>% 
  arrange(Year, Crop) %>% 
  distinct(Year, Crop, CC, Block, Plot, Rye.kgha) %>% 
  mutate(Mg_ha = Rye.kgha*0.001) %>% 
  mutate(CC = case_when(
    CC == '1-3 DAP' ~ 'green', 
    CC == '14-21 DPP' ~ 'early', 
    CC == '3-7 DPP' ~ 'late', 
    CC == 'No CC' ~ 'check'
  )) %>% 
  filter(CC != 'check') %>% 
  mutate_at(vars(1:5), as.factor) %>% 
  print(n = Inf)


ggplot(cc_new, aes(CC, Mg_ha))+
  geom_boxplot(position = 'dodge')+
  facet_wrap(~Crop)+
  scale_x_discrete(limits = c('early', 'late', 'green'))

ggplot(filter(cc_new, Crop == 'corn'), aes(CC, Mg_ha))+
  geom_boxplot(position = 'dodge')+
  facet_wrap(~Year)+
  scale_x_discrete(limits = c('early', 'late', 'green'))

ggplot(filter(cc_new, Crop == 'soybean'), aes(CC, Mg_ha))+
  geom_boxplot(position = 'dodge')+
  facet_wrap(~Year)+
  scale_x_discrete(limits = c('early', 'late', 'green'))


cc_figure_df <- cc_new %>%
  group_by(CC, Crop, Year) %>% 
  summarise(mean = mean(Mg_ha),
            sd = sd(Mg_ha), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  arrange(Crop, CC, Year)


# corn 
corn_cc <- cc_new %>% 
  filter(Crop == 'corn')

# exploratory plots
corn_cc
# isolate the explanatory and response variables
resp <- names(corn_cc[7])
expl <- names(corn_cc[1:5])

resp <- set_names(resp)
resp
expl <- set_names(expl)
expl

# option 1
# create a plotting function for categorical expl and continuous response
# in this instance, hard coding the data set into the fxn bc I am only working with one df
# strings can not be directly added to aes, so the use of the .data pronoun is needed
scatter <- function(x,y) {
  ggplot(corn_cc, aes(x = .data[[x]], y = .data[[y]]))+
    geom_boxplot()+
    theme_bw()
}

scatter(x = 'CC', y = 'Mg_ha')

plots <- map(expl, ~scatter(.x, 'Mg_ha'))
plots

# getting them all on the same grid
ggarrange(plotlist = plots)

#option 2
#another option is to create all combinations, and then plot them
resp_expl <- tidyr::expand_grid(resp, expl)
resp_expl
# pmap now to loop through the rows of this tibble
plots2 <- pmap(resp_expl, ~scatter(x = .y, y = .x))
plots2





m0 <- lmer(Mg_ha ~ (1|Block), data = corn_cc)
m1 <- lmer(Mg_ha ~ CC +(1|Block), data = corn_cc)
m2 <- lmer(Mg_ha ~ CC + Year + (1|Block), data = corn_cc)
m3 <- lmer(Mg_ha ~ CC * Year +(1|Block/Plot), data = corn_cc)
anova(m0, m1, m2, m3)
hist(residuals(m3))
check_model(m3)
summary(m3)





cld(emmeans(m3, ~CC|Year), Letters = letters)
# Year = 2021:
#   CC    emmean    SE df lower.CL upper.CL .group
# early  1.817 0.161 36    1.491    2.143  a    
# late   3.031 0.161 36    2.705    3.357   b   
# green  5.293 0.161 36    4.967    5.619    c  
# 
# Year = 2022:
#   CC    emmean    SE df lower.CL upper.CL .group
# early  1.166 0.161 36    0.839    1.492  a    
# late   2.757 0.161 36    2.431    3.083   b   
# green  5.491 0.161 36    5.165    5.817    c  
# 
# Year = 2023:
#   CC    emmean    SE df lower.CL upper.CL .group
# early  0.639 0.161 36    0.312    0.965  a    
# late   1.745 0.161 36    1.419    2.072   b   
# green  2.162 0.161 36    1.836    2.489   b   


cld(emmeans(m3, ~Year), Letters = letters)
# Year emmean     SE df lower.CL upper.CL .group
# 2023   1.52 0.0929 24     1.32     1.71  a    
# 2022   3.14 0.0929 24     2.95     3.33   b   
# 2021   3.38 0.0929 24     3.19     3.57   b   


# soybean
soy_cc <- cc_new %>% 
  filter(Crop == 'soybean')

s0 <- lmer(Mg_ha ~ (1|Block), data = soy_cc)
s1 <- lmer(Mg_ha ~ CC +(1|Block), data = soy_cc)
s2 <- lmer(Mg_ha ~ CC + Year + (1|Block), data = soy_cc)
s3 <- lmer(Mg_ha ~ CC * Year +(1|Block), data = soy_cc)
anova(s0, s1, s2, s3)
hist(residuals(s3))

cld(emmeans(s3, ~CC|Year), Letters = letters)
# Year = 2022:
#   CC    emmean    SE   df lower.CL upper.CL .group
# early  0.469 0.145 26.6    0.171    0.767  a    
# late   1.704 0.145 26.6    1.406    2.002   b   
# green  2.863 0.145 26.6    2.565    3.161    c  
# 
# Year = 2023:
#   CC    emmean    SE   df lower.CL upper.CL .group
# early  0.872 0.199 37.5    0.469    1.276  a    
# green  1.750 0.199 37.5    1.347    2.154   b   
# late   1.759 0.199 37.5    1.355    2.162   b 

cld(emmeans(s3, ~Year), Letters = letters)
# Year emmean     SE    df lower.CL upper.CL .group
# 2023   1.46 0.1217 17.42     1.20     1.72  a    
# 2022   1.68 0.0927  6.87     1.46     1.90  a


# corn cover crop plots ####

cld_cc_figure_df <- cc_figure_df %>% 
  mutate(group = case_when(
    Crop == 'corn' & CC == 'early' ~ 'a',
    Crop == 'corn' & CC == 'late' ~ 'b',
    Crop == 'corn' & CC == 'green' & Year == '2023' ~ 'b',
    Crop == 'corn' & CC == 'green' & Year %in% c('2021', '2022') ~ 'c',
    Crop == 'soybean' & CC == 'early' ~ 'a',
    Crop == 'soybean' & CC == 'late' ~ 'b',
    Crop == 'soybean' & CC == 'green' & Year == '2022' ~ 'c',
    Crop == 'soybean' & CC == 'green' & Year == '2023' ~ 'b'
  )) %>% 
  arrange(Crop, Year, CC)


ggplot(filter(cld_cc_figure_df, Crop == 'corn'), aes(x = CC, y = mean, fill = CC))+  
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.75)+
  facet_wrap(~Year)+
  scale_x_discrete(limits = c('early', 'late', 'green'),
                     labels = c("Early", "Late", "Green"))+
  scale_fill_manual(values = c("#D95F02", "#1B9E77","#7570B3"))+
  geom_errorbar( aes(x=CC, ymin=mean-se, ymax=mean+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Average cover crop biomass by treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment termination",
       y = "Mean cover-crop biomass (Mg/ha)")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        strip.text = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_discrete(limits = c(0,1,2,3,4,5,6))+
  geom_text(aes(label = group, y = 6.2), size = 10)

#over all bar
# ggplot(filter(cc_mg_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
#   scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
#   scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
#   geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
#   geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
#                  colour="black", alpha=0.9, size=1.3)+
#   labs(title = "Corn: Mean Cover Crop Biomass x Treatment",
#        subtitle = "Years: 2021-2023",
#        x = "Treatment termination",
#        caption = "DPP: Days pre plant
# DAP: Days after plant")+
#   ylab(bquote("Mean cover crop" (Mg / ha ^-1)))+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=26),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 28),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
#   annotate("text", x = 1, y = 2.8, label = "a", size = 10)+
#   annotate("text", x = 2, y = 4.9, label = "b", size = 10)+
#   annotate("text", x = 3, y = 7.9, label = "c", size = 10)



###

# bean cover crop plots ####

ggplot(filter(cld_cc_figure_df, Crop == 'soybean'), aes(x = CC, y = mean, fill = CC))+  
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.75)+
  facet_wrap(~Year)+
  scale_x_discrete(limits = c('early', 'late', 'green'),
                   labels = c("Early", "Late", "Green"))+
  scale_fill_manual(values = c("#D95F02", "#1B9E77","#7570B3"))+
  geom_errorbar( aes(x=CC, ymin=mean-se, ymax=mean+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Soy: Average cover crop biomass by treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment termination",
       y = "Mean cover-crop biomass (Mg/ha)")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        strip.text = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_discrete(limits = c(0,1,2,3))+
  geom_text(aes(label = group, y = 3.2), size = 10)

# # bar by year 
# ggplot(filter(bcc_year_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
#   scale_x_discrete(limits = c('br', 'grbr', 'gr'),
#     labels = c("14-28 DPP", "3-7 DPP", "1-3 DAP"))+
#   scale_fill_manual(values = c("#D95F02","#1B9E77", "#7570B3"))+
#   geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
#   geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
#                  colour="black", alpha=0.9, size=1.3)+
#   facet_wrap(~year)+
#   labs(title = "Soybean: Cover Crop Biomass x Treatment",
#        # subtitle = "Years: 2022-2023",
#        x = "Treatment",
#        caption = "DPP: Days pre plant
# DAP: Days after plant")+
#   ylab(bquote("Mean" (Mg / ha ^-1)))+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=26),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 28),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.text = element_text(size = 26),
#         plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))
